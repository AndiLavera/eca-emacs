;;; eca-process.el --- ECA (Editor Code Assistant) process -*- lexical-binding: t; -*-
;; Copyright (C) 2025 Eric Dallo
;;
;; SPDX-License-Identifier: Apache-2.0
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  The ECA (Editor Code Assistant) process.
;;
;;; Code:

(require 's)
(require 'f)

(require 'eca-util)
(require 'eca-api)

(defcustom eca-custom-command nil
  "The eca server command.
If not provided, download and start eca automatically."
  :group 'eca
  :risky t
  :type '(repeat string))

(defcustom eca-extra-args '()
  "The extra args to pass to eca server command."
  :group 'eca
  :risky t
  :type '(repeat string))

(defcustom eca-process-wrapper-function nil
  "Optional function to wrap the eca server command before launch.
When non-nil, called with two arguments: COMMAND (list of strings,
already including `eca-extra-args') and ROOTS (list of absolute
workspace folder paths).  Must return a list of strings to be
passed to `make-process'.

Useful for running ECA inside a sandbox like firejail, bubblewrap,
or jai, optionally whitelisting workspace roots at startup.  See
the README's \"Sandboxing\" section for examples."
  :group 'eca
  :risky t
  :type '(choice (const nil) function))

(defcustom eca-server-download-method 'url-retrieve
  "The method to use to download eca server binary.
Some Emacs versions / distributions have issues with
curl blocking Emacs, but `url-retrieve' should be always async.
Test different options if facing issues."
  :group 'eca
  :type '(choice
          (const :tag "Async url-retrieve" url-retrieve)
          (const :tag "Curl" curl)))

(defcustom eca-server-download-url nil
  "The custom URL to download eca server."
  :group 'eca
  :type 'string)

(defcustom eca-server-install-path
  (f-join (expand-file-name
           (locate-user-emacs-file "eca"))
          (if (eq system-type 'windows-nt)
              "eca.exe"
            "eca"))
  "Path to the eca server binary."
  :risky t
  :type 'file
  :group 'eca)

(defcustom eca-server-version-file-path
  (f-join (expand-file-name
           (locate-user-emacs-file "eca"))
          "eca-version")
  "File in which eca version will be."
  :risky t
  :type 'string
  :group 'eca)

(defconst eca-ext-pwsh-script "pwsh -noprofile -noninteractive \
-nologo -ex bypass -c Expand-Archive -Path '%s' -DestinationPath '%s'"
  "Pwsh script to unzip file.")

(defconst eca-ext-powershell-script "powershell -noprofile -noninteractive \
-nologo -ex bypass -command Expand-Archive -path '%s' -dest '%s'"
  "Powershell script to unzip file.")

(defconst eca-ext-unzip-script "bash -c 'mkdir -p %2$s && unzip -qq -o %1$s -d %2$s'"
  "Unzip script to unzip file.")

(defconst eca-ext-ark-script "bash -c 'mkdir -p %2$s && ark -b -o %2$s %1$s'"
  "Ark script to unzip file.")

(defcustom eca-unzip-script (lambda ()
                              (cond ((and (eq system-type 'windows-nt)
                                          (executable-find "pwsh"))
                                     eca-ext-pwsh-script)
                                    ((and (eq system-type 'windows-nt)
                                          (executable-find "powershell"))
                                     eca-ext-powershell-script)
                                    ((executable-find "unzip") eca-ext-unzip-script)
                                    ((executable-find "pwsh") eca-ext-pwsh-script)
                                    ((executable-find "ark") eca-ext-ark-script)
                                    (t nil)))
  "The script to unzip downloaded eca server."
  :group 'eca
  :type 'string)

(defcustom eca-min-gc-cons-threshold (* 100 1024 1024)
  "Temporarily increase GC threshold during heavy message processing.
If current `gc-cons-threshold` is lower use that on filter server messages.'"
  :type 'integer
  :group 'eca)

(defun eca-process--buffer-name (session)
  "Return the process buffer name for SESSION."
  (format "<eca[%s]:%s>"
          (eca--session-project-name session)
          (eca--session-id session)))

(defun eca-process--stderr-buffer-name (session)
  "Return the stderr buffer name for SESSION."
  (format "<eca:stderr[%s]:%s>"
          (eca--session-project-name session)
          (eca--session-id session)))

(defcustom eca-server-releases-cache-ttl 3600
  "Time-to-live (seconds) for the cached eca server releases list.
Once expired, the next call that needs the releases list will refetch
from the GitHub API.  Set to nil to disable expiry (legacy behavior:
cache lives until Emacs is restarted).  Set to 0 to always refetch.
This affects update detection at session start and on `eca-restart',
so longer-running Emacs sessions can still pick up newer eca releases.
See also `eca-server-check-updates'."
  :type '(choice (const :tag "Never expire" nil)
          (integer :tag "Seconds"))
  :group 'eca)

(defvar eca-process--releases-cache nil
  "Cached parsed releases list from GitHub API.
When non-nil, a cons cell of (FETCH-TIME . RELEASES) where FETCH-TIME
is the value of `float-time' when RELEASES was last fetched.  Honored
together with `eca-server-releases-cache-ttl' by
`eca-process--fetch-releases'.")

(cl-defun eca--curl-download-file (&key url path on-done)
  "Downloads a file from URL to PATH shelling out to system with curl.
Calls ON-DONE when done."
  (let* ((remote (file-remote-p default-directory))
         (curl-cmd (or (executable-find "curl" remote)
                       (executable-find "curl.exe" remote)))
         (expanded (expand-file-name
                    (if (and remote (not (file-remote-p path)))
                        (concat remote path)
                      path)))
         (local-path (file-local-name expanded))
         (buf (generate-new-buffer " *eca-curl*")))
    (unless curl-cmd
      (error "Curl not found. Please install curl or customize eca-custom-command"))
    (make-process
     :name "eca-curl"
     :buffer buf
     :command (list curl-cmd "-L" "-s" "-S" "-f" "-o" local-path url)
     :file-handler t
     :noquery t
     :sentinel (lambda (proc _event)
                 (unless (process-live-p proc)
                   (let ((code (process-exit-status proc)))
                     (if (zerop code)
                         (progn
                           (kill-buffer buf)
                           (funcall on-done))
                       ;; Extract error output
                       (let ((output (with-current-buffer buf
                                       (string-trim (buffer-string)))))
                         (kill-buffer buf)
                         (error "Curl failed (exit %d) downloading %s: %s"
                                code url output)))))))))

(cl-defun eca--url-retrieve-download-file (&key url path on-done)
  "Downloads async a file from URL to PATH via `url-retrieve'.
Calls ON-DONE when done
Workaround for `url-copy-file` that has issues with macos async threads.
https://github.com/emacs-lsp/lsp-mode/issues/4746#issuecomment-2957183423

Not supported for remote installs; signals a `user-error' when
`default-directory' is a TRAMP path.  Use `curl' instead."
  (when (file-remote-p default-directory)
    (user-error "`url-retrieve' download method is not supported for remote installs; set `eca-server-download-method' to `curl'"))
  (url-retrieve
   url
   (lambda (status)
     (let ((resp-buf (current-buffer)))
       (unwind-protect
           (progn
             (when-let ((error-data (plist-get status :error)))
               (error "%s" error-data))
             (let ((coding-system-for-write 'binary)
                   (buffer-file-coding-system 'binary))
               (goto-char (point-min))
               (unless (re-search-forward "\r?\n\r?\n" nil t)
                 (error "Failed to parse HTTP response for download"))
               (write-region (point) (point-max) path nil 'silent)
               (funcall on-done)))
         (ignore-errors (kill-buffer resp-buf)))))
   nil
   t))

(defun eca--curl-download-string (url)
  "Download content from URL as a string, shelling out to curl."
  (let ((curl-cmd (or (executable-find "curl" (file-remote-p default-directory))
                      (executable-find "curl.exe" (file-remote-p default-directory)))))
    (unless curl-cmd
      (error "Curl not found. Please install curl or customize eca-custom-command"))
    (with-temp-buffer
      (let ((exit-code (process-file curl-cmd nil t nil
                                     "-L" "-s" "-S" "-f" url)))
        (unless (= exit-code 0)
          (error "Curl failed to download from %s (exit %d)" url exit-code))
        (let ((output (buffer-string)))
          (when (s-blank? output)
            (error "Curl failed to download from %s" url))
          output)))))

(defconst eca-process--releases-url "https://api.github.com/repos/editor-code-assistant/eca/releases"
  "Github url for retrieving json files with infos about release binaries.")

(defun eca-process--releases-cache-valid-p ()
  "Return non-nil if `eca-process--releases-cache' is fresh enough.
Honors `eca-server-releases-cache-ttl': when nil any cached entry is
considered valid; when 0 the cache is always considered stale."
  (when (consp eca-process--releases-cache)
    (let ((ttl eca-server-releases-cache-ttl)
          (fetched-at (car eca-process--releases-cache)))
      (cond
       ((null ttl) t)
       ((and (numberp ttl) (<= ttl 0)) nil)
       ((numberp fetched-at)
        (< (- (float-time) fetched-at) ttl))
       (t nil)))))

(defun eca-process--fetch-releases ()
  "Return cached releases list, fetching from GitHub if needed.
Refetches when the cache is empty or has expired per
`eca-server-releases-cache-ttl'.  On fetch failure, any previously
cached value is preserved and returned."
  (if (eca-process--releases-cache-valid-p)
      (cdr eca-process--releases-cache)
    (condition-case err
        (let* ((json-string
                (eca--curl-download-string
                 eca-process--releases-url))
               (releases (with-temp-buffer
                           (insert json-string)
                           (goto-char (point-min))
                           (eca-api--json-read-buffer))))
          (setq eca-process--releases-cache
                (cons (float-time) releases))
          releases)
      (error
       (eca-warn "Failed to fetch releases: %s" err)
       (cdr-safe eca-process--releases-cache)))))

(defun eca-process--get-latest-server-version ()
  "Return the latest server version."
  (when-let ((releases (eca-process--fetch-releases)))
    (plist-get (elt releases 0) :tag_name)))

(defun eca-process--get-property (property &optional version)
  "Retrieve PROPERTY for server binary VERSION.
When VERSION is nil, returns PROPERTY from the latest release."
  (when-let ((releases (eca-process--fetch-releases)))
    (let ((props (if version
                     (seq-find (lambda (ver)
                                 (string-equal
                                  (plist-get ver :tag_name)
                                  version))
                               releases)
                   (elt releases 0))))
      (plist-get props property))))

(defun eca-process--get-current-server-version ()
  "Return the current version of installed server if available."
  (let ((vf (plist-get (eca-process--server-paths) :version-file)))
    (when (f-exists? vf)
      (f-read vf))))

(defun eca-process--find-extracted-binary (temp-dir name)
  "Find extracted binary NAME in TEMP-DIR.
On Windows, handle .exe extension mismatch:
try alternate name with or without .exe."
  (let ((primary (f-join temp-dir name)))
    (if (f-exists? primary)
        primary
      (when (eq system-type 'windows-nt)
        (let ((alt (if (string-suffix-p ".exe" name)
                       (f-join temp-dir
                               (file-name-sans-extension name))
                     (f-join temp-dir
                             (concat name ".exe")))))
          (when (f-exists? alt) alt))))))

(defun eca-process--server-paths ()
  "Return a plist of all install/download paths, TRAMP-aware.
Keys: :store, :download, :old, :temp-extract. When
`default-directory' is remote, every path carries the TRAMP prefix and
`~' in `eca-server-install-path' is resolved against the remote HOME."
  (let* ((remote (file-remote-p default-directory))
         (raw eca-server-install-path)
         (store (expand-file-name (if remote (concat remote raw) raw))))
    (list :store store
          :download (concat store ".zip")
          :old (concat store ".old")
          :version-file (concat (file-name-directory store) "eca-version")
          ;; Use `file-name-directory' (no IO) instead of `f-parent', which
          ;; calls `file-truename' and would force a TRAMP roundtrip to the
          ;; remote just to compute a parent directory.
          :temp-extract (concat (directory-file-name
                                 (file-name-directory store))
                                "-temp"))))

(defun eca-process--cleanup-old-server ()
  "Try to delete any leftover .old server binary from previous update.
On Windows, running executables can be renamed but not deleted, so we
clean them up on next startup."
  (let ((old-path (plist-get (eca-process--server-paths) :old)))
    (when (f-exists? old-path)
      (condition-case nil
          (progn
            (f-delete old-path)
            (eca-info "Cleaned up old server binary"))
        (error nil)))))

(defun eca-process--uninstall-server ()
  "Remove downloaded server."
  (let* ((paths (eca-process--server-paths))
         (download-path (plist-get paths :download))
         (store-path (plist-get paths :store)))
    (when (f-exists? download-path) (f-delete download-path))
    (when (f-exists? store-path) (f-delete store-path))))

(defun eca-process--remote-uname ()
  "Return (SYSTEM . ARCH) for current `default-directory'.
Runs `uname' on the remote when remote, else returns local values."
  (if (file-remote-p default-directory)
      (let ((s (with-temp-buffer
                 (when (zerop (process-file "uname" nil t nil "-s"))
                   (string-trim (buffer-string)))))
            (m (with-temp-buffer
                 (when (zerop (process-file "uname" nil t nil "-m"))
                   (string-trim (buffer-string))))))
        (cons (pcase s
                ("Linux" 'gnu/linux)
                ("Darwin" 'darwin)
                (_ (intern (downcase (or s "")))))
              (or m "x86_64")))
    (cons system-type (car (split-string system-configuration "-")))))

(defun eca-process--download-url (version)
  "Return the server download url for VERSION."
  (or eca-server-download-url
      (-let (((sys . arch) (eca-process--remote-uname)))
        (format "https://github.com/editor-code-assistant/eca/releases/download/%s/eca-native-%s.zip"
                version
                (pcase sys
                  ('gnu/linux (cond
                               ((string= "x86_64" arch) "static-linux-amd64")
                               (t (concat "linux-" arch))))
                  ('darwin (concat "macos-"
                                   (cond
                                    ((string= "x86_64" arch) "amd64")
                                    ((string= "arm64" arch) "aarch64")
                                    (t arch))))
                  ('windows-nt "windows-amd64"))))))

(defun eca-process--get-file-sha256 (file)
  "Compute and return the SHA256 hash of FILE.
When FILE is remote, run sha256sum/shasum on the remote so the file's
bytes are not transferred back across TRAMP just to be hashed."
  (if-let ((remote (file-remote-p file)))
      (with-temp-buffer
        (let* ((local (file-local-name file))
               (exit-code (cond
                           ((executable-find "sha256sum" remote)
                            (process-file "sha256sum" nil t nil local))
                           ((executable-find "shasum" remote)
                            (process-file "shasum" nil t nil "-a" "256" local))
                           (t
                            (error "No sha256sum/shasum on remote host")))))
          (unless (zerop exit-code)
            (error "Remote hash computation failed for %s" file))
          (car (split-string (buffer-string)))))
    ;; local
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally file)
      (secure-hash 'sha256 (current-buffer)))))

(defun eca-process--unzip-archive (archive dest)
  "Extract ARCHIVE into DEST."
  (let* ((remote (file-remote-p default-directory))
         (script (if remote
                     eca-ext-unzip-script
                   (when eca-unzip-script (funcall eca-unzip-script)))))
    (unless script
      (error "Unable to find `unzip' or `powershell' on the path, please customize `eca-unzip-script'"))
    
    (unless remote
      (mkdir (file-local-name dest) t))

    (let ((cmd (format script (file-local-name archive) (file-local-name dest))))
      (with-temp-buffer
        (let ((exit (process-file-shell-command cmd nil t)))
          (unless (zerop exit)
            (error "Unzip failed (exit %d) for %s: %s"
                   exit archive (string-trim (buffer-string)))))))))

(defun eca-process--check-sha256 (download-path url version)
  "Check sha256 checksum of archive at DOWNLOAD-PATH.
The archive should be retrieved from URL and have
the given VERSION."
  (if-let* ((asset (seq-find
                    (lambda (asset)
                      (let ((asset-url (plist-get asset :browser_download_url)))
                        (and (stringp asset-url)
                             (string-equal url asset-url))))
                    (eca-process--get-property :assets version)))
            (digest (plist-get asset :digest))
            (sha256 (and (stringp digest)
                         (string-match "sha256:" digest)
                         (substring digest (match-end 0)))))
      (unless (string-equal
               sha256
               (eca-process--get-file-sha256 download-path))
        (error "The downloaded archive for the eca binary is corrupted"))
    (eca-warn "Cannot retrieve sha256 for the eca binary archive, skipping checksum verification")))

(defun eca-process--download-server (on-downloaded version)
  "Download eca server of VERSION calling ON-DOWNLOADED when success."
  (-let* ((paths (eca-process--server-paths))
          (download-path (plist-get paths :download))
          (store-path (plist-get paths :store))
          (old-path (plist-get paths :old))
          (version-file (plist-get paths :version-file))
          (temp-extract-dir (plist-get paths :temp-extract))
          (url (eca-process--download-url version))
          (download-fn (pcase eca-server-download-method
                         ('url-retrieve #'eca--url-retrieve-download-file)
                         ('curl #'eca--curl-download-file)
                         (_ (error (eca-error (format "Unknown download method '%s' for eca-server-download-method" eca-server-download-method)))))))
    (condition-case err
        (progn
          ;; Clean up any old files from previous updates
          (eca-process--cleanup-old-server)
          (when (f-exists? download-path) (f-delete download-path))
          (when (f-exists? version-file) (f-delete version-file))
          (when (f-exists? temp-extract-dir) (f-delete temp-extract-dir t))
          (mkdir (file-name-directory download-path) t)
          (eca-info "Downloading eca server from %s to %s..." url download-path)
          (funcall
           download-fn
           :url url
           :path download-path
           :on-done (lambda ()
                      (eca-info "Downloaded eca. Checking sha256...")
                      (eca-process--check-sha256 download-path url version)
                      (eca-info "Unzipping eca...")
                      (eca-process--unzip-archive download-path temp-extract-dir)
                      (let ((new-binary (eca-process--find-extracted-binary
                                         temp-extract-dir (f-filename store-path))))
                        (unless new-binary
                          (error "Expected binary not found after extraction: %s"
                                 (f-join temp-extract-dir (f-filename store-path))))
                        ;; Rename old binary to .old if it exists
                        ;; On Windows, running executables can be renamed but not deleted
                        (when (f-exists? store-path)
                          (when (f-exists? old-path)
                            (condition-case nil (f-delete old-path) (error nil)))
                          (rename-file store-path old-path))
                        ;; Move new binary into place
                        (rename-file new-binary store-path))
                      ;; Clean up temp directory
                      (when (f-exists? temp-extract-dir)
                        (condition-case nil (f-delete temp-extract-dir t) (error nil)))
                      ;; Try to delete old binary (may fail if still in use, that's ok)
                      (when (f-exists? old-path)
                        (condition-case nil (f-delete old-path) (error nil)))
                      (f-write-text version 'utf-8 version-file)
                      (set-file-modes store-path #o0700)
                      (eca-info "Installed eca successfully!")
                      (funcall on-downloaded))))
      (error (eca-error "Failed to download eca server %s" err)))))

(defun eca-process--program-path (path)
  "Return PATH as a program path suitable for `make-process'.
Resolves against `default-directory', so when it is a TRAMP path
PATH is interpreted on the remote host (including `~' and relative
segments).  The returned string never carries a TRAMP prefix."
  (let* ((remote (file-remote-p default-directory))
         (qualified (if (and remote (not (file-remote-p path)))
                        (concat remote path)
                      path)))
    (file-local-name (expand-file-name qualified))))

(defun eca-process--server-command ()
  "Return the command to start server."
  (let* ((paths (eca-process--server-paths))
         (store (plist-get paths :store))
         (system-command (executable-find "eca" (file-remote-p default-directory))))
    (cond
     (eca-custom-command
      (list :decision 'custom
            :command (cons (eca-process--program-path (car eca-custom-command))
                           (cdr eca-custom-command))))

     (system-command
      (list :decision 'system
            :command (list (eca-process--program-path system-command) "server")))

     ((and (not (f-exists? store))
           (not (eca-process--get-latest-server-version)))
      (list :decision 'error-download
            :message "Could not fetch latest version of eca. Please check your internet connection and try again. You can also download eca manually and set the path via eca-custom-command variable"))

     ((and (f-exists? store)
           (not (string-version-lessp (eca-process--get-current-server-version)
                                      (eca-process--get-latest-server-version))))
      (list :decision 'already-installed
            :command (list (eca-process--program-path store) "server")))

     (t (list :decision 'download
              :latest-version (eca-process--get-latest-server-version)
              :command (list (eca-process--program-path store) "server"))))))

(defun eca-process--parse-header (s)
  "Parse string S as a ECA (KEY . VAL) header."
  (let ((pos (string-match "\:" s))
        key val)
    (unless pos
      (signal 'eca-invalid-header-name (list s)))
    (setq key (substring s 0 pos)
          val (s-trim-left (substring s (+ 1 pos))))
    (when (equal key "Content-Length")
      (cl-assert (cl-loop for c across val
                          when (or (> c ?9) (< c ?0)) return nil
                          finally return t)
                 nil (format "Invalid Content-Length value: %s" val)))
    (cons key val)))

(defun eca-process--make-filter (handle-msg)
  "Return a process filter function that parse JSON-RPC from stdout.
HANDLE-MSG is called for each complete message parsed.

The returned closure captures parsing state across invocations so that
messages larger than a single OS pipe read (e.g. large `write_file'
tool calls with full diffs) are reassembled correctly."
  (let (;; Persistent state across filter invocations:
        (leftovers nil)
        (body-length nil)
        (body-received 0)
        (body nil))
    (lambda (_proc raw-output)
      (let ((gc-cons-threshold (max gc-cons-threshold eca-min-gc-cons-threshold))
            chunk)
        (setf chunk (if (s-blank? leftovers)
                        (encode-coding-string raw-output 'utf-8-unix t)
                      (concat leftovers (encode-coding-string raw-output 'utf-8-unix t))))
        (setf leftovers nil)
        (let (messages)
          (while (not (s-blank? chunk))
            (if (not body-length)
                ;; Read headers
                (if-let* ((body-sep-pos (string-match-p "\r\n\r\n" chunk)))
                    ;; We've got all the headers, handle them all at once:
                    (setf body-length (let* ((headers (mapcar #'eca-process--parse-header
                                                              (split-string
                                                               (substring-no-properties chunk
                                                                                        (or (string-match-p "Content-Length" chunk)
                                                                                            (error "Unable to find Content-Length header"))
                                                                                        body-sep-pos)
                                                               "\r\n")))
                                             (content-length (cdr (assoc "Content-Length" headers))))
                                        (if content-length
                                            (string-to-number content-length)
                                          ;; This usually means either the server or our parser is
                                          ;; screwed up with a previous Content-Length
                                          (error "No Content-Length header")))
                          body-received 0
                          body nil
                          chunk (substring-no-properties chunk (+ body-sep-pos 4)))

                  ;; Haven't found the end of the headers yet. Save everything
                  ;; for when the next chunk arrives and await further input.
                  (setf leftovers chunk
                        chunk nil))
              (let* ((chunk-length (string-bytes chunk))
                     (left-to-receive (- body-length body-received))
                     (this-body (if (< left-to-receive chunk-length)
                                    (prog1 (substring-no-properties chunk 0 left-to-receive)
                                      (setf chunk (substring-no-properties chunk left-to-receive)))
                                  (prog1 chunk
                                    (setf chunk nil))))
                     (body-bytes (string-bytes this-body)))
                (push this-body body)
                (setf body-received (+ body-received body-bytes))
                (when (>= chunk-length left-to-receive)
                  (condition-case err
                      (with-temp-buffer
                        (apply #'insert
                               (nreverse
                                (prog1 body
                                  (setf body-length nil
                                        body-received 0
                                        body nil))))
                        (decode-coding-region (point-min)
                                              (point-max)
                                              'utf-8)
                        (goto-char (point-min))
                        (push (eca-api--json-read-buffer) messages))

                    (error
                     (setf body-length nil
                           body-received 0
                           body nil)
                     (eca-warn "Failed to parse the following chunk:\n'''\n%s\n'''\nwith message %s"
                               (concat leftovers raw-output)
                               err)))))))
          (mapc handle-msg
                (nreverse messages)))))))

;; Public

(defun eca-process-start (session on-start handle-msg)
  "Start the eca process for SESSION calling ON-START after.
Call HANDLE-MSG for new msgs processed."
  (unless (process-live-p (eca--session-process session))
    ;; Clean up any .old binary from previous updates
    (eca-process--cleanup-old-server)
    (-let* ((default-directory (or (car (eca--session-workspace-folders session))
                                   default-directory))
            ((result &as &plist :decision decision :command command) (eca-process--server-command))
            (start-process-fn (lambda ()
                                (let ((command (append command eca-extra-args)))
                                  (when eca-process-wrapper-function
                                    (setq command
                                          (funcall eca-process-wrapper-function
                                                   command
                                                   (eca--session-workspace-folders session))))
                                  (eca-info "Starting process '%s'" (string-join command " "))
                                  (setf (eca--session-process session)
                                        (make-process
                                         :coding 'no-conversion
                                         :connection-type 'pipe
                                         :name "eca"
                                         :command command
                                         :buffer (eca-process--buffer-name session)
                                         :stderr (get-buffer-create (eca-process--stderr-buffer-name session))
                                         :filter (eca-process--make-filter handle-msg)
                                         :sentinel (lambda (process exit-str)
                                                     (unless (process-live-p process)
                                                       (when-let* ((name (eca-process--stderr-buffer-name session))
                                                                   (buf (get-buffer name)))
                                                         (with-current-buffer buf
                                                           (rename-buffer (concat (buffer-name) ":closed") t)
                                                           (setq-local mode-line-format '("*Closed session*"))))
                                                       (eca-delete-session session)
                                                       (eca-info "process has exited (%s)" (s-trim exit-str))))
                                         :file-handler t
                                         :noquery t)))
                                (funcall on-start))))
      (pcase decision
        ('custom (funcall start-process-fn))

        ('system (funcall start-process-fn))

        ('error-download (user-error (eca-error (plist-get result :message))))

        ('already-installed (funcall start-process-fn))

        ('download (eca-process--download-server (lambda ()
                                                   (funcall start-process-fn))
                                                 (plist-get result :latest-version)))))))

(defun eca-process-running-p (session)
  "Return non nil if eca process for SESSION is running."
  (and session
       (process-live-p (eca--session-process session))))

(defun eca-process-stop (session)
  "Stop the eca process for SESSION if running."
  (when session
    (kill-process (eca--session-process session))
    (kill-buffer (eca-process--buffer-name session))
    ;; Rename stderr buffer to closed and clean up older closed ones
    (let ((stderr-buffer (get-buffer (eca-process--stderr-buffer-name session))))
      (when stderr-buffer
        (with-current-buffer stderr-buffer
          (rename-buffer (concat (buffer-name) ":closed") t)
          (setq-local mode-line-format '("*Closed session*"))
          (when-let ((win (get-buffer-window (current-buffer))))
            (quit-window nil win))
          ;; Keep only the most recently closed stderr buffer; kill older ones.
          ;; Only kill :closed buffers — never non-closed ones which belong
          ;; to active sessions.
          (let ((current (current-buffer)))
            (dolist (b (buffer-list))
              (when (and (not (eq b current))
                         (string-match-p "^<eca:stderr:.*>:closed" (buffer-name b)))
                (kill-buffer b)))))))))

(defun eca-process-show-stderr (session)
  "Open the eca process stderr buffer for SESSION."
  (if-let ((buf (get-buffer (eca-process--stderr-buffer-name session))))
      (if (window-live-p (get-buffer-window buf))
          (select-window (get-buffer-window buf))
        (display-buffer buf))
    (message "No stderr buffer for session %d"
             (eca--session-id session))))

(defun eca-process--server-version ()
  "Return the server version by running the eca binary with --version."
  (when-let* ((binary (or (car eca-custom-command)
                          (executable-find "eca" (file-remote-p default-directory))
                          (when (f-exists? eca-server-install-path)
                            eca-server-install-path)))
              (program (eca-process--program-path binary))
              (output (ignore-errors
                        (with-temp-buffer
                          (when (zerop (process-file program nil
                                                     (list (current-buffer) (null-device))
                                                     nil "--version"))
                            (string-trim (buffer-string)))))))
    (unless (string-empty-p output)
      output)))

;;;###autoload
(defun eca-show-stderr ()
  "Open the eca process stderr buffer if running."
  (interactive)
  (eca-process-show-stderr (eca-session)))

;;;###autoload
(defun eca-install-server ()
  "Force download the latest eca server.
Clears `eca-process--releases-cache' first so the latest version is
re-checked against GitHub even within a long-running Emacs session."
  (interactive)
  (setq eca-process--releases-cache nil)
  (eca-process--download-server (lambda ())
                                (eca-process--get-latest-server-version)))

;;;###autoload
(defun eca-server-check-updates ()
  "Check GitHub for a newer eca server release.
Bypasses `eca-process--releases-cache' (and thus
`eca-server-releases-cache-ttl') so the answer is always fresh.
Reports via the echo area whether the installed server is up to date,
whether a newer version is available, or whether the check failed."
  (interactive)
  (setq eca-process--releases-cache nil)
  (let ((latest (eca-process--get-latest-server-version))
        (current (eca-process--get-current-server-version)))
    (cond
     ((null latest)
      (eca-warn "Could not check for eca server updates."))
     ((null current)
      (eca-info
       (concat "No eca server installed; latest available is %s. "
               "Run M-x eca-install-server to install.")
       latest))
     ((string-version-lessp current latest)
      (eca-info
       (concat "eca server %s is available (installed: %s). "
               "Run M-x eca-install-server to upgrade.")
       latest current))
     (t
      (eca-info "eca server is up to date (%s)." current)))))

;;;###autoload
(defun eca-uninstall-server ()
  "Remove downloaded eca server if present."
  (interactive)
  (eca-process--uninstall-server)
  (eca-info "Server uninstalled!"))

(provide 'eca-process)
;;; eca-process.el ends here
