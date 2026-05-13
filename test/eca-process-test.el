;;; eca-process-test.el --- Tests for eca-process -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'buttercup)
(require 'tramp)
(require 'time-stamp)
(require 'eca-process)

(describe "eca-process--server-command"
  (it "uses remote executable-find and strips TRAMP from the command path"
    (let ((default-directory "/ssh:host:/tmp/")
          (eca-custom-command nil))
      (spy-on 'executable-find :and-call-fake
              (lambda (command &optional remote)
                (when (and (string= command "eca") remote)
                  "/ssh:host:/usr/bin/eca")))
      (let ((result (eca-process--server-command)))
        (expect (plist-get result :decision) :to-equal 'system)
        (expect (plist-get result :command) :to-equal '("/usr/bin/eca" "server")))))

  (it "strips TRAMP from eca-custom-command on remote hosts"
    (let ((default-directory "/ssh:host:/tmp/")
          (eca-custom-command '("/ssh:host:/opt/eca" "server" "--foo")))
      (spy-on 'executable-find :and-return-value nil)
      (let ((result (eca-process--server-command)))
        (expect (plist-get result :decision) :to-equal 'custom)
        (expect (plist-get result :command)
                :to-equal '("/opt/eca" "server" "--foo")))))

  (it "resolves ~ and strips TRAMP for already-installed :command on remote"
    (let ((default-directory "/ssh:host:/workspace/")
          (eca-custom-command nil)
          (eca-server-install-path "~/.eca/bin/eca"))
      (spy-on 'tramp-get-home-directory :and-return-value "/home/node")
      (spy-on 'executable-find :and-return-value nil)
      (spy-on 'f-exists? :and-return-value t)
      (spy-on 'eca-process--get-current-server-version :and-return-value "1.0.0")
      (spy-on 'eca-process--get-latest-server-version :and-return-value "1.0.0")
      (let ((result (eca-process--server-command)))
        (expect (plist-get result :decision) :to-equal 'already-installed)
        (expect (plist-get result :command)
                :to-equal '("/home/node/.eca/bin/eca" "server"))))))

(describe "eca-process--server-paths"
  (it "derives all four paths from the TRAMP-aware store path"
    (let ((default-directory "/ssh:host:/workspace/")
          (eca-server-install-path "~/.eca/bin/eca"))
      (spy-on 'tramp-get-home-directory :and-return-value "/home/node")
      (let ((p (eca-process--server-paths)))
        (expect (plist-get p :store)
                :to-equal "/ssh:host:/home/node/.eca/bin/eca")
        (expect (plist-get p :download)
                :to-equal "/ssh:host:/home/node/.eca/bin/eca.zip")
        (expect (plist-get p :old)
                :to-equal "/ssh:host:/home/node/.eca/bin/eca.old")
        (expect (plist-get p :temp-extract)
                :to-equal "/ssh:host:/home/node/.eca/bin-temp")
        (expect (plist-get p :version-file)
                :to-equal "/ssh:host:/home/node/.eca/bin/eca-version"))))

  (it "never leaks host HOME into any path on remote"
    (let ((default-directory "/ssh:host:/workspace/")
          (process-environment (cons "HOME=/Users/andi" process-environment))
          (eca-server-install-path "~/.eca/bin/eca"))
      (spy-on 'tramp-get-home-directory :and-return-value "/home/node")
      (let ((p (eca-process--server-paths)))
        (dolist (k '(:store :download :old :temp-extract))
          (expect (plist-get p k) :not :to-match "/Users/andi")
          (expect (plist-get p k) :to-match "^/ssh:host:/home/node"))))))

(describe "eca--curl-download-file"
  (it "passes -o with remote-local path (no literal ~)"
    (let ((default-directory "/ssh:host:/workspace/")
          (real-make-process (symbol-function 'make-process))
          captured-command
          captured-plist)
      (spy-on 'tramp-get-home-directory :and-return-value "/root")
      (spy-on 'executable-find :and-return-value "/usr/bin/curl")
      (spy-on 'make-process :and-call-fake
              (lambda (&rest kwargs)
                (setq captured-plist kwargs
                      captured-command (plist-get kwargs :command))
                ;; Run a no-op child locally only: forwarding `:file-handler'
                ;; would make TRAMP try to spawn on the fake remote host.
                (let ((default-directory "/tmp/"))
                  (apply real-make-process
                         (list :name "eca-curl-stub"
                               :buffer (plist-get kwargs :buffer)
                               :command '("/bin/true")
                               :noquery t
                               :sentinel (plist-get kwargs :sentinel))))))
      (eca--curl-download-file :url "https://example.com/eca.zip"
                               :path "~/.eca/bin/eca.zip"
                               :on-done #'ignore)
      (expect (plist-get captured-plist :file-handler) :to-be t)
      (let ((o-arg
             (catch 'eca-o
               (let ((cmd captured-command))
                 (while cmd
                   (when (equal (car cmd) "-o")
                     (throw 'eca-o (cadr cmd)))
                   (setq cmd (cdr cmd)))))))
        (expect o-arg :to-equal "/root/.eca/bin/eca.zip")))))

(describe "eca-process--server-paths (local regression)"
  (it "uses host HOME locally without TRAMP prefix"
    (let ((default-directory "/tmp/eca-process-test-local/")
          (process-environment (cons "HOME=/home/test" process-environment))
          (eca-server-install-path "~/.eca/bin/eca"))
      (let ((p (eca-process--server-paths)))
        (expect (plist-get p :store)        :to-equal "/home/test/.eca/bin/eca")
        (expect (plist-get p :download)     :to-equal "/home/test/.eca/bin/eca.zip")
        (expect (plist-get p :old)          :to-equal "/home/test/.eca/bin/eca.old")
        (expect (plist-get p :temp-extract) :to-equal "/home/test/.eca/bin-temp")))))

(describe "eca-process--unzip-archive"
  (it "invokes bash, not host shell-file-name, on remote"
    (let ((default-directory "/ssh:host:/workspace/")
          ;; Simulate macOS host: zsh exists locally but won't exist in
          ;; most Linux containers. Routing through it would crash the
          ;; remote exec before unzip ever ran.
          (shell-file-name "/bin/zsh")
          (eca-ext-unzip-script "bash -c 'mkdir -p %2$s && unzip -qq -o %1$s -d %2$s'")
          captured-program captured-args)
      (spy-on 'process-file :and-call-fake
              (lambda (program &rest args)
                (setq captured-program program
                      captured-args args)
                0))
      (eca-process--unzip-archive
       "/ssh:host:/home/node/.eca/bin/eca.zip"
       "/ssh:host:/home/node/.eca/bin-temp")
      (expect captured-program :to-equal "bash")
      ;; args = (INFILE BUFFER DISPLAY &rest PROGRAM-ARGS)
      (expect (nth 3 captured-args) :to-equal "-c")
      (expect (nth 4 captured-args) :to-match "/home/node/.eca/bin/eca.zip")
      (expect (nth 4 captured-args) :to-match "/home/node/.eca/bin-temp")))

  (it "raises with stderr text when extraction fails"
    (let ((default-directory "/ssh:host:/workspace/")
          (eca-ext-unzip-script "bash -c 'mkdir -p %2$s && unzip -qq -o %1$s -d %2$s'"))
      (spy-on 'process-file :and-call-fake
              (lambda (_program _infile buffer &rest _rest)
                ;; Production calls with BUFFER = t (current buffer).
                (when (eq buffer t)
                  (insert "unzip: cannot find zipfile"))
                9))
      (expect (eca-process--unzip-archive
               "/ssh:host:/missing.zip"
               "/ssh:host:/tmp/out")
              :to-throw 'error))))

(provide 'eca-process-test)
;;; eca-process-test.el ends here
