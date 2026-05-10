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

  (it "returns remote-not-found when eca is missing on a remote host"
    (let ((default-directory "/ssh:host:/tmp/")
          (eca-custom-command nil))
      (spy-on 'executable-find :and-return-value nil)
      (let ((result (eca-process--server-command)))
        (expect (plist-get result :decision) :to-equal 'remote-not-found)
        (expect (plist-get result :message) :to-match "eca not found on remote host")))))

(provide 'eca-process-test)
;;; eca-process-test.el ends here
