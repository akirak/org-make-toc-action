;;; -*- lexical-binding: t -*-

;; Prevent error message open when a file is opened in a buffer
(setq vc-handled-backends nil)

(require 'org-make-toc)

(unless command-line-args-left
  (message "Requires an argument")
  (kill-emacs 1))

(let ((has-error nil))
  (setq make-backup-files nil)
  (dolist (file command-line-args-left)
    (if (string-match-p (rx ".org" eol) file)
        (let ((start (float-time)))
          (with-current-buffer (find-file-noselect file)
            (org-make-toc)
            (save-buffer)
            (message "%.2f sec  %s"
                     (- (float-time) start)
                     file)))
      (progn
        (setq has-error t)
        (error "Not an Org file: %s" file))))
  (kill-emacs (if has-error 1 0)))
