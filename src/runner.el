;;; -*- lexical-binding: t -*-

(require 'org-make-toc)

(let ((has-error nil))
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
