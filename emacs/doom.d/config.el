;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Auto-enable pretty mode for org files
(add-hook! 'org-mode-hook '+org-pretty-mode)

;; Shell out to `idea-open <file> <line>'
;; to open the current file in IntelliJ IDEA
(defun ncw/idea-open ()
  (interactive)
  (async-shell-command
   (format "idea-open %s %s"
           (buffer-file-name)
           (format-mode-line "%l"))))
