;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Auto-enable pretty mode for org files
(add-hook! 'org-mode-hook '+org-pretty-mode)

;; Default is 80 in Doom Emacs
(setq-default fill-column 70)

;; Swap Option<->Command keys so Meta key is easier to reach
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

(defun ncw/idea-open ()
  "Shell out to `idea-open <file> <line>' to open current file in IntelliJ IDEA."
  (interactive)
  (async-shell-command
   (format "idea-open %s %s"
           (buffer-file-name)
           (format-mode-line "%l"))))
