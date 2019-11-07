;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

(defun ncw/idea-open ()
  (interactive)
  (async-shell-command
   (format "idea-open %s %s"
           (buffer-file-name)
           (format-mode-line "%l"))))
