;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

(defun ncw/idea-open ()
  (interactive)
  (async-shell-command (concat "idea-open " (buffer-file-name))))
