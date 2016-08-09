
(defun ncw/configure-erc ()
  "Configure IRC. Associated commands:
   * `ncw/irc-jack-in' Connect to all servers
   * `ncw/irc-layout'  Configure current frame with channel windows"
  (require 'ncw-irc-config nil t)

  (setq erc-prompt-for-nickserv-password nil
        erc-hide-list '("JOIN" "NICK" "PART" "QUIT" "MODE"))

  ;; Don't start buffers in insert mode
  (setq evil-normal-state-modes '(erc-mode))

  ;; Don't wrap long lines
  (add-hook 'erc-join-hook 'toggle-truncate-lines)

  ;; Automatically insert newlines
  (erc-fill-mode t)

  ;; Filter foolish content
  ;; "\\[GitHub\\].* labeled an issue in"
  ;; "\\[GitHub\\].* unlabeled an issue in"
  (setq erc-foolish-content '("Users on #.*"
                              "\\[GitHub\\].* starred"
                              "\\[GitHub\\].* forked"
                              "\\[GitHub\\].* synchronize a Pull Request"))
  (defun ncw/erc-filter-foolish-content (msg)
    "Ignore messages that match `erc-foolish-content'."
    (when (erc-list-match erc-foolish-content msg)
      (setq erc-insert-this nil)))
  (add-hook 'erc-insert-pre-hook 'ncw/erc-filter-foolish-content))

(provide 'ncw-irc)
