(defvar nwolfe-packages '(ag erc))

(defun nwolfe/init-erc ()
  (use-package erc
    :init
    (progn
      ;; Automatically join channels on startup
      (erc-autojoin-mode t)
      (setq erc-autojoin-channels-alist
            '((".*\\.freenode.net" "#puppet" "#trapperkeeper")))

      ;; Don't notify me about these events
      (setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                      "324" "329" "332" "333" "353" "477"))

      ;; Don't show these events in the channel
      (setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK")))))
