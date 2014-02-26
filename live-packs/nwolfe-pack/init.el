;; User pack init file
;;
;; Use this file to initiate the pack configuration.
;; See README for more information.

;; Load bindings config
(live-load-config-file "bindings.el")

;; Add package repositories
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

;; Install extra packages
(dolist (p '(projectile))
  (when (not (package-installed-p p))
    (package-install p)))

;; Activate global modes
(projectile-global-mode)

;; Redirect yasnippets
(setq yas-snippet-dirs
      '("~/.live-packs/nwolfe-pack/snippets"
        "~/.emacs.d/etc/snippets"))
