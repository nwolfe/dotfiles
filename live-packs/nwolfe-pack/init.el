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
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

;; Install extra packages
(dolist (p '(coffee-mode
             puppet-mode
             projectile
             helm
             helm-projectile))
  (when (not (package-installed-p p))
    (package-install p)))

;; Activate global modes
(projectile-global-mode)

;; Custom snippets location, and load them now
(setq yas-snippet-dirs
      '("~/.live-packs/nwolfe-pack/snippets"
        "~/.emacs.d/etc/snippets"))
(yas-reload-all)

;; Enable puppet-mode for .pp files
(add-to-list 'auto-mode-alist '("\\.pp\\'" . puppet-mode))
