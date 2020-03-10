;; Enable MELPA
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
  ;; and `package-pinned-packages`. Most users will not need or want to do this.
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  )
(package-initialize)

;; Use 'use-package' to install other packages
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
(setq use-package-always-ensure t)

(use-package better-defaults)

(use-package which-key
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.5))

(use-package company
  :config
  (setq company-idle-delay 0.1)
  (setq company-global-modes '(not org-mode))
  (setq company-minimum-prefix-length 1)
  (add-hook 'after-init-hook 'global-company-mode))

(use-package org
  :config
  ;; Enable indent mode for all .org files
  (setq org-startup-indented t)
  ;; Auto-wrap lines when they exceed fill column
  (add-hook 'org-mode-hook 'turn-on-auto-fill))

(use-package magit)

;; Shortcut to open Emacs configuration file
(defun ncw/configure-emacs ()
  "Open Emacs configuration file (init.el)."
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(global-set-key (kbd "C-x C") 'ncw/configure-emacs)

;; Don't create #autosave# files
(setq auto-save-default nil)
;; Don't create backup~ files
(setq make-backup-files nil)
;; Don't create .# files
(setq create-lockfiles nil)

;; Store "custom" file separately
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; Show cursor location in modeline
(column-number-mode)

;; Better visual line wrapping
;; (global-visual-line-mode)

;; Show absolute line numbers
;; (global-linum-mode)

;; No need for verbosity
(fset 'yes-or-no-p 'y-or-n-p)

;; Auto-update buffers when they are changed externally
(global-auto-revert-mode t)

;; Delete trailing spaces on file save.
;; A newline is added to EOF by better-defaults package
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Setup recent files. Use "C-x C-r" to open recent files.
;; NOTE this overrides the default keybinding for
;; `find-file-read-only`.
(require 'recentf)
(setq recentf-max-saved-items 50
      recentf-max-menu-items 15)
(recentf-mode)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)
