(defconst IS-MAC (eq system-type 'darwin))
(defconst IS-WINDOWS (memq system-type '(windows-nt ms-dos)))

;; Enable MELPA
(require 'package)
(let* ((no-ssl (and IS-WINDOWS (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
  ;; and `package-pinned-packages`. Most users will not need or want to do this.
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  )
(package-initialize)

;; Use 'use-package' to install other packages
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
(setq use-package-always-ensure t)
;; Allow `:if' and `:ensure' to work together. See
;; https://github.com/jwiegley/use-package/issues/637
(setq use-package-keywords (cons :unless (delq :unless use-package-keywords)))
(setq use-package-keywords (cons :when (delq :when use-package-keywords)))
(setq use-package-keywords (cons :if (delq :if use-package-keywords)))

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

;; No need for verbosity
(fset 'yes-or-no-p 'y-or-n-p)

;; Auto-update buffers when they are changed externally
(global-auto-revert-mode t)

;; Delete trailing spaces on file save.
;; A newline is added to EOF by better-defaults package
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; (Mac OS) Swap Option<->Command keys so Meta is easier to reach
(when IS-MAC
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super))

;; Treat symlinks like normal files; don't prompt me
(setq vc-follow-symlinks t)

;; 8 is just too many
(setq-default tab-width 4)

;; Insert matching parenthesis, etc.
(electric-pair-mode)

;; Smarter move-to-beginning-of-line
;; http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.
Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.
If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))
  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))
(global-set-key (kbd "C-a") 'smarter-move-beginning-of-line)

(use-package org ; builtin
  :config
  ;; Enable indent mode for all .org files
  (setq org-startup-indented t)
  ;; Auto-wrap lines when they exceed fill column
  (add-hook 'org-mode-hook 'turn-on-auto-fill)
  (setq org-hide-emphasis-markers t))

(use-package recentf ; builtin
  :config
  (recentf-mode))

(use-package better-defaults)

(use-package doom-themes
  :config
  (load-theme 'doom-one))

(use-package which-key
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.5))

(use-package company
  :config
  (setq company-idle-delay 0.1)
  (setq company-global-modes '(not org-mode eshell-mode))
  (setq company-minimum-prefix-length 1)
  (setq company-dabbrev-downcase nil)
  (add-hook 'after-init-hook 'global-company-mode))

(use-package magit
  :bind
  ("C-x g" . magit-status))

(use-package ivy
  :config
  (ivy-mode)
  ;; Show recent files (from recentf-mode) in switch buffer list
  (setq ivy-use-virtual-buffers t))

(use-package swiper
  :bind
  ("C-s" . swiper))

(use-package avy
  :bind
  ("C-:" . avy-goto-char-timer))

(use-package amx
  :config
  (amx-mode))

(use-package projectile
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode)
  (setq projectile-completion-system 'ivy)
  (setq projectile-switch-project-action
        (lambda ()
          (magit-status)
          (delete-other-windows))))

(use-package ripgrep
  :unless IS-WINDOWS
  :config
  (setq ripgrep-executable "/usr/local/bin/rg"))

(use-package hl-todo
  :config
  (global-hl-todo-mode))

(use-package git-gutter
  :config
  (global-git-gutter-mode))

(use-package dockerfile-mode
  :unless IS-WINDOWS)

(use-package groovy-mode
  :unless IS-WINDOWS
  :config
  (setq groovy-indent-offset 2)
  (add-hook 'groovy-mode-hook 'hl-todo-mode))

(use-package markdown-mode
  :unless IS-WINDOWS)

(use-package markdown-toc
  :unless IS-WINDOWS)

(use-package yaml-mode
  :unless IS-WINDOWS)

(use-package gist)
