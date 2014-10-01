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

;; Smarter C-a
;; Copied from: http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/
(defun smarter-move-to-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

   Move point to the first non-whitespace character on this line.
   If point is already there, move to the beginning of the line.
   Effectively toggle between the first non-whitespace character
   and the beginning of the line.

   If ARG is not nil or 1, move forward ARG-1 lines first. If
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
