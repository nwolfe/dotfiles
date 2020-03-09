;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; Better defaults. No starter kits!
(add-to-list 'load-path "c:/Users/a1979/Documents/better-defaults")
(require 'better-defaults)

;; Disable annoying autosave/backup files
(setq auto-save-default nil)
(setq make-backup-files nil)

;; Enable indent mode for all .org files
(setq org-startup-indented t)

;; Show cursor location in modeline
(column-number-mode)
