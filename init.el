;; Enable MELPA
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
  ;; and `package-pinned-packages`. Most users will not need or want to do this.
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
  )
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
(column-number-mode 1)

;; No need for verbosity
(fset 'yes-or-no-p 'y-or-n-p)
