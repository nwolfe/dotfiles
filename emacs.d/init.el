(require 'package)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar packages '(starter-kit
			 starter-kit-lisp
			 starter-kit-ruby
			 starter-kit-bindings
			 starter-kit-eshell
                         color-theme-solarized
                         clojure-mode
                         clojure-test-mode
                         cider
                         ))

(dolist (p packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; themes
;;   solarized-[light|dark]
;;   tomorrow-night-bright
;;   tomorrow-night
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")
(load-theme 'tomorrow-night t)


