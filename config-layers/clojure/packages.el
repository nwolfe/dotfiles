;; (defun clojure/init-cider-mode ()
;;   (use-package cider
;;     :config))

;; (evil-leader/set-key-for-mode 'clojure-mode
;;   "d r j" 'cider-jack-in
;;   "d r l" 'cider-load-buffer
;;   "d r n" 'cider-repl-set-ns
;;   "d r z" 'cider-switch-to-repl-buffer)

(defvar clojure-packages '(clojure-mode cider align-cljlet))

(defun fancy-symbols ()
  (font-lock-add-keywords 'clojure-mode
    `(("(\\(fn\\)[\[[:space:]]"
       (0 (progn (compose-region (match-beginning 1)
                                 (match-end 1) "λ"))))
      ("\\(#\\)("
       (0 (progn (compose-region (match-beginning 1)
                                 (match-end 1) "ƒ"))))
      ("\\(#\\){"
       (0 (progn (compose-region (match-beginning 1)
                                 (match-end 1) "∈")))))))

(defun clojure/init-clojure-mode ()
  (use-package clojure-mode
    :config
    (progn
      (add-hook 'clojure-mode-hook 'smartparens-strict-mode)
      (add-hook 'clojure-mode-hook 'evil-lisp-state)
      (fancy-symbols))))
