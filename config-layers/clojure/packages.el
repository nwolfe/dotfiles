(defvar clojure-packages
  '(clojure-mode
    cider
    align-cljlet
    smartparens
    rainbow-delimiters
    ))

(defun clojure/fancify-symbols ()
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

(defun clojure/bind-keys ()
  (evil-leader/set-key-for-mode 'clojure-mode
    "mps" 'sp-forward-slurp-sexp))

(defun clojure/init-clojure-mode ()
  (use-package clojure-mode
    :config
    (progn
      (add-hook 'clojure-mode-hook 'smartparens-strict-mode)
      (add-hook 'clojure-mode-hook 'evil-lisp-state)
      (add-hook 'clojure-mode-hook 'cider-mode)
      (clojure/fancify-symbols)
      (clojure/bind-keys)
      )))

(defun clojure/init-cider ()
  (use-package cider
    :config
    (progn
      (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
      (add-hook 'cider-repl-mode-hook 'smartparens-strict-mode)
      (add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode))))
