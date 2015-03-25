;; Place your bindings here.

;; For example:
;;(define-key global-map (kbd "C-+") 'text-scale-increase)
;;(define-key global-map (kbd "C--") 'text-scale-decrease)

;; Remap C-a to smarter-move-beginning-of-line
(global-set-key [remap move-beginning-of-line]
                'smarter-move-to-beginning-of-line)

(global-set-key (kbd "M-x") 'helm-M-x)

;; For some reason you can't use `global-set-key` to define this.
;; Maybe it has something to do with the fact that it's not
;; prepended with a modifier key (i.e. JJ vs C-JJ or M-JJ)?
(key-chord-define-global "JJ" 'prelude-switch-to-previous-buffer)
