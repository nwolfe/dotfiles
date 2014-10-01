;; Place your bindings here.

;; For example:
;;(define-key global-map (kbd "C-+") 'text-scale-increase)
;;(define-key global-map (kbd "C--") 'text-scale-decrease)

;; Remap C-a to smarter-move-beginning-of-line
(global-set-key [remap move-beginning-of-line]
                'smarter-move-to-beginning-of-line)
