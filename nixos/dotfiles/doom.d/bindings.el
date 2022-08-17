;;; private/ptival/+bindings.el -*- lexical-binding: t; -*-

;; cf. https://stackoverflow.com/questions/4351044/binding-m-up-m-down-in-emacs-23-1-1
(define-key function-key-map "\eOA" [up])
(define-key function-key-map "\eOB" [down])
(define-key function-key-map "\eOC" [right])
(define-key function-key-map "\eOD" [left])
(define-key function-key-map "\e\eOA" [M-up])
(define-key function-key-map "\e\eOB" [M-down])
(define-key function-key-map "\e\eOC" [M-right])
(define-key function-key-map "\e\eOD" [M-left])

;;; Code:
(map!
 (:after coq
   :map coq-mode-map
   "M-<up>" #'proof-undo-last-successful-command
   "M-<right>" #'proof-goto-point
   "M-<down>" #'proof-assert-next-command-interactive
   "M-C-<up>" #'proof-undo-last-successful-command
   "M-C-<right>" #'proof-goto-point
   "M-C-<down>" #'proof-assert-next-command-interactive))
