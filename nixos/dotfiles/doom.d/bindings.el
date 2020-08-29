;;; private/ptival/+bindings.el -*- lexical-binding: t; -*-

;;; Code:
(map!
 (:after coq
   :map coq-mode-map
   "M-C-<up>" #'proof-undo-last-successful-command
   "M-C-<right>" #'proof-goto-point
   "M-C-<down>" #'proof-assert-next-command-interactive))
