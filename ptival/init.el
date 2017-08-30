;;; private/ptival/init.el

(load "ProofGeneral/generic/proof-site")

(setq delete-trailing-lines t)

(setq doom-font (font-spec :size 15)
      doom-unicode-font (font-spec :family "DejaVu Sans Mono" :size 15))

; prevents Evil from doing auto-completion when pressing Esc
; (e.g. in ProofGeneral it's super annoying)
(setq evil-want-abbrev-expand-on-insert-exit nil)

(setq require-final-newline t)

; preserve double spaces after period when re-flowing text into 80 columns
(setq sentence-end-double-space t)

; will display nice file type icons next to files
(setq +ivy-buffer-icons t)

(set! :popup "^\\*intero:" :regexp t :align 'right :size 0.5)

(set-mouse-color "red")

(add-hook! 'before-save-hook 'delete-trailing-whitespace)
