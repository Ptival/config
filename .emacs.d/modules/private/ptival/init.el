;;; private/ptival/init.el

(load "ProofGeneral/generic/proof-site")

(setq doom-font (font-spec :size 15)
      doom-unicode-font (font-spec :family "DejaVu Sans Mono" :size 15))

; prevents Evil from doing auto-completion when pressing Esc
; (e.g. in ProofGeneral it's super annoying)
(setq evil-want-abbrev-expand-on-insert-exit nil)

; I really don't like how Haskell indents the line after a `let`, this prevents
; it, but also has some bad side-effects
(setq haskell-indentation-starter-offset 0)

; exactly one newline at the end of every file, unconditionally
(setq require-final-newline t)
(setq delete-trailing-lines t)

; preserve double spaces after period when re-flowing text into 80 columns
(setq sentence-end-double-space t)

; will display nice file type icons next to files
(setq +ivy-buffer-icons t)

; this tells popups where to go, default tends to be bottom and small
;(set! :popup "^\\*intero:" :regexp t :align 'right :size 0.5)
;(set! :popup "^\\*OCaml\\*$" :regexp t :align 'right :size 0.5)

; mouse pointer is more visible when red
(set-mouse-color "red")

; deletes trailing whitespaces on every line upon saving files
(add-hook! 'before-save-hook 'delete-trailing-whitespace)

; AUCTeX should ask us what the main .tex is
(setq-default TeX-master nil)
