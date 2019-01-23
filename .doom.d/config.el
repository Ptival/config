;;; private/ptival/config.el -*- lexical-binding: t; -*-

;;; This is so emacs takes focus when started, on OSX
; (x-focus-frame nil)
(add-hook! 'window-setup-hook (x-focus-frame nil))

(setq doom-theme 'doom-vibrant)

(when (eq system-type 'darwin)
  (setq mac-control-modifier 'control)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier  'alt)
  (define-key!
    [A-left]        #'left-word
    [A-right]       #'right-word
    [A-up]          #'evil-backward-paragraph
    [A-down]        #'evil-forward-paragraph
    [A-backspace]   #'backward-kill-word
    [A-S-backspace] #'kill-word
    [A-delete]      #'kill-word
    [A-kp-delete]   #'kill-word
    [A-SPC]         #'just-one-space
    [A-backslash]   #'delete-horizontal-space
    [M-left]        #'doom/backward-to-bol-or-indent
    [M-right]       #'doom/forward-to-last-non-comment-or-eol
    [M-up]          #'evil-backward-paragraph
    [M-down]        #'evil-forward-paragraph
    [M-backspace]   #'doom/backward-kill-to-bol-and-indent
    [M-delete]      #'kill-line
    [M-kp-delete]   #'kill-word
    ))

;;; Code:
(when (featurep! :feature evil)
  (load! "bindings"))

; don't need this anymore now that ProofGeneral is in MELPA
; (load "ProofGeneral/generic/proof-site")
(setq proof-three-window-mode-policy 'hybrid)
(setq proof-electric-terminator-enable nil) ; annoys me

(setq doom-font (font-spec :family "DejaVu Sans Mono" :size 12)
      doom-unicode-font (font-spec :family "DejaVu Sans Mono" :size 12)
      doom-big-font (font-spec :family "DejaVu Sans Mono" :size 12))

; prevents Evil from doing auto-completion when pressing Esc
; (e.g. in ProofGeneral it's super annoying)
(setq evil-want-abbrev-expand-on-insert-exit nil)

; I really don't like how Haskell indents the line after a `let`, this prevents
; it, but also has some bad side-effects
(setq haskell-indentation-starter-offset 0)

; also align the qualified shorthand
(setq haskell-align-imports-pad-after-name t)

; exactly one newline at the end of every file, unconditionally
(setq require-final-newline t)
(setq delete-trailing-lines t)

; preserve double spaces after period when re-flowing text into 80 columns
(setq sentence-end-double-space t)

; will display nice file type icons next to files
(setq +ivy-buffer-icons t)

; this tells popups where to go, default tends to be bottom and small
(set-popup-rule! "^\\*intero:" :side 'bottom :size 0.20)
(set-popup-rule! "^\\*OCaml\\*$" :side 'bottom :size 0.3)

; mouse pointer is more visible when red
(set-mouse-color "red")

; deletes trailing whitespaces on every line upon saving files
(add-hook! 'before-save-hook 'delete-trailing-whitespace)

; AUCTeX should ask us what the main .tex is, not guess
(setq-default TeX-master nil)

; spellcheck when editing LaTeX files
(add-hook 'LaTeX-mode-hook #'turn-on-flyspell)

; prevents spellcheck on LaTeX output buffers
(setq-hook! 'LaTeX-mode-hook +spellcheck-immediately nil)

(setq +modeline-file-path-with-project 'modeline-file-path-truncated-upto-project-root)

(setq merlin-command "ocamlmerlin")

(setq flycheck-check-syntax-automatically '(mode-enabled save))

(setq flycheck-idle-change-delay 5)

(add-to-list 'default-frame-alist '(fullscreen . maximized))
