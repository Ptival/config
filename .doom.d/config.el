;;; private/ptival/config.el -*- lexical-binding: t; -*-

;;; lorri is installed globally, so it lives in /run/current-system/sw/bin
;;; but this path is not in the default PATH passed to emacs. Adding it,
;;; which requires setting both exec-path and PATH in process-environment.
(add-to-list 'exec-path "/run/current-system/sw/bin")
(setenv "PATH" (concat (getenv "PATH") ":/run/current-system/sw/bin"))

(add-hook! 'haskell-mode-hook
  (setq lsp-haskell-process-args-hie (list "-d" "-l" "/tmp/hie.log" "-r" (haskell-cabal-find-dir))))

(add-hook 'dante-mode-hook
  '(lambda () (flycheck-add-next-checker 'haskell-dante '(warning . haskell-hlint))))

;;; For direnv and coq to play nicely, we must update `coq-prog-env` when the
;;; direnv environment gets updated
(defun update-coq-prog-env (&rest _args)
  (setq coq-prog-env process-environment))
(add-hook! 'coq-mode-hook
           (advice-add 'direnv-update-directory-environment :after #'update-coq-prog-env))
;;; Sadly, exec-path gets overwritten at some point in the stack, so we
;;; overwrite it again just before calling scomint-exec-1
(defun add-nix-path (orig-func &rest args)
  "Adds nix's PATH to exec-path for finding coqtop succesfully"
  (let
    (
     (exec-path (split-string (getenv "PATH") ":"))
     (process-environment coq-prog-env))
    (apply orig-func args)))
(advice-add 'scomint-exec-1 :around #'add-nix-path)
(advice-add 'shell-command :around #'add-nix-path)

;;; This is so emacs takes focus when started, on OSX
; (x-focus-frame nil)
(add-hook! 'window-setup-hook (x-focus-frame nil))

; DOES NOT WORK
;(add-hook! 'haskell-mode-hook
;  (setq lsp-haskell-process-wrapper-function
;        (lambda (argv)
;          (append argv (list "-r" (lsp-haskell--get-root))))))

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
    ; This is mapped to M-\ by default, but let's also have it on A-\
    [A-\\]          #'delete-horizontal-space
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
    [M-kp-delete]   #'kill-line
    [M-v]           #'yank
    ; Coq commands
    [A-M-up]        #'proof-undo-last-successful-command
    [A-M-down]      #'proof-assert-next-command-interactive
    [A-M-right]     #'proof-goto-point
    [A-C-up]        #'proof-undo-last-successful-command
    [A-C-down]      #'proof-assert-next-command-interactive
    [A-C-right]     #'proof-goto-point
    ))

;;; Code:
(when (featurep! :feature evil)
  (load! "bindings"))

; don't need this anymore now that ProofGeneral is in MELPA
; (load "ProofGeneral/generic/proof-site")
(setq proof-three-window-mode-policy 'hybrid)
(setq proof-electric-terminator-enable nil) ; annoys me

; ProofGeneral does abbreviation expansion (looks like auto-completion) of many
; short words, which is extremely annoying in abbrev-mode.  Providing an empty
; abbreviation table overrides this behavior.
(defun erase-coq-abbrev-table ()
    "Erase the Coq abbreviation table."
    (setq coq-mode-abbrev-table '()))
(erase-coq-abbrev-table)
(add-hook 'coq-mode-hook #'erase-coq-abbrev-table)
; Disable the super annoying comment insertion behavior when pressing Enter
; inside a Coq comment
(setq-hook! 'coq-mode-hook comment-line-break-function nil)
(setq-hook! 'coq-mode-hook coq-prefer-top-of-conclusion t)
(setq-hook! 'coq-mode-hook coq-highlighted-hyps-bg "dark violet")

(setq doom-font (font-spec :family "Iosevka SS05" :size 16)
      doom-unicode-font (font-spec :family "Apple Color Emoji" :size 16)
      doom-big-font (font-spec :family "Iosevka SS05" :size 16))

;; default font
; (set-face-attribute 'default nil :family "Iosevka SS05")
; (set-fontset-font t 'unicode "DejaVu Sans Mono" nil 'prepend)

; (setq doom-font (font-spec :family "Fira Code" :size 14)
;       doom-unicode-font (font-spec :family "Fira Code" :size 14)
;       doom-big-font (font-spec :family "Fira Code" :size 14))

; (setq doom-font (font-spec :family "DejaVu Sans Mono" :size 12)
;       doom-unicode-font (font-spec :family "DejaVu Sans Mono" :size 12)
;       doom-big-font (font-spec :family "DejaVu Sans Mono" :size 12))

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
(set-popup-rule! "^\\*intero:" :side 'bottom :size 0.4)
(set-popup-rule! "^\\*OCaml\\*$" :side 'bottom :size 0.4)
(set-popup-rule! "^\\*Flycheck errors\\*$" :side 'bottom :size 0.33 :select t)

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

(defun my-haskell-compile-command ()
  "Nix-aware Haskell compile command"
  (let ((cabal-directory (haskell-cabal-find-dir)))
    (format
     "cd %s && echo Entering directory \\`$(pwd)\\' && if [ \"X$IN_NIX_SHELL\" == \"X\" ] ; then nix-shell --run 'cabal v2-build'; else cabal v2-build && cabal v2-test; fi"
     cabal-directory)))

(defun set-my-haskell-compile-command ()
  "Sets compile-command to a nix-aware one"
  (setq compile-command #'my-haskell-compile-command))

(add-hook 'haskell-mode-hook #'set-my-haskell-compile-command)

; (setq lsp-haskell-process-path-hie "hie-wrapper")

(add-hook! 'flycheck-mode-hook
  (message "Enabled flycheck-mode in %s" (current-buffer)))

(add-hook! 'haskell-mode-hook
  (message "Enabled haskell-mode in %s" (current-buffer)))

(add-hook! 'dante-mode-hook
  (message "Enabled dante-mode in %s" (current-buffer)))

(add-hook 'coq-mode-hook
          (lambda ()
            (set (make-local-variable 'prettify-symbols-alist)
                 '(("Admitted." . ?😱) ("admit." . ?😱)
  ("Alpha" . ?Α) ("Beta" . ?Β) ("Gamma" . ?Γ)
  ("Delta" . ?Δ) ("Epsilon" . ?Ε) ("Zeta" . ?Ζ)
  ("Eta" . ?Η) ("Theta" . ?Θ) ("Iota" . ?Ι)
  ("Kappa" . ?Κ) ("Lambda" . ?Λ) ("Mu" . ?Μ)
  ("Nu" . ?Ν) ("Xi" . ?Ξ) ("Omicron" . ?Ο)
  ("Pi" . ?Π) ("Rho" . ?Ρ) ("Sigma" . ?Σ)
  ("Tau" . ?Τ) ("Upsilon" . ?Υ) ("Phi" . ?Φ)
  ("Chi" . ?Χ) ("Psi" . ?Ψ) ("Omega" . ?Ω)
  ("alpha" . ?α) ("beta" . ?β) ("gamma" . ?γ)
  ("delta" . ?δ) ("epsilon" . ?ε) ("zeta" . ?ζ)
  ("eta" . ?η) ("theta" . ?θ) ("iota" . ?ι)
  ("kappa" . ?κ) ("lambda" . ?λ) ("mu" . ?μ)
  ("nu" . ?ν) ("xi" . ?ξ) ("omicron" . ?ο)
  ("pi" . ?π) ("rho" . ?ρ) ("sigma" . ?σ)
  ("tau" . ?τ) ("upsilon" . ?υ) ("phi" . ?φ)
  ("chi" . ?χ) ("psi" . ?ψ) ("omega" . ?ω)
  ))))

; (set-fontset-font t 'unicode (font-spec :name "Apple Color Emoji") nil 'append)

; (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend)

; (set-fontset-font "fontset-default" 'unicode (font-spec :name "Apple Color Emoji") nil 'prepend)
