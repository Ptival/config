;;; private/ptival/config.el -*- lexical-binding: t; -*-

(setq
 doom-font (font-spec :family "Iosevka SS09" :size 20)
 ;; doom-font (font-spec :family "Iosevka SS09" :size 24)
 ;; not sure when this one gets used, so outrageous value to notice
 doom-big-font (font-spec :family "Iosevka SS09" :size 100))
(when (eq system-type 'gnu/linux)
  (setq doom-unicode-font (font-spec :family "Symbola" :size 20)))
(when (eq system-type 'darwin)
  (setq doom-unicode-font (font-spec :family "Apple Color Emoji" :size 20)))

;; Too slow on this computer to let flycheck go crazy at every keypress
(setq flycheck-check-syntax-automatically '(mode-enabled save))

;; prevent haskell-ghc from wanting to run
(add-hook! 'flycheck-mode-hook
  (add-to-list 'flycheck-disabled-checkers 'haskell-ghc))

;;;; lsp-ui settings ;;;;;
(add-hook! 'lsp-ui-doc-mode-hook
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-alignment 'window)
  ;(setq lsp-ui-doc-use-webkit t)
  (setq lsp-ui-doc-max-height 14)
  (setq lsp-ui-doc-max-width 100)
  (setq lsp-ui-doc-position 'top))

(setq doom-theme 'doom-tomorrow-night)
(setq fancy-splash-image "/home/val/.doom.d/doom-small.png")

;; Add hlint after intero
;; (with-eval-after-load 'intero
;;   (flycheck-add-next-checker 'intero '(warning . haskell-hlint)))

(require 'lsp)
(require 'lsp-haskell)
(add-hook 'haskell-mode-hook #'lsp)
(add-hook 'lsp-ui-mode-hook #'lsp-ui-doc-mode)


(use-package lsp-haskell
 :ensure t
 :config
 ;; (setq lsp-haskell-process-path-hie "hie")
 (setq lsp-haskell-process-path-hie "hie-wrapper")
 (lsp-haskell-set-formatter-ormolu)
 ;; TODO: once haskell-language-server becomes really good, this sets it up
 ;; (setq lsp-haskell-process-path-hie "haskell-language-server-wrapper")
 ;; Comment/uncomment this line to see interactions between lsp client/server.
 ;; (setq lsp-log-io t)
 )

;;; lorri is installed globally, so it lives in /run/current-system/sw/bin
;;; but this path is not in the default PATH passed to emacs. Adding it,
;;; which requires setting both exec-path and PATH in process-environment.
; (add-to-list 'exec-path "/run/current-system/sw/bin")
; (setenv "PATH" (concat (getenv "PATH") ":/run/current-system/sw/bin"))

;; for haskell-ide-engine, incompatible with haskell-language-server
;; (add-hook! 'haskell-mode-hook
;;   (setq lsp-haskell-process-args-hie (list "-d" "-l" "/tmp/hie.log" "-r" (haskell-cabal-find-dir))))

;; (add-hook 'dante-mode-hook
;;   '(lambda () (flycheck-add-next-checker 'haskell-dante '(warning . haskell-hlint))))

;; NOTE: Currently, dante does not handle cabal v1 correctly.
;; cf. https://github.com/jyp/dante/issues/141
;; The following snippet fixes the bare-cabal method.
;; You will also want to put a `.dir-locals.el` file in every top Haskell
;; directory, with contents like:
;; ((nil . ((dante-method . bare-cabal)
;;         (dante-target . "name-of-the-target-for-this-directory"))))
;; (after! dante
;;   (setq dante-methods-alist
;;         `((bare-cabal ,(lambda (d) (directory-files d t "..cabal$")) ("cabal" "v1-repl" dante-target "--builddir=dist/dante")))))

;; ;; I'm getting errors at start up about coq-prog-env not being defined, so...
;; (setq coq-prog-env process-environment)
;; ;;; For direnv and coq to play nicely, we must update `coq-prog-env` when the
;; ;;; direnv environment gets updated
(defun update-coq-prog-env (&rest _args)
  (setq coq-prog-env process-environment))
(add-hook! 'coq-mode-hook
  (advice-add 'direnv-update-directory-environment :after #'update-coq-prog-env))

;;; Sadly, exec-path gets overwritten at some point in the stack, so we
;;; overwrite it again just before calling scomint-exec-1
;;; This is necessary for dante to run correctly
(defun add-nix-path (orig-func &rest args)
  "Adds nix's PATH to exec-path for finding coqtop succesfully"
  (let
    ((exec-path (split-string (getenv "PATH") ":")))
    (apply orig-func args)))

(defun add-coq-prog-env (orig-func &rest args)
  "Adds nix's PATH to exec-path for finding coqtop succesfully"
  (let
    ((process-environment coq-prog-env))
    (apply orig-func args)))

;; If dante starts before the PATH is set up, it will fail to find cabal
;; (advice-add 'dante-start :before #'direnv-update-environment)

(add-hook 'post-command-hook #'direnv--maybe-update-environment)

; (advice-add 'scomint-exec-1 :around #'add-nix-path)
; (advice-add 'shell-command  :around #'add-nix-path)

(add-hook! 'coq-mode-hook
  (advice-add 'scomint-exec-1 :around #'add-coq-prog-env)
  (advice-add 'shell-command  :around #'add-coq-prog-env))

;; ;;; This is so emacs takes focus when started, on OSX
;; ; (x-focus-frame nil)
;; (add-hook! 'window-setup-hook (x-focus-frame nil))

;; ; DOES NOT WORK
;; ;(add-hook! 'haskell-mode-hook
;; ;  (setq lsp-haskell-process-wrapper-function
;; ;        (lambda (argv)
;; ;          (append argv (list "-r" (lsp-haskell--get-root))))))


;; (when (eq system-type 'darwin)
;;   (setq mac-control-modifier 'control)
;;   (setq mac-command-modifier 'meta)
;;   (setq mac-option-modifier  'alt)
;;   (define-key!
;;     [A-left]        #'left-word
;;     [A-right]       #'right-word
;;     [A-up]          #'evil-backward-paragraph
;;     [A-down]        #'evil-forward-paragraph
;;     ; This is mapped to M-\ by default, but let's also have it on A-\
;;     [A-\\]          #'delete-horizontal-space
;;     [A-backspace]   #'backward-kill-word
;;     [A-S-backspace] #'kill-word
;;     [A-delete]      #'kill-word
;;     [A-kp-delete]   #'kill-word
;;     [A-SPC]         #'just-one-space
;;     [A-backslash]   #'delete-horizontal-space
;;     [M-left]        #'doom/backward-to-bol-or-indent
;;     [M-right]       #'doom/forward-to-last-non-comment-or-eol
;;     [M-up]          #'evil-backward-paragraph
;;     [M-down]        #'evil-forward-paragraph
;;     [M-backspace]   #'doom/backward-kill-to-bol-and-indent
;;     [M-delete]      #'kill-line
;;     [M-kp-delete]   #'kill-line
;;     [M-v]           #'yank
;;     ; Coq commands (Mac or Kinesis?)
;;     ; [A-M-up]        #'proof-undo-last-successful-command
;;     ; [A-M-down]      #'proof-assert-next-command-interactive
;;     ; [A-M-right]     #'proof-goto-point
;;     ; Coq commands (Mac or Kinesis?)
;;     ; [A-C-up]        #'proof-undo-last-successful-command
;;     ; [A-C-down]      #'proof-assert-next-command-interactive
;;     ; [A-C-right]     #'proof-goto-point
;;     ))

(when (eq system-type 'gnu/linux)
  (define-key!
    ; Coq commands (X1 Carbon)
    [C-s-up]    #'proof-undo-last-successful-command
    [C-s-down]  #'proof-assert-next-command-interactive
    [C-s-right] #'proof-goto-point
    ))

;; ;;; Code:
;; (when (featurep! :feature evil)
;;   (load! "bindings"))

;; ; don't need this anymore now that ProofGeneral is in MELPA
;; ; (load "ProofGeneral/generic/proof-site")
;; (setq proof-three-window-mode-policy 'hybrid)
;; (setq proof-electric-terminator-enable nil) ; annoys me

;; ; ProofGeneral does abbreviation expansion (looks like auto-completion) of many
;; ; short words, which is extremely annoying in abbrev-mode.  Providing an empty
;; ; abbreviation table overrides this behavior.
;; (defun erase-coq-abbrev-table ()
;;     "Erase the Coq abbreviation table."
;;     (setq coq-mode-abbrev-table '()))
;; (erase-coq-abbrev-table)
;; (add-hook 'coq-mode-hook #'erase-coq-abbrev-table)
;; ; Disable the super annoying comment insertion behavior when pressing Enter
;; ; inside a Coq comment
;; (setq-hook! 'coq-mode-hook comment-line-break-function nil)
;; (setq-hook! 'coq-mode-hook coq-prefer-top-of-conclusion t)
;; (setq-hook! 'coq-mode-hook coq-highlighted-hyps-bg "dark violet")

;; ; prevents Evil from doing auto-completion when pressing Esc
;; ; (e.g. in ProofGeneral it's super annoying)
;; (setq evil-want-abbrev-expand-on-insert-exit nil)

; I really don't like how Haskell indents the line after a `let`, this prevents
; it, but also has some bad side-effects
(setq haskell-indentation-starter-offset 0)

; also align the qualified shorthand
(setq haskell-align-imports-pad-after-name t)

;; exactly one newline at the end of every file, unconditionally
(setq require-final-newline t)
(setq delete-trailing-lines t)

;; preserve double spaces after period when re-flowing text into 80 columns
(setq sentence-end-double-space t)

;; ; will display nice file type icons next to files
;; (setq +ivy-buffer-icons t)

;; ; this tells popups where to go, default tends to be bottom and small
;; (set-popup-rule! "^\\*intero:" :side 'bottom :size 0.4)
;; (set-popup-rule! "^\\*dante" :side 'bottom :size 0.4)
;; (set-popup-rule! "^\\*OCaml\\*$" :side 'bottom :size 0.4)
(set-popup-rule! "^\\*Flycheck errors\\*$" :side 'bottom :size 0.33 :select t)

;; ; deletes trailing whitespaces on every line upon saving files
(add-hook! 'before-save-hook 'delete-trailing-whitespace)

;; ; AUCTeX should ask us what the main .tex is, not guess
;; (setq-default TeX-master nil)

;; ; spellcheck when editing LaTeX files
;; (add-hook 'LaTeX-mode-hook #'turn-on-flyspell)

;; ; prevents spellcheck on LaTeX output buffers
;; (setq-hook! 'LaTeX-mode-hook +spellcheck-immediately nil)

;; Makes it so that the modeline looks shorter
;; like: ~/p/t/p/project/f/g/file
(setq doom-modeline-buffer-file-name-style
      'truncate-except-project)

;; (setq merlin-command "ocamlmerlin")

;; Start emacs maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(add-hook
 'coq-mode-hook
 (lambda ()
   (set
    (make-local-variable 'prettify-symbols-alist)
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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((dante-target . "codescape-cli")
     (dante-target . "server")
     (dante-repl-command-line "nix-shell" "--run"
                              (concat "cabal v2-repl "
                                      (or dante-target
                                          (dante-package-name)
                                          "")
                                      " --builddir=dist/dante"))
     (dante-target . "codescape")
     (haskell-align-imports-pad-after-name 't))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
