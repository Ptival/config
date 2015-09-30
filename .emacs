;;(require 'color-theme)
;;(color-theme-initialize)

;;(add-to-list 'custom-theme-load-path "/usr/share/emacs/etc/themes")
;;(color-theme-solarized-dark)

;;(require 'rainbow-delimiters)

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (package-initialize)
  )

(load "/usr/share/emacs/site-lisp/ProofGeneral/generic/proof-site.el")

;;(load "/home/vrobert/install/ssreflect-1.5.coq85beta2/pg-ssr.el")

(setq backup-directory-alist
  `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
  `((".*" ,temporary-file-directory t)))

(global-set-key (kbd "C-c <C-right>") (kbd "C-c <C-return>"))
(global-set-key (kbd "C-c <C-down>") (kbd "C-c C-n"))
(global-set-key (kbd "C-c <C-up>") (kbd "C-c C-u"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(agda2-include-dirs
   (quote
    ("." "/home/vrobert/install/agda/agda-stdlib-0.9/src")))
 '(column-number-mode t)
 '(coq-load-path (quote ("/home/vrobert/kraken/reflex/coq")))
 '(coq-one-command-per-line nil)
 '(coq-prog-args
   (quote
    ("-emacs-U" "-R" "/usr/local/lib/coq/user-contrib/Ssreflect/" "Ssreflect" "-R" "/usr/local/lib/coq/user-contrib/MathComp/" "MathComp" "-impredicative-set")))
 '(coq-unicode-tokens-enable nil)
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" default)))
 '(electric-indent-mode nil)
 '(fill-column 80)
 '(global-visual-line-mode t)
 '(haskell-process-type (quote cabal-repl))
 '(inhibit-startup-screen t)
 '(latex-run-command "pdflatex")
 '(menu-bar-mode nil)
 '(proof-allow-undo-in-read-only t)
 '(proof-colour-locked (quote red))
 '(proof-disappearing-proofs nil)
 '(proof-script-fly-past-comments t)
 '(proof-shrink-windows-tofit t)
 '(proof-splash-enable nil)
 '(proof-three-window-enable t)
 '(scroll-bar-mode nil)
 '(show-paren-mode nil)
 '(show-trailing-whitespace t)
 '(tool-bar-mode nil)
 '(tuareg-skip-after-eval-phrase nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "WhiteSmoke" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 100 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))
 '(agda2-highlight-datatype-face ((t (:foreground "deep sky blue"))))
 '(agda2-highlight-function-face ((t (:foreground "deep sky blue"))))
 '(agda2-highlight-module-face ((t (:foreground "magenta"))))
 '(agda2-highlight-primitive-type-face ((t (:foreground "deep sky blue"))))
 '(agda2-highlight-record-face ((t (:foreground "deep sky blue"))))
 '(font-lock-function-name-face ((t (:foreground "mediumspringgreen" :weight bold :height 1.0))))
 '(highlight ((t (:background "dark gray" :foreground "Old Lace"))))
 '(mode-line-buffer-id ((t (:background "black" :foreground "indian red" :family "Monaco")))))

(setq scroll-step 1
      scroll-conservatively 10000)

;; Agda

(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))
(setq agda2-highlight-face-groups 'default-faces)

;; Haskell

(add-to-list 'load-path "/usr/share/emacs/site-lisp/haskell-mode/")
(require 'haskell-mode-autoloads)
(add-to-list 'Info-default-directory-list "/usr/share/emacs/site-lisp/haskell-mode/")
(add-hook 'haskell-mode-hook
          (lambda ()
            (turn-on-haskell-decl-scan)
            (turn-on-haskell-doc)
            (turn-on-haskell-indentation)))

;;(require 'bind-key)
;;(bind-key "C-c C-l" 'haskell-process-load-file haskell-mode-map)
;;(bind-key "C-c C-t" 'haskell-process-do-type   haskell-mode-map)
;;(bind-key "C-c C-i" 'haskell-process-do-info   haskell-mode-map)
;;(bind-key "SPC" 'haskell-mode-contextual-space haskell-mode-map)

;; OCaml

(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)
(setq auto-mode-alist
      (append '(("\\.ml[ily]?$" . tuareg-mode)
                ("\\.topml$" . tuareg-mode))
              auto-mode-alist))

(setq haskell-process-log t)

;; no tabs thanks
(setq-default indent-tabs-mode nil)

;; nice completion bro
(global-set-key (kbd "<C-return>") 'dabbrev-expand)

(setq column-number-mode t)

;;(require 'whitespace)
;;(setq whitespace-style '(face empty tabs lines-tail trailing))
;;(global-whitespace-mode t)

;;(load-theme 'solarized t)
;;(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

