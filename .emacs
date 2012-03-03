;; Load ProofGeneral
(load "/usr/share/emacs/site-lisp/ProofGeneral/generic/proof-site.el")

(load-library "cl")
(setq lisp-indent-function 'common-lisp-indent-function)

(global-whitespace-mode 1)
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-midnight)))

(column-number-mode 1)

(global-set-key (kbd "C-c <C-right>") (kbd "C-c <C-return>"))
(global-set-key (kbd "C-c <C-down>") (kbd "C-c C-n"))
(global-set-key (kbd "C-c <C-up>") (kbd "C-c C-u"))
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(agda2-include-dirs (quote ("/home/varobert/agda/lib-0.6/src" ".")))
 '(column-number-mode t)
 '(coq-one-command-per-line nil)
 '(coq-time-commands nil)
 '(inhibit-startup-screen t)
 '(proof-allow-undo-in-read-only t)
 '(proof-delete-empty-windows t)
 '(proof-disappearing-proofs nil)
 '(proof-keep-response-history t)
 '(proof-multiple-frames-enable nil)
 '(proof-script-fly-past-comments t)
 '(proof-shrink-windows-tofit t)
 '(proof-splash-enable nil)
 '(proof-three-window-enable t)
 '(proof-toolbar-enable nil)
 '(tool-bar-mode nil)
 '(whitespace-line-column 80)
 '(whitespace-space (quote whitespace-space))
 '(whitespace-style (quote (face tabs trailing lines space-before-tab empty space-after-tab tab-mark))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "grey85" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "unknown" :family "unifont")))))

(setq scroll-step 1
      scroll-conservatively 10000)

(set-scroll-bar-mode 'right)

;; Removes annoying messages upon leaving
(setq save-abbrevs 'silently)

(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))
(setq agda2-highlight-face-groups 'default-faces)

(require 'evil)
(evil-mode 1)

(setq auto-mode-alist (cons '("\\.ml[iylp]?$" . caml-mode) auto-mode-alist))
(autoload 'caml-mode "caml" "Major mode for editing Caml code." t)
(autoload 'run-caml "inf-caml" "Run an inferior Caml process." t)
(add-to-list 'auto-mode-alist '("\\.ml[iylp]?" . tuareg-mode))
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)
(setq-default indent-tabs-mode nil)



;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TypeRex configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Loading TypeRex mode for OCaml files
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp")
(add-to-list 'auto-mode-alist '("\\.ml[iylp]?" . typerex-mode))
(add-to-list 'interpreter-mode-alist '("ocamlrun" . typerex-mode))
(add-to-list 'interpreter-mode-alist '("ocaml" . typerex-mode))
(autoload 'typerex-mode "typerex" "Major mode for editing Caml code" t)

;; TypeRex mode configuration
(setq ocp-server-command "/usr/local/bin/ocp-wizard")
(setq-default indent-tabs-mode nil)

;; Uncomment to enable typerex command menu by right click
;;(setq ocp-menu-trigger [mouse-3])

;; Uncomment to make new syntax coloring look almost like Tuareg
;;(setq ocp-theme "tuareg_like")
;; Uncomment to disable new syntax coloring and use Tuareg one
;;(setq ocp-theme "tuareg")
;; Uncomment to disable syntax coloring completely
;;(setq ocp-syntax-coloring nil)

;; TypeRex currently uses the Tuareg indentation mechanism. To get a result
;; closer to the OCaml programming guidelines described at
;; http://caml.inria.fr/resources/doc/guides/guidelines.en.html
;; Some users prefer to indent slightly less, as
;;(setq typerex-let-always-indent nil)
;;(setq typerex-with-indent 0)
;;(setq typerex-function-indent 0)
;;(setq typerex-fun-indent 0)
;; Another reasonable choice regarding if-then-else is:
;;(setq typerex-if-then-else-indent 0)

;;;; Auto completion (experimental)
;;;; Don't use M-x invert-face default with auto-complete! (emacs -r is OK)
;;(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/auto-complete-mode")
;;(setq ocp-auto-complete t)

;;;; Using <`> to complete whatever the context, and <C-`> for `
;;(setq auto-complete-keys 'ac-keys-backquote-backslash)
;;;; Options: nil (default), 'ac-keys-default-start-with-c-tab, 'ac-keys-two-dollar
;;;; Note: this overrides individual auto-complete key settings

;;;; I want immediate menu pop-up
;;(setq ac-auto-show-menu 0.)
;;;; Short delay before showing help
;;(setq ac-quick-help-delay 0.3)
;;;; Number of characters required to start (nil to disable)
;;(setq ac-auto-start 0)

;;;; Uncomment to enable auto complete mode globally (independently of OCaml)
;;(require 'auto-complete-config)
;;(add-to-list 'ac-dictionary-directories "/usr/local/share/emacs/site-lisp/auto-complete-mode/ac-dict")
;;(ac-config-default)

;; For debugging only
;;;;(setq ocp-debug t)
;;;;(setq ocp-profile t)
;;;;(setq ocp-dont-catch-errors t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End of TypeRex configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
