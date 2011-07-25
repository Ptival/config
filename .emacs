(load-library "cl") 
(setq lisp-indent-function 'common-lisp-indent-function)

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
 '(column-number-mode t)
 '(coq-time-commands nil)
 '(proof-allow-undo-in-read-only t)
 '(proof-delete-empty-windows t)
 '(proof-disappearing-proofs t)
 '(proof-keep-response-history t)
 '(proof-multiple-frames-enable nil)
 '(proof-script-fly-past-comments t)
 '(proof-shrink-windows-tofit t)
 '(proof-splash-enable nil)
 '(proof-three-window-enable t)
 '(proof-toolbar-enable nil)
 '(tool-bar-mode nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

(setq scroll-step 1)

(set-scroll-bar-mode 'right)

(setq-default show-trailing-whitespace t)

(require 'caml-font)
