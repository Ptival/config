;;; private/ptival/init.el

(setq delete-trailing-lines t)
(setq require-final-newline t)
(setq sentence-end-double-space t)
(set! :popup "^\\*intero:" :regexp t :align 'right :size 100)
(set-mouse-color "red")
(setq +ivy-buffer-icons t)

(add-hook! 'before-save-hook 'delete-trailing-whitespace)
