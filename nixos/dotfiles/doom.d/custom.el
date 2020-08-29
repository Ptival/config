(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((dante-target . "chick")
     (dante-target . "codescape-cli")
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
