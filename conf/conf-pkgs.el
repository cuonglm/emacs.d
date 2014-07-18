;; Use Cask as Package Manager
;; https://github.com/cask/cask
(require 'cask "~/.cask/cask.el")
(cask-initialize)

;; Sync Cask file with package install via M-x list-package
;; https://github.com/rdallasgray/pallet
(require 'pallet)

;; Use Solarized theme
(load-theme 'solarized-light t)

;; Jedi
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;; flycheck
(require 'flycheck)
;; only check if the mode is enabled or the buffer was saved
(setq flycheck-check-syntax-automatically '(mode-enabled save))
(add-hook 'python-mode-hook 'flycheck-mode)
(add-hook 'perl-mode-hook 'flycheck-mode)
(add-hook 'sh-mode-hook 'flycheck-mode)

;; auto-complete
(require 'auto-complete-config)
(ac-config-default)
(setq ac-auto-show-menu nil)
(setq ac-ignore-case nil)

;; use C-n, C-p to select
;;(setq ac-use-menu-map t)
;;(define-key ac-menu-map "\C-n" 'ac-next)
;;(define-key ac-menu-map "\C-p" 'ac-previous)

;; undo-tree
(global-undo-tree-mode)

(require 'ack-and-a-half)

(provide 'conf-pkgs)
