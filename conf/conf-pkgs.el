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
(add-hook 'cperl-mode-hook 'flycheck-mode)
(add-hook 'sh-mode-hook 'flycheck-mode)

;; auto-complete
(require 'auto-complete-config)
;; dirty fix for having AC everywhere
(define-globalized-minor-mode real-global-auto-complete-mode
  auto-complete-mode (lambda ()
                       (if (not (minibufferp (current-buffer)))
                         (auto-complete-mode 1))))

(real-global-auto-complete-mode t)
(ac-config-default)
(global-auto-complete-mode t)
(auto-complete-mode t)
(setq ac-auto-show-menu 1)
(setq ac-ignore-case nil)

;; use M-n, M-p to select
(setq ac-use-menu-map t)
(define-key ac-menu-map "\M-n" 'ac-next)
(define-key ac-menu-map "\M-p" 'ac-previous)

;; undo-tree
(global-undo-tree-mode)

;; ack
(require 'ack-and-a-half)

;; perl-completion
(require 'perl-completion)
(global-set-key (kbd "C-M-p") 'plcmp-cmd-complete-all)

(add-hook 'cperl-mode-hook
          (lambda()
            (require 'perl-completion)
            (perl-completion-mode t)))

(add-hook 'cperl-mode-hook
          (lambda ()
            (when (require 'auto-complete nil t) ; no error whatever auto-complete.el is not installed.
              (auto-complete-mode t)
              (make-variable-buffer-local 'ac-sources)
              (setq ac-sources
                    '(ac-source-perl-completion)))))

(provide 'conf-pkgs)
