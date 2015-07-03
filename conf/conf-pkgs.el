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
(setq jedi:complete-on-dot nil)
(defvar jedi:goto-stack '())
(defun jedi:jump-to-definition ()
  (interactive)
  (add-to-list 'jedi:goto-stack
               (point-marker))
  (jedi:goto-definition))
(defun jedi:jump-back ()
  (interactive)
  (let ((p (pop jedi:goto-stack)))
    (if p (progn
            (switch-to-buffer (marker-buffer p))
            (goto-char p)))))

(add-hook 'python-mode-hook
  (lambda ()
    ;; fill column indicator
    (require 'fill-column-indicator)
    (add-hook 'after-change-major-mode-hook 'fci-mode)
    (setq fci-rule-color "LightSlateBlue")
    (setq-default fci-rule-column 79)
    (setq fci-handle-truncate-lines t)
    ;; _ as a word separator
    (modify-syntax-entry ?_ "_" python-mode-syntax-table)
    ;; comment region function
    (local-set-key (kbd "C-c ;") 'comment-region)
    ;; python tab-width
    (setq python-indent-offset 4)
    (setq tab-width 4)
    (local-set-key (kbd "C-.") 'jedi:jump-to-definition)
    (local-set-key (kbd "C-,") 'jedi:jump-back)
    (local-set-key (kbd "C-c d") 'jedi:show-doc)))

(setq py-install-directory (concat "~/.emacs.d/.cask/" emacs-version "/elpa/python-mode-*"))
(require 'python-mode)
; use IPython
(setq-default py-shell-name "ipython")
(setq-default py-which-bufname "IPython")
(setq py-force-py-shell-name-p t)
(setq py-ipython-command-args "")
; switch to the interpreter after executing code
(setq py-shell-switch-buffers-on-execute-p t)
(setq py-switch-buffers-on-execute-p t)
; don't split windows
(setq py-split-windows-on-execute-p nil)
(add-hook 'python-mode-hook
  (lambda ()
    (local-set-key (kbd "C-c i") 'py-shell)))

;; flycheck
(require 'flycheck)
;; only check if the mode is enabled or the buffer was saved
(setq flycheck-check-syntax-automatically '(mode-enabled save))
(add-hook 'python-mode-hook 'flycheck-mode)
(add-hook 'cperl-mode-hook 'flycheck-mode)
(add-hook 'sh-mode-hook 'flycheck-mode)
(add-hook 'sh-mode-hook (lambda () (flycheck-select-checker 'sh-shellcheck)))

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
(setq ac-auto-show-menu t)
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
(add-hook 'cperl-mode-hook
          (lambda()
            (require 'perl-completion)
            (perl-completion-mode t)
            (global-set-key (kbd "C-M-p") 'plcmp-cmd-complete-all)
            (when (require 'auto-complete nil t) ; no error whatever auto-complete.el is not installed.
              (auto-complete-mode t)
              (make-variable-buffer-local 'ac-sources)
              (setq ac-sources
                    '(ac-source-perl-completion)))
            (setq cperl-indent-level 4
              cperl-close-paren-offset -4
              cperl-continued-statement-offset 4
              cperl-tab-always-indent t
              cperl-indent-parens-as-block t
              perl-indent-parens-as-block t)))

;; doc-view continuous mode
(setq doc-view-continuous t)

;; erc
(require 'erc)

;; sh-mode identation
(add-hook 'sh-mode-hook
  (lambda ()
    (setq sh-basic-offset 2)
    (setq sh-indentation 2)
    (setq tab-width 2)
    (define-key sh-mode-map (kbd "RET") 'reindent-then-newline-and-indent)))

;; lisp indent
(setq lisp-body-indent 2)

;; neotree
(require 'neotree)
(global-set-key [f1] 'neotree-toggle)

;; saveplace
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/saved-places")

;; projectile + helm
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(setq projectile-enable-caching t)

(provide 'conf-pkgs)
