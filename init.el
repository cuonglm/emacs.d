;; package.el for packages manager
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "http://melpa.milkbox.net/packages/")
                         ("elpy" . "https://jorgenschaefer.github.io/packages/")))
(package-initialize)

;; Make sure use-package (https://github.com/jwiegley/use-package) installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'bind-key)
(require 'diminish)

;;;;;;;;;;;;;;;;;;;
;; Common config ;;
;;;;;;;;;;;;;;;;;;;

;; Store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Change yes-no to y-n
(fset 'yes-or-no-p 'y-or-n-p)

;; Disable tab
(setq-default indent-tabs-mode nil)

;; Enable clipboard
(setq x-select-enable-clipboard t)

;; cperl-mode is preferred to perl-mode
(mapc
 (lambda (pair)
   (if (eq (cdr pair) 'perl-mode)
       (setcdr pair 'cperl-mode)))
 (append auto-mode-alist interpreter-mode-alist))

;;;;;;;;;;;;;;;;
;; GUI config ;;
;;;;;;;;;;;;;;;;

;; Open Emacs in full screen
(custom-set-variables '(initial-frame-alist (quote ((fullscreen . maximized)))))

;; Don't show startup screen
(setq inhibit-startup-screen t)

;; Remove scratch message
(setq initial-scratch-message "")

;; Show column
(setq column-number-mode t)

;; Turn off scrollbar
(scroll-bar-mode -1)

;; Turn off menubar
(menu-bar-mode -1)

;; Turn off toolbar
(tool-bar-mode -1)

;; Set tab width 4 spaces
(setq-default tab-width 4)

;; Show keystrokes
(setq echo-keystrokes 0.02)

;; Show buffer name in title bar
(setq frame-title-format "%f")

;; Highlight current line
(global-hl-line-mode 1)

;;;;;;;;;;;;;;;;;
;; Key binding ;;
;;;;;;;;;;;;;;;;;

;; Enable auto indent
(define-key global-map (kbd "RET") 'newline-and-indent)

;; C-h as backspace
(setf (global-key-binding (kbd "C-h")) (kbd "<backspace>"))

;; C-w to kill word backward
(global-set-key (kbd "C-w") 'backward-kill-word)

;; C-c C-w to kill region
(global-set-key (kbd "C-x C-w") 'kill-region)

;; C-c h for help command
(global-set-key (kbd "C-c h") 'help-command)

;; C-x C-b as ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;;;;;;;;;;;;;;;;;;;;
;; Packages config ;;
;;;;;;;;;;;;;;;;;;;;;

;; Uniquify style
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'reverse))

;; Enable recent files mode
(use-package recentf
  :config
  (recentf-mode t))

;; Enable ido-mode
(use-package ido
  :config
  (ido-mode t))

;; saveplace
(use-package saveplace
  :config
  (setq-default save-place t)
  (setq save-place-file "~/.emacs.d/saved-places"))

;; doc-view
(use-package doc-view
  :config
  (setq doc-view-continuous t))

;; erc
(use-package erc)

;; anything
(use-package anything
  :ensure t)

;; Use Solarized theme
(use-package solarized-theme
  :ensure t
  :config
  (load-theme 'solarized-light t))

;; flycheck
(use-package flycheck
  :ensure t
  :init
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  :config
  (add-hook 'python-mode-hook 'flycheck-mode)
  (add-hook 'cperl-mode-hook 'flycheck-mode)
  (add-hook 'sh-mode-hook 'flycheck-mode)
  (add-hook 'sh-mode-hook (lambda () (flycheck-select-checker 'sh-shellcheck))))

;; undo-tree
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode))

;; ack
(use-package ack-and-a-half
  :ensure t)

;; perl-completion
(use-package perl-completion
  :ensure t
  :bind ("C-M-p" . plcmp-cmd-complete-all)
  :config
  (add-hook 'cperl-mode-hook
            (lambda()
              (perl-completion-mode t)
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
                    perl-indent-parens-as-block t))))

;; neo tree
(use-package neotree
  :ensure t
  :bind ([f2] . neotree-toggle))

;; projectile + helm
(use-package projectile
  :ensure t
  :config
  (use-package helm-projectile
    :ensure t
    :bind (("M-x" . helm-M-x)
           ("C-x C-f" . helm-find-files))
    :config
    (helm-projectile-on))
  (projectile-global-mode)
  (setq projectile-completion-system 'helm)
  (setq projectile-enable-caching t))

;; magit
(use-package magit
  :ensure t)

;; company-go
(use-package company-go
  :ensure t)

;; company-jedi
(use-package company-jedi
  :ensure t)

;; company-ansible
(use-package company-ansible
  :ensure t)

;; company-c-headers
(use-package company-c-headers
  :ensure t)

;; company
(use-package company
  :ensure t
  :diminish company-mode
  :bind ("M-." . company-complete-common)
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (add-to-list 'company-backends 'company-c-headers)
  (add-to-list 'company-backends 'company-ansible)
  (add-to-list 'company-backends 'company-jedi)
  (add-to-list 'company-backends 'company-go))

;; elpy
(use-package elpy
  :ensure t
  :init (with-eval-after-load 'python (elpy-enable))
  :bind ("C-c M-c" . elpy-shell-switch-to-shell)
  :config
  (elpy-use-ipython)
  ;; fill column indicator
  (use-package fill-column-indicator
    :ensure t
    :config
    (add-hook 'after-change-major-mode-hook 'fci-mode)
    (setq fci-rule-color "LightSlateBlue")
    (setq-default fci-rule-column 79)
    (setq fci-handle-truncate-lines t))
  ;; Use a stack for jump to/back definition
  (defvar elpy-goto-stack '())
  (defun elpy-jump-to-definition ()
    (interactive)
    (add-to-list 'elpy-goto-stack
                 (point-marker))
    (elpy-goto-definition))
  (defun elpy-jump-back ()
    (interactive)
    (let ((p (pop elpy-goto-stack)))
      (if p (progn
              (switch-to-buffer (marker-buffer p))
              (goto-char p)))))
  (add-hook 'elpy-mode-hook
            (lambda ()
              (local-set-key (kbd "M-.") 'elpy-jump-to-definition)
              (local-set-key (kbd "M-,") 'elpy-jump-back))))

;; jinja2
(use-package jinja2-mode
  :ensure t)

;; YAML
(use-package yaml-mode
  :ensure t)

;; Ansible
(use-package ansible
  :ensure t
  :config
  (add-hook 'yaml-mode-hook '(lambda () (ansible t))))

;; Elixir
(use-package elixir-mode
  :ensure t)

;; helm-gtags
(use-package helm-gtags
  :ensure t
  :bind (("C-c g a" . helm-gtags-tags-in-this-function)
         ("C-j" . helm-gtags-select)
         ("M-." . helm-gtags-dwim)
         ("M-," . helm-gtags-pop-stack)
         ("C-c <" . helm-gtags-previous-historu)
         ("C-c >" . helm-gtags-next-history))
  :config
  (setq helm-gtags-ignore-case t
        helm-gtags-auto-update t
        helm-gtags-use-input-at-cursor t
        helm-gtags-pulse-at-cursor t
        helm-gtags-prefix-key "\C-cg"
        helm-gtags-suggested-key-mapping t)
  (add-hook 'c-mode-hook 'helm-gtags-mode)
  (add-hook 'c++-mode-hook 'helm-gtags-mode))

;;;;;;;;;;;;;;;;;;;;
;; Hook Functions ;;
;;;;;;;;;;;;;;;;;;;;

;; sh-mode identation
(add-hook 'sh-mode-hook
  (lambda ()
    (setq sh-basic-offset 2)
    (setq sh-indentation 2)
    (setq tab-width 2)
    (define-key sh-mode-map (kbd "RET") 'reindent-then-newline-and-indent)))

;; lisp indentation
(add-hook 'emacs-lisp-mode-hook
  (lambda ()
    (setq lisp-body-indent 2)))
