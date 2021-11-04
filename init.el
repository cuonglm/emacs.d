;;; init.el --- CuongLM personal emacs configuration

;;; Commentary:

;; To use this init.el, you need to install all dependencies first:
;;
;;    $ sudo make deps
;;    $ sh install_global.sh
;;

;;; Code:

;; package.el for packages manager
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("elpy" . "https://jorgenschaefer.github.io/packages/")))
(package-initialize)

;; Make sure use-package (https://github.com/jwiegley/use-package) installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'bind-key)

;;;;;;;;;;;;;;;;;;;
;; Common config ;;
;;;;;;;;;;;;;;;;;;;

(desktop-save-mode t)

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
(if (version< emacs-version "25.1")
    (setq x-select-enable-clipboard t)
  (setq select-enable-clipboard t))

;; cperl-mode is preferred to perl-mode
(mapc (lambda (pair)
        (if (eq (cdr pair) 'perl-mode)
            (setcdr pair 'cperl-mode)))
      (append auto-mode-alist interpreter-mode-alist))

;; custom file
(setq custom-file "~/.emacs.d/custom.el")
(when (not (file-exists-p custom-file))
  (with-temp-buffer (write-file custom-file)))
(load custom-file)

;; dired for osx
(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))

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
(global-hl-line-mode t)

;; Show paren mode
(show-paren-mode t)

;; scroll-margin
(setq scroll-margin 5)

;; Default font
(if (eq system-type 'darwin)
  (set-frame-font "Terminus \(TTF\)-13" t t)
(set-frame-font "Terminus-13" t t))

;;;;;;;;;;;;;;;;;
;; Key binding ;;
;;;;;;;;;;;;;;;;;

;; Enable auto indent
(define-key global-map (kbd "RET") 'newline-and-indent)

;; C-h as backspace
(setf (global-key-binding (kbd "C-h")) (kbd "<backspace>"))

;; C-w to kill word backward
(global-set-key (kbd "C-w") 'backward-kill-word)

;; C-x C-w to kill region
(global-set-key (kbd "C-x C-w") 'kill-region)

;; C-c h for help command
(global-set-key (kbd "C-c h") 'help-command)

;; C-x C-b as ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Indent region or buffer
(defun my/indent-region-or-buffer ()
  "Indent region if active, otherwise buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (indent-region (region-beginning) (region-end))
      (indent-region (point-min) (point-max)))))

(global-set-key (kbd "C-M-\\") 'my/indent-region-or-buffer)

;;;;;;;;;;;;;;;;;;;;;
;; Packages config ;;
;;;;;;;;;;;;;;;;;;;;;

;; diminish
(use-package diminish
  :ensure t)

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

;; exec-path-from-shell
(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-shell-name "zsh")
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH"))

;; helm
(use-package helm
  :ensure t
  :config (progn
            (setq  helm-gtags-ignore-case t)
            (setq  helm-gtags-auto-update t)
            (setq  helm-gtags-use-input-at-cursor t)
            (setq  helm-gtags-pulse-at-cursor t)
            (setq  helm-gtags-prefix-key "\C-cg")
            (setq  helm-gtags-suggested-key-mapping t)))

(use-package helm-gtags
  :ensure t
  :config (progn
            (add-hook 'c-mode-hook 'helm-gtags-mode)
            (add-hook 'c++-mode-hook 'helm-gtags-mode)
            (add-hook 'asm-mode-hook 'helm-gtags-mode)
            (define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
            (define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
            (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
            (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
            (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
            (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)))

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
  (mapc (lambda (mode)
          (add-hook mode 'flycheck-mode))
        '(python-mode-hook
          cperl-mode-hook
          sh-mode-hook
          go-mode-hook
          c-mode-hook
          c++-mode-hook
          emacs-lisp-mode-hook
          php-mode-hook
          raku-mode-hook
          lua-mode-hook
          rustic-mode-hook))
  (add-hook 'sh-mode-hook
            (lambda ()
              (flycheck-select-checker 'sh-shellcheck)))
  (add-hook 'go-mode-hook
            (lambda ()
              (flycheck-select-checker 'go-golint)
              (setq flycheck-disabled-checkers '(go-build go-vet)))))

;; flycheck-checkbashisms
(use-package flycheck-checkbashisms
  :ensure t
  :config
  (flycheck-checkbashisms-setup)
  (setq flycheck-checkbashisms-posix t))

;; flycheck-raku
(use-package flycheck-raku
  :ensure t)

;; flycheck-rust
(use-package flycheck-rust
  :ensure t
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; undo-tree
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode))

;; perl-completion
;; (use-package perl-completion
;;   :ensure t
;;   :bind ("C-M-p" . plcmp-cmd-complete-all)
;;   :config
;;   (add-hook 'cperl-mode-hook
;;             `(lambda()
;;                (perl-completion-mode t)
;;                (when (require 'auto-complete nil t) ; no error whatever auto-complete.el is not installed.
;;                  (auto-complete-mode t)
;;                  (make-local-variable 'ac-sources)
;;                  (setq ac-sources '(ac-source-perl-completion)))
;;                (setq cperl-indent-level 4
;;                      cperl-close-paren-offset -4
;;                      cperl-continued-statement-offset 4
;;                      cperl-tab-always-indent t
;;                      cperl-indent-parens-as-block t
;;                      perl-indent-parens-as-block t))))

;; neo tree
(use-package neotree
  :ensure t
  :bind ([f2] . neotree-toggle))

;; projectile + helm
(use-package projectile
  :ensure t
  :init (bind-key "C-x b" 'projectile-ibuffer)
  :config
  (use-package helm-projectile
    :ensure t
    :bind (("M-x" . helm-M-x)
           ("C-x C-f" . helm-find-files)
           ("C-c h m" . helm-man-woman)
           ("C-c p s a" . helm-projectile-ack)))
  (setq helm-M-x-fuzzy-match t)
  (projectile-mode)
  (setq projectile-completion-system 'helm)
  (helm-projectile-on)
  (setq projectile-enable-caching t))

;; magit
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-x M-g". magit-dispatch))
  :config
  (remove-hook 'server-switch-hook 'magit-commit-diff)
  (setq vc-handled-backends (delq 'Git vc-handled-backends)))

;; company-jedi
(use-package company-jedi
  :ensure t)

;; company-ansible
(use-package company-ansible
  :ensure t)

;; company-c-headers
(use-package company-c-headers
  :ensure t)

;; company-coq
(use-package company-coq
  :ensure t)

(use-package company-lua
  :ensure t)

;; company
(use-package company
  :ensure t
  :diminish company-mode
  :bind ("M-/" . company-complete-common)
  :config (progn
            (add-hook 'after-init-hook 'global-company-mode)
            (mapc (lambda (pkg)
                    (add-to-list 'company-backends pkg))
                      '(company-c-headers
                        company-ansible
                        company-jedi
                        company-coq
                        company-lua))
            ;; Workaround for working with fci-mode
            (defvar-local company-fci-mode-on-p nil)

            (defun my/company-turn-off-fci (&rest ignore)
              (when (boundp 'fci-mode)
                (setq company-fci-mode-on-p fci-mode)
                (when fci-mode (fci-mode -1))))

            (defun my/company-maybe-turn-on-fci (&rest ignore)
              (when company-fci-mode-on-p (fci-mode 1)))

            (add-hook 'company-completion-started-hook 'my/company-turn-off-fci)
            (add-hook 'company-completion-finished-hook 'my/company-maybe-turn-on-fci)
            (add-hook 'company-completion-cancelled-hook 'my/company-maybe-turn-on-fci)))

;; elpy
(use-package elpy
  :ensure t
  :diminish auto-revert-mode
  :init
  (setq elpy-rpc-backend "jedi")
  (with-eval-after-load 'python (elpy-enable))
  :bind ("M-," . pop-tag-mark)
  :config
  (add-hook 'elpy-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c M-c") 'elpy-shell-switch-to-shell)
              (setq python-shell-interpreter "ipython"
                    python-shell-interpreter-args "-i --simple-prompt"))))

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

;; Alchemist
(use-package alchemist
  :ensure t)

;; helm-gtags
(use-package helm-gtags
  :ensure t
  :diminish helm-gtags-mode
  :config
  (setq helm-gtags-ignore-case t
        helm-gtags-auto-update t
        helm-gtags-use-input-at-cursor t
        helm-gtags-pulse-at-cursor t
        helm-gtags-prefix-key "\C-cg"
        helm-gtags-suggested-key-mapping t)
  (add-hook 'c-mode-hook 'helm-gtags-mode)
  (add-hook 'c++-mode-hook 'helm-gtags-mode)
  (eval-after-load 'helm-gtags
    '(progn
       (define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
       (define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
       (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
       (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
       (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
       (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history))))

;; go-mode
(use-package go-mode
  :ensure t
  :hook (progn
          (go-mode . lsp-deferred))
  :config (progn
            (defun lsp-go-install-save-hooks ()
              (add-hook 'before-save-hook #'lsp-format-buffer t t)
              (add-hook 'before-save-hook #'lsp-organize-imports t t))
            (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)))

;; go-guru
(use-package go-guru
  :ensure t
  :demand t)

;; gotest
(use-package gotest
  :ensure t)

;; go-tag
(use-package go-tag
  :ensure t)

;; go-dlv
(use-package go-dlv
  :ensure t)

;; fill column indicator
(use-package fill-column-indicator
  :ensure t
  :config
  (eval-when-compile (require 'cl))
  (setq fci-rule-color "LightSlateBlue")
  (setq fci-handle-truncate-lines t)
  (defun my/fci-config (mode num)
    (lexical-let ((_num num))
      (add-hook mode (lambda ()
                       (progn
                         (setq fci-rule-column _num)
                         (fci-mode t))))))
  (let (mode-config-hash)
    (setq mode-config-hash (make-hash-table :test 'equal))
    (puthash 'python-mode-hook 79 mode-config-hash)
    (puthash 'c-mode-hook 80 mode-config-hash)
    (puthash 'cperl-mode-hook 80 mode-config-hash)
    (maphash (lambda (k v) (my/fci-config k v)) mode-config-hash)))

;; ws-butler
(use-package ws-butler
  :ensure t
  :diminish ws-butler-mode
  :config
  (setq ws-butler-keep-whitespace-before-point nil)
  (mapc (lambda (mode)
          (add-hook mode 'ws-butler-mode))
        '(prog-mode-hook
          yaml-mode-hook
          jinja2-mode-hook)))

;; comment-dwim-2
(use-package comment-dwim-2
  :ensure t
  :bind ("M-;" . comment-dwim-2))

;; web-mode
(use-package web-mode
  :ensure t
  :mode ("\\.html?\\'" "\\.phtml\\'" "\\.tpl\\.php\\'"))

;; elisp-slime-nav
(use-package elisp-slime-nav
  :ensure t
  :diminish elisp-slime-nav-mode
  :config
  (mapc (lambda (mode)
          (add-hook mode 'elisp-slime-nav-mode))
        '(emacs-lisp-mode-hook
          ielm-mode-hook)))

;; Raku
(use-package raku-mode
  :ensure t
  :defer t)

;; helm-c-yasnippet
(use-package helm-c-yasnippet
  :ensure t
  :bind ("C-c y" . helm-yas-complete))

;; dockerfile-mode
(use-package dockerfile-mode
  :ensure t)

;; haskell-mode
(use-package haskell-mode
  :ensure t
  :config
  (defun my/haskell-mode-hoogle-at-point ()
    "Show Hoogle documentation for the indentifier at POINT."
    (interactive)
    (haskell-hoogle (thing-at-point 'word) t))
  (eval-after-load 'haskell-mode
    '(progn
       (define-key haskell-mode-map [f8] 'haskell-navigate-imports)
       (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
       (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
       (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
       (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
       (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
       (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)
       (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)
       (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-jump-to-def-or-tag)
       (define-key haskell-mode-map (kbd "C-c d") 'my/haskell-mode-hoogle-at-point)))
  (eval-after-load 'haskell-cabal
    '(progn
       (define-key haskell-cabal-mode-map (kbd "C-c C-s") 'haskell-interactive-switch)
       (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
       (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
       (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))
  (custom-set-variables
   '(haskell-tags-on-save t)
   '(haskell-stylish-on-save t)
   '(haskell-process-suggest-remove-import-lines t)
   '(haskell-process-auto-import-loaded-modules t)
   '(haskell-process-log t)
   '(haskell-process-type 'cabal-repl)))

;; shm
(use-package shm
  :ensure t
  :config
  (add-hook 'haskell-mode-hook 'structured-haskell-mode)
  (set-face-background 'shm-current-face "#eee8d5")
  (set-face-background 'shm-quarantine-face "lemonchiffon"))

;; hindent
(use-package hindent
  :ensure t
  :diminish hindent-mode
  :config
  (defun my/hindent-reformat-region-or-buffer ()
    "Reformat region if active, otherwise buffer."
    (interactive)
    (save-excursion
      (if (region-active-p)
          (hindent-reformat-region (region-beginning) (region-end))
        (hindent-reformat-buffer))))
  (add-hook 'haskell-mode-hook 'hindent-mode)
  (eval-after-load 'haskell-mode
    '(progn
       (define-key haskell-mode-map (kbd "C-M-\\") 'my/hindent-reformat-region-or-buffer)))
  (custom-set-variables
   '(hindent-style "johan-tibell")))

;; ansible-doc
(use-package ansible-doc
  :ensure t
  :diminish ansible-doc-mode
  :config
  (add-hook 'yaml-mode-hook #'ansible-doc-mode))

;; lua-mode
(use-package lua-mode
  :ensure t)

;; tern-mode
(use-package tern
  :ensure t
  :diminish tern-mode
  :config
  (add-hook 'js-mode-hook (lambda () (tern-mode t))))

;; smartparens
(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config
  (sp-with-modes
   '(c-mode c++-mode)
   (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
   (sp-local-pair "/*" "*/" :post-handlers '((" | " "SPC") ("* ||\n[i]" "RET")))))

;; cc-mode
(use-package cc-mode
  :ensure t
  :commands c-mode
  :config
  (add-hook
   'c-mode-hook
   (lambda ()
     (setq c-basic-offset 4)
     (setq c-default-style "gnu")
     (show-smartparens-global-mode t)
     (smartparens-global-mode t))))

;; semantic-stickyfunc-enhance
(use-package stickyfunc-enhance
  :ensure t
  :config
  (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)
  (semantic-mode t))

;; js2-mode
(use-package js2-mode
  :ensure t
  :mode "\\.js\\'")

;; ag.el
(use-package ag
  :ensure t
  :config
  (setq ag-highlight-search t)
  (setq ag-reuse-buffers 't))

;; rustic
(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t)))

;; direnv
(use-package direnv
  :ensure t
  :config
  (direnv-mode t))

;; diminish
(diminish 'auto-revert-mode)

;; pipenv
(use-package pipenv
  :ensure t
  :hook (python-mode . pipenv-mode)
  :init
  (setq
   pipenv-projectile-after-switch-function
   #'pipenv-projectile-after-switch-extended))

;; protobuf
(use-package protobuf-mode
  :ensure t
  :config
  (defconst my-protobuf-style
    '((c-basic-offset . 2)
      (indent-tabs-mode . nil)))
  (add-hook 'protobuf-mode-hook
    (lambda () (c-add-style "my-style" my-protobuf-style t))))

;; Coq
(use-package proof-general
  :ensure t
  :config (progn
            (setq coq-compile-before-require t)
            (add-hook 'coq-mode-hook #'company-coq-mode)))

;; lsp-mode
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (go-mode . lsp-deferred)
  :custom
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  (lsp-rust-analyzer-server-display-inlay-hints t)
  :config (progn
            (setq lsp-prefer-flymake nil)
            (setq lsp-rust-server 'rust-analyzer)
            (add-hook 'lsp-mode-hook 'lsp-ui-mode)
            (lsp-register-custom-settings
              '(("gopls.completeUnimported" t t)
                ("gopls.usePlaceholders" t t)))))

;; lsp-ui
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil)
  :config (progn
            (setq lsp-ui-sideline-enable nil)))

;;;;;;;;;;;;;;;;;;;;
;; Hook Functions ;;
;;;;;;;;;;;;;;;;;;;;

;; sh-mode
(add-hook 'sh-mode-hook
          (lambda ()
            (setq sh-basic-offset 2)
            (setq tab-width 2)
            (define-key sh-mode-map (kbd "RET") 'reindent-then-newline-and-indent)))

;; lisp
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq lisp-body-indent 2)))

;; python
(add-hook 'python-mode-hook
          (lambda ()
            (setq python-indent-offset 4)))

(provide 'init)
;;; init.el ends here

;; coding: utf-8
