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
                         ("marmalade" . "https://marmalade-repo.org/packages/")
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
(mapc (lambda (pair)
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
(global-hl-line-mode t)

;; Show paren mode
(show-paren-mode t)

;; scroll-margin
(setq scroll-margin 5)

;; Default font
(set-frame-font "Terminus-13" t t)

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
  (exec-path-from-shell-initialize))

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
          perl6-mode-hook
          lua-mode-hook))
  (add-hook 'sh-mode-hook
            (lambda ()
              (flycheck-select-checker 'sh-shellcheck)))
  (add-hook 'go-mode-hook
            (lambda ()
              (flycheck-select-checker 'go-golint)
              (setq flycheck-disabled-checkers '(go-build)))))

;; flycheck-checkbashisms
(use-package flycheck-checkbashisms
  :ensure t
  :config
  (flycheck-checkbashisms-setup)
  (setq flycheck-checkbashisms-posix t))

;; flycheck-per6
(use-package flycheck-perl6
  :ensure t)

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
            `(lambda()
               (perl-completion-mode t)
               (when (require 'auto-complete nil t) ; no error whatever auto-complete.el is not installed.
                 (auto-complete-mode t)
                 (make-local-variable 'ac-sources)
                 (setq ac-sources '(ac-source-perl-completion)))
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
  :init (bind-key "C-x b" 'projectile-ibuffer)
  :config
  (use-package helm-projectile
    :ensure t
    :bind (("M-x" . helm-M-x)
           ("C-x C-f" . helm-find-files)))
  (setq helm-M-x-fuzzy-match t)
  (projectile-global-mode)
  (setq projectile-completion-system 'helm)
  (helm-projectile-on)
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

;; company-ghc
(use-package company-ghc
  :ensure t
  :config
  (custom-set-variables '(company-ghc-show-info t)))

;; company-tern
(use-package company-tern
  :ensure t
  :config
  (setq company-tern-meta-as-single-line t))

;; company
(use-package company
  :ensure t
  :diminish company-mode
  :bind ("M-/" . company-complete-common)
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (add-to-list 'company-backends '(company-ghc :with company-dabbrev-code))
  (mapc (lambda (pkg)
          (add-to-list 'company-backends pkg))
        '(company-c-headers
          company-ansible
          company-jedi
          company-go
          company-tern))
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
  (add-hook 'company-completion-cancelled-hook 'my/company-maybe-turn-on-fci))

;; elpy
(use-package elpy
  :ensure t
  :diminish auto-revert-mode
  :init
  (setq elpy-rpc-backend "jedi")
  (with-eval-after-load 'python (elpy-enable))
  :bind ("M-," . pop-tag-mark)
  :config
  (elpy-use-ipython)
  (add-hook 'elpy-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c M-c") 'elpy-shell-switch-to-shell))))

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
       (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-histor)
       (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history))))

;; go-mode
(use-package go-mode
  :ensure t
  :config
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook
            (lambda ()
              (local-set-key (kbd "M-.") 'godef-jump)
              (local-set-key (kbd "C-c C-c d") 'godoc-at-point)
              (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports))))

;; fill column indicator
(use-package fill-column-indicator
  :ensure t
  :config
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

;; markdown-mode
(use-package markdown-mode
  :ensure t
  :pin marmalade
  :mode "\\.md\\'"
  :config
  (add-hook 'markdown-mode-hook
            (lambda ()
              (when buffer-file-name
                (add-hook 'after-save-hook
                          'check-parens
                          nil t)))))

;; comment-dwim-2
(use-package comment-dwim-2
  :ensure t
  :bind ("M-;" . comment-dwim-2))

;; xlicense
(use-package xlicense
  :ensure t)

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

;; perspective
(use-package perspective
  :ensure t
  :config
  (persp-mode))

;; Perl6
(use-package perl6-mode
  :ensure t)

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

;; ghc
(use-package ghc
  :ensure t
  :config
  (autoload 'ghc-init "ghc" nil t)
  (autoload 'ghc-debug "ghc" nil t)
  (add-hook 'haskell-mode-hook (lambda () (ghc-init))))

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
  :config
  (add-hook 'js-mode-hook (lambda () (tern-mode t))))

;;;;;;;;;;;;;;;;;;;;
;; Hook Functions ;;
;;;;;;;;;;;;;;;;;;;;

;; sh-mode indentation
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

;; python indentation
(add-hook 'python-mode-hook
          (lambda ()
            (setq python-indent-offset 4)))

;; C indentation
(add-hook 'c-mode-hook
          (lambda ()
            (setq c-basic-offset 4)))

(provide 'init)
;;; init.el ends here

;; coding: utf-8
