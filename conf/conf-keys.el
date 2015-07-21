;; Use M-/ for auto complete, C-n, C-p to select
(global-set-key "\M-/" 'auto-complete)
(setq ac-use-menu-map t)
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)

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

(provide 'conf-keys)
