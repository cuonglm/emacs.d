;; Use M-/ for auto complete, C-n, C-p to select
(global-set-key "\M-/" 'auto-complete)
(setq ac-use-menu-map t)
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)

;; Enable auto indent
(define-key global-map (kbd "RET") 'newline-and-indent)

;; C-h as backspace
(setf (global-key-binding (kbd "C-h")) (kbd "<backspace>"))
(global-set-key (kbd "C-M-h") 'backward-kill-word)

;; C-M-h for help
(global-set-key (kbd "C-z") 'help-command)

(provide 'conf-keys)
