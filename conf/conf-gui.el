;; Open Emacs in full screen
(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))

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
(setq frame-title-format "%b")

;; Highlight current line
(global-hl-line-mode 1)

(provide 'conf-gui)
