;; Open Emacs in full screen
(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))

;; Don't show startup screen
(setq inhibit-startup-screen t)

;; Remove scratch message
(setq initial-scratch-message "")

;; Show column
(setq column-number-mode t)

;; turn off scrollbar
(scroll-bar-mode -1)

;; turn off menubar
(menu-bar-mode -1)

;; turn off toolbar
(tool-bar-mode -1)

;; set tab width 4 spaces
(setq-default tab-width 4)

;; Show keystrokes
(setq echo-keystrokes 0.02)

(provide 'conf-gui)
