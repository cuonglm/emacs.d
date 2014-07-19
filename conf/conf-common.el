;; Store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Enable recent files mode
(require 'recentf)
(recentf-mode t)

;; Disable tab
(setq-default indent-tabs-mode nil)

;; Change yes-no to y-n
(fset 'yes-or-no-p 'y-or-n-p)

;; cperl-mode is preferred to perl-mode
(mapc
     (lambda (pair)
       (if (eq (cdr pair) 'perl-mode)
           (setcdr pair 'cperl-mode)))
     (append auto-mode-alist interpreter-mode-alist))

(provide 'conf-common)
