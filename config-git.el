;;;; config-git.el ---- Git integration within emacs

(require 'magit)

(define-key mode-specific-map (kbd "g") 'magit-status)

(provide 'config-git)
