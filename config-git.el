;;;; config-git.el ---- Git integration within emacs

(require 'magit)

(global-set-key (kbd "C-x C-g") 'magit-status)

(provide 'config-git)
