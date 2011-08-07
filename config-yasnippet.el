;;;; config-yasnippet.el

(require 'yasnippet)
(yas/initialize)

(setq yas/root-directory (concat dotfiles-dir "snippets"))

;; Load the snippets
(yas/load-directory yas/root-directory)

(provide 'config-yasnippet)
