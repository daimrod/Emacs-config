;;;; config-yasnippet.el

(setq yasnippet-dir (concat elisp-dir "yasnippet-0.6.1c/"))
(add-to-list 'load-path yasnippet-dir)
(require 'yasnippet)
(yas/initialize)
(yas/load-directory (concat yasnippet-dir "snippets/"))

(setq yas/root-directory (concat dotfiles-dir "mysnippets/"))

;; Load the snippets
(yas/load-directory yas/root-directory)

(provide 'config-yasnippet)
