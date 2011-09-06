;;;; init.el

(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Load path etc.

(setq home-dir "/home/daimrod/")
(setq dotfiles-dir (file-name-directory
		    (or (buffer-file-name) load-file-name)))
(setq elisp-dir (concat dotfiles-dir "elisp/"))
(setq bzr-dir (concat home-dir "bzr/"))

(add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path elisp-dir)
(setq custom-file (concat dotfiles-dir "custom.el"))

;; These should be loaded on startup rahter than autoloaded on demande
;; since they are likely to be used in every session

(require 'cl)
(require 'saveplace)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)
(require 'w3m-load)
(require 'mime-w3m)
(require 'workgroups)
(require 'scratch)

;; Load my configuration
(require 'config-defuns)
(require 'config-bindings)
(require 'config-theme)
(require 'config-lisp)
(require 'config-cc-mode)
(require 'config-ediff)
(require 'config-slime)
(require 'config-yasnippet)
(require 'config-misc)
(require 'config-bbdb)

(load custom-file 'noerror)

;; enabled/disabled commands
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
