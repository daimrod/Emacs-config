;;;; config-auto-comple.el
;; How to update auto-complete
;; cd ~/git/auto-complete/
;; git pull
;; make install DIR=~/.emacs.d/elisp/

(require 'auto-complete-config)

(add-to-list 'ac-dictionary-directories
             (concat elisp-dir "ac-dict/"))

(ac-config-default)

(setq ac-use-quick-help t)

(provide 'config-auto-comple)
