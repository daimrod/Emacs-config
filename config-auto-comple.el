;;;; config-auto-comple.el

(require 'auto-complete-config)

(add-to-list 'ac-dictionary-directories
             (concat elisp-dir "ac-dict/"))

(ac-config-default)

(provide 'config-auto-comple)
