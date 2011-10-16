;;;; config-cc-mode.el

(require 'smart-tabs-mode)
(require 'smarttabs)

(setq c-default-style
      '((java-mode . "java")
        (awk-mode . "awk")
        (c-mode . "k&r")
        (other . "linux")))

(setq-default c-basic-offset 4)
(setq-default tab-width 4) ; or any other preferred value
(setq cua-auto-tabify-rectangles nil)

(provide 'config-cc-mode)
