;;;; config-cc-mode.el

(require 'smart-tabs-mode)
(require 'smarttabs)

(setq c-default-style
      '((java-mode . "java")
        (awk-mode . "awk")
        (c-mode . "k&r")
        (other . "linux")))

(setq-default c-basic-offset 4
              tab-width 4 ; or any other preferred value
              cua-auto-tabify-rectangles nil
              compilation-window-height 10)

(add-hook 'c-mode-hook '(lambda ()
                         (setq c-auto-newline t)))

(provide 'config-cc-mode)
