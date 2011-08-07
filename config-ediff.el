;;;; config-ediff.el --- Ediff customization

(require 'ediff)

;; how ediff will split
(setq ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain)

(provide 'config-ediff)
