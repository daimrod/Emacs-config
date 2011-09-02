;;;; config-color-theme.el --- Define the color theme to be used

(require 'color-theme)
(color-theme-initialize)
(setq color-theme-is-global t
      color-theme-is-cumulative t
      color-theme-load-all-themes nil)

(require 'color-theme-tango)
(color-theme-tango)

;; Set the default font
(set-face-attribute 'default (not 'this-frame-only)
                    :family "Bitstream Vera Sans Mono"
		    :height 130
		    :width 'expanded)

;; tab and indentation configuration
(setq-default indent-tabs-mode nil)
(setq tab-width 4)

(global-font-lock-mode 1)
(setq font-lock-maximum-decoration t)

(provide 'config-theme)
