;;;; config-slime.el --- SLIME configuration

(require 'slime-autoloads)
(require 'slime)
(slime-setup '(slime-repl
	       slime-asdf
	       slime-banner
	       slime-autodoc
	       slime-editing-commands
	       slime-fancy-inspector
	       slime-fancy
	       slime-fontifying-fu
	       slime-fuzzy
	       slime-indentation
	       slime-mdot-fu
	       slime-package-fu
	       slime-references
	       slime-scratch
	       slime-xref-browser
	       slime-presentations))

(slime-autodoc-mode)

(setq inferior-lisp-program "/usr/bin/sbcl"
      lisp-indent-function 'common-lisp-indent-function
      slime-complete-symbol-function 'slime-fuzzy-complete-symbol
      slime-startup-animation t
      slime-complete-symbol*-fancy t
      max-lisp-eval-depth 2000
      common-lisp-hyperspec-root "/home/daimrod/lisp/doc/HyperSpec/"
      slime-net-coding-system 'utf-8-unix)

(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))

(define-key slime-repl-mode-map (kbd "C-c C-v C-l") 'slime-pretty-print-presentation-at-point)slime-mode-map
(define-key slime-repl-mode-map (kbd "C-c C-v l") 'slime-pretty-print-presentation-at-point)slime-mode-map

(defun slime-dc ()
  "Close the current connection and the repl-buffer"
  (interactive)
  (save-window-excursion
	(slime-switch-to-output-buffer)
	(kill-buffer)
	(slime-net-close (slime-connection))))

(provide 'config-slime)
