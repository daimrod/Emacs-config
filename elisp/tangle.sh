#!/bin/sh
":"; exec emacs --quick --script "$0" -- "$@" # -*- mode: emacs-lisp; -*-
;;; Commentary:

;; (message "foo: %S" argv)                ; print on stderr
;; (princ "bar")                           ; print on stdout

;;; Code:

(add-to-list 'load-path (expand-file-name "~/.emacs.d/modules/org-mode/lisp/"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/modules/org-mode/contrib/lisp/"))
(require 'org)
(require 'ob)
(require 'ob-tangle)

(message "%s" (format-time-string "%Y-%m-%d %H:%M:%S"))
(dolist (file argv)
  (with-current-buffer (find-file-noselect file)
    (org-babel-tangle)))

(setq argv nil) ; prevents emacs from interpreting remaining args
