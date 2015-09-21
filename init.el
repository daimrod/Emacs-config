;;; init.el --- Initialization

;; Copyright (C) 2015 Grégoire Jadi

;; Author: Grégoire Jadi <gregoire.jadi@gmail.com>

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))

(defvar elisp-dir (expand-file-name "elisp/" user-emacs-directory))
(defvar elpa-dir (expand-file-name "elpa/" user-emacs-directory))
(defvar config-dir (expand-file-name "config/" user-emacs-directory))
(defvar modules-dir (expand-file-name "modules/" user-emacs-directory))

(defcustom src-dir (expand-file-name "~/src/elisp/")
  "The source directory where third-part modules are located."
  :group 'dmd-config)

(add-to-list 'load-path config-dir)
(add-to-list 'load-path elisp-dir)

;; custom-file configuration
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(eval-and-compile
  (require 'bytecomp)
  (byte-compile-disable-warning 'cl-functions)
  (require 'cl))

(defvar dmd-config-modules
  (cl-loop for config-file in (directory-files config-dir nil "^config-.*.el$")
           collect (intern (file-name-base config-file)))
  "List of available configuration modules.")

(dolist (module-dir (directory-files modules-dir t "^[^.]"))
  (add-to-list 'load-path module-dir))

(defvar use-package-verbose t)
(require 'use-package)
(require 'bind-key)

(load (expand-file-name "~/quicklisp/slime-helper.el") t)
(load (expand-file-name "~/quicklisp/clhs-use-local.el") t)

(use-package pdf-tools
  :load-path "pdf-tools/pdf-tools-0.70/"
  :config
  (pdf-tools-install))

;; Enabled/Disabled commands
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)

(use-package info
  :demand t)

(use-package adaptive-wrap)

(use-package ispell
  :config
  (defalias 'isp 'rw-ispell-change-dictionary))

(use-package dabbrev
  :bind (("M-/" . dabbrev-expand)))

(use-package align
  :bind (("C-x \\" . align-regexp)))

(use-package isearch
  :bind (("C-s" . isearch-forward-regexp)
         ("C-r" . isearch-backward-regexp)))

(use-package w3m
  :commands (w3m-buffer))

(use-package windmove
  :bind* (("S-<up>" . windmove-up)
          ("S-<down>" . windmove-down)
          ("S-<right>" . windmove-right)
          ("S-<left>" . windmove-left)))

;; Window switching. (C-x o goes to the next window)
(bind-key "C-x C-o" (lambda () (interactive) (other-window -1)))

;; Use another version of zap-to-char (don't chopd the last char)
(defun zap-to-char- (arg char)
  "Kill up to and not including ARGth occurrence of CHAR.
Case is ignored if ‘case-fold-search’ is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found."
  (interactive "p\ncZap to char-: ")
  ;; Avoid "obsolete warnings for translation-table-for-input.
  (with-no-warnings
    (if (char-table-p translation-table-for-input)
        (setq char (or (aref translation-table-for-input char) char))))
  (save-excursion
    (kill-region (point)
                 (progn
                   (search-forward (char-to-string char) nil nil arg)
                   (if (>= arg 0)
                       (- (point) 1)
                     (+ (point) 1))))))

(bind-key "M-z" 'zap-to-char-)

;; Move in window
(defun move-to-window-line-top ()
  "Goto the first visible line."
  (interactive)
  (move-to-window-line 0))

(defun move-to-window-line-bottom ()
  "Goto to the last visible line."
  (interactive)
  (move-to-window-line -1))

(bind-keys ("C-c <" . move-to-window-line-top)
           ("C-c >" . move-to-window-line-bottom))


;; Inactive marks -- Quickmove between mark without disturbing transient-mark-mode
(defun dmd-push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region.
Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

(defun dmd-jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))

(bind-keys ("C-`" . dmd-push-mark-no-activate)
           ("M-`" . dmd-jump-to-mark))

;; Scrolling -- Scroll up and down slowly by default (one line at time)
(defcustom dmd-small-scrolling 5
  "How many lines should be scrolled with `dmd-small-scroll-[up/down]-command'."
  :type 'integer
  :group 'dmd-config)

(defun dmd-small-scroll-up-command (&optional arg)
  "Scroll text of selected window upward ARG lines; or `dmd-small-scrolling' if no ARG."
  (interactive "^P")
  (let ((fun-scroll-up (if (fboundp 'scroll-up-command)
                           'scroll-up-command
                         'scroll-up)))
    (if arg
        (funcall fun-scroll-up arg)
      (funcall fun-scroll-up dmd-small-scrolling))))

(defun dmd-small-scroll-down-command (&optional arg)
  "Scroll text of selected window downward ARG lines; or `dmd-small-scrolling' if no ARG."
  (interactive "^P")
  (let ((fun-scroll-down (if (fboundp 'scroll-down-command)
                             'scroll-down-command
                           'scroll-down)))
    (if arg
        (funcall fun-scroll-down arg)
      (funcall fun-scroll-down dmd-small-scrolling))))

(bind-keys ("C-v" . dmd-small-scroll-up-command)
           ("M-v" . dmd-small-scroll-down-command))

;; Remove annoying keybindings
(unbind-key "C-x C-z")
(unbind-key "C-z")
(unbind-key "C-x C-c")

(use-package graze-url
  :bind (("C-c y" . gu-copy-url-at-point)
         ("C-c b" . gu-browse-url)
         ("C-c w s" . gu-search)))

;; iy-go-to-char configuration
(use-package iy-go-to-char
  :bind (("C-c f" . iy-go-to-char)
         ("C-c F" . iy-go-to-char-backward)
         ("C-c ;" . iy-go-to-char-continue)
         ("C-c ," . iy-go-to-char-continue-backward)))

;; Quiet!
(use-package config-quiet
  :bind (("C-c q" . quiet-mode)))

;; Winner configuration
(use-package winner
  :bind (("C-c u" . winner-undo)
         ("C-c r" . winner-redo)))

(bind-key "<f11>" 'toggle-fullscreen)

;;;; Terminal Emulator
;; C-x 4 t is for multi-term in another window, so a terminal in
;; another frame should be an xterm or similar.
(defcustom terminal-emulator "xterm"
  "A terminal emulator to use."
  :group 'external)

(defcustom terminal-emulator-parameters nil
  "The parameters to send to the terminal emulator."
  :group 'external
  :type '(repeat string))

(defun dmd-terminal-emulator ()
  "Open a terminal emulator using `terminal-emulator'."
  (interactive)
  (let ((process-environment
         (remove-if (lambda (env)
                      (string-match-p "^TMUX=" env))
                    process-environment)))
    (apply
     #'start-process
     "Terminal Emulator"
     nil
     (etypecase terminal-emulator
       (string terminal-emulator)
       (function (funcall terminal-emulator))
       (symbol (symbol-value terminal-emulator)))
     terminal-emulator-parameters)))

(bind-key "C-x 5 t" 'dmd-terminal-emulator)

(bind-key "M-Q" 'unfill-paragraph)

(use-package compile
  :init (use-package compile-cache)
  :bind (("<f5>" . compile-cache)
         ("<f6>" . recompile)))

(global-set-key (kbd "M-\\") 'execute-extended-command)

;;;; Helm
(use-package helm-config
  :init
  (use-package helm-mode
    :demand t
    :config
    (helm-mode 1))
  :bind (("M-x" . helm-M-x)
         ("C-c m" . helm-M-x)
         ("C-c C-m" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-buffers-list)
         ("C-c h" . helm-command-prefix))
  :config
  (use-package helm-command)
  (use-package helm-files)
  (use-package helm-buffers)
  (use-package helm-ag)
  (use-package helm-bibtex)
  (use-package helm-pages
    :bind (("C-c j" . helm-pages))))

(global-set-key (kbd "C-;") 'newline-and-indent)

(use-package comint
  :config
  (defcustom comint-buffer-minimum-size 0
    "The minimum size for comint buffer when truncating."
    :type 'integer
    :group 'comint)
  (defun dmd-comint-truncate-buffer (&optional n)
    "Does what comint-truncate-buffer should do. That is, truncate
the buffer to keep N lines.

If N is not set, use `comint-buffer-minimum-size'."
    (interactive "P")
    (let ((comint-buffer-maximum-size
           (or n
               (- (line-number-at-pos (point-max)) (line-number-at-pos)))))
      (comint-truncate-buffer)))
  (bind-key "C-c C-l" 'dmd-comint-truncate-buffer comint-mode-map))

(use-package doc-view
  :config
  (defun dmd-doc-view-info ()
    "Open a buffer with the current doc's info as text."
    (interactive)
    (let ((buffer (concat "*Info of "
                          (file-name-nondirectory buffer-file-name)
                          "*")))
      (if (get-buffer buffer)
          (kill-buffer buffer))
      (call-process "/usr/bin/pdfinfo" nil buffer nil buffer-file-name)
      (switch-to-buffer buffer)
      (read-only-mode 1)
      (goto-char (point-min))))

  (defun dmd-doc-view-external ()
    "Open the current document using an external program."
    (interactive)
    (start-process "doc-view external" (generate-new-buffer " *DocView External Viewer*")
                   "/usr/bin/evince" buffer-file-name))
  
  (bind-keys :map doc-view-mode-map
             ("C-c C-i" . dmd-doc-view-info)
             ("C-c C-v" . dmd-doc-view-external)))

(use-package hideshow
  :config
  (bind-keys :map hs-minor-mode-map
             ("C-c -" . hs-hide-block)
             ("C-c _" . hs-hide-all)
             ("C-c =" . hs-show-block)
             ("C-c +" . hs-show-all)))

(use-package company
  :bind (("C-SPC" . company-complete))
  :config)

(use-package magit
  :load-path "modules/magit/lisp"
  :init
  (use-package git-commit-mode)
  (use-package magit-autoloads)

  :bind (("C-c g" . magit-status))

  :config
  (add-to-list 'Info-directory-list
               (expand-file-name (concat src-dir "magit/")))

  (use-package magit-svn
    :config
    (add-hook 'magit-mode-hook 'magit-svn-mode))
  (use-package orgit))

(use-package javadoc-lookup
  :bind (("C-h j" . javadoc-lookup))
  :config
  (javadoc-add-roots "/usr/share/doc/openjdk-7-jdk/api"))

(use-package eclim
  :config
  (bind-key "C-c C-e p r" 'eclim-run-class eclim-mode-map)
  (add-hook 'eclim-mode-hook 'company-emacs-eclim-setup)
  (add-hook 'java-mode-hook 'eclim-mode))

(use-package color-moccur
  :config
  (use-package moccur-edit)
  (bind-keys :prefix-map moccur-map
             :prefix "M-o"
             ("s" . occur-by-moccur)
             ("m" . moccur)
             ("d" . dmoccur))
  (defalias 'mgrep 'moccur-grep)
  (defalias 'mrgrep 'moccur-grep-find))

(use-package org
  :load-path "modules/org-mode/lisp"
  :config
  (add-to-list 'load-path (expand-file-name "modules/org-mode/contrib/lisp" user-emacs-directory))
  (bind-keys :map org-mode-map
	     ("C-c )" . helm-bibtex)
	     ("C-c j)" . (lambda (&optional prefix)
                                           (interactive "P")
                                           (if prefix
                                               (helm-org-agenda-files-headings)
                                             (helm-org-in-buffer-headings))))
	     ("C-c >" . org-time-stamp-inactive))

  (add-to-list 'Info-directory-list
               (expand-file-name "org-mode/doc" modules-dir))
  (require 'org-contacts)
  (require 'org-clock)
  (require 'org-habit)
  (require 'org-ref)
  (require 'org-agenda)
  (require 'ob)
  (require 'ob-python)
  (use-package diary-lib
	:config
	(diary-list-entries (calendar-current-date) nil 'list-only)
	(mapc (lambda (file)
			(bury-buffer (find-file-noselect file)))
		  diary-included-files)))

(use-package pyvenv
  :config
  (pyvenv-tracking-mode t)
  (pyvenv-mode 1))

(use-package elpy
  :config
  (elpy-enable))

(use-package message
  :config
  (unbind-key "C-c C-c" message-mode-map))

(use-package bibtex
  :config
  (bind-key "C-c C-o" 'dmd-bibtex-open bibtex-mode-map))

(bind-key "C-x #" 'delete-frame)

(use-package ox-beamer
  :config
  (unbind-key "C-c C-b" org-beamer-mode-map))

(bind-key "<f9>" 'org-agenda)

(use-package yasnippet
  :config
  (bind-key "C-c & C-s" 'company-yasnippet yas-minor-mode-map)
  (use-package company-yasnippet
    :bind (("M-C" . company-yasnippet))))

(use-package mu4e
  :load-path "modules/mu/mu4e"
  :config
  (add-to-list 'Info-directory-list
               (expand-file-name (expand-file-name
                                  "mu/mu4e"
                                  modules-dir)))
  (setq mu4e-mu-binary (or (executable-find "mu")
                           (expand-file-name "mu/mu/mu"
                                             modules-dir)))
  (use-package org-mu4e
    :demand t))

(use-package which-key
  :config
  (which-key-mode 1))

(mapc (lambda (module)
        (message "Loading %s" module)
        ;; (require module)
        (unless (ignore-errors (require module))
          (warn "Failed to load module `%s'" module)))
	  dmd-config-modules)

(use-package env-helper
  :init
  (use-package filenotify
    :commands (file-notify-add-watch))
  :config
  (file-notify-add-watch "/etc/environment"
                         '(change)
                         #'dmd--environment-watcher))

;; automagically tail log files
(use-package autorevert
  :mode ("\\.log\\'" . auto-revert-tail-mode)
  :config
  (defun dmd--etc-log-tail-handler ()
    (goto-char (point-max))
    (make-local-variable 'auto-revert-interval)
    (setq auto-revert-interval 1)
    (auto-revert-set-timer)
    (setq auto-revert-verbose nil)
    (read-only-mode 1)
    (font-lock-mode -1)
    (visual-line-mode -1)
    (toggle-truncate-lines 1)
    (when (fboundp 'show-smartparens-mode)
      (show-smartparens-mode 0)))
  (add-hook 'auto-revert-tail-mode-hook 'dmd--etc-log-tail-handler))

(use-package slime
  :config
  (defalias 'srepl 'slime-repl))

(use-package copyright
  :config
  ;;; redefined skeleton (original in copyright.el)
  (define-skeleton copyright
    "Insert a copyright by $ORGANIZATION notice at cursor."
    nil
    comment-start
    " Copyright (C) " `(format-time-string "%Y") " by "
    (or (getenv "ORGANIZATION")
        user-full-name)
    comment-end \n
    comment-start
    " See the file LICENSE for copying permission."
    comment-end \n))

(use-package workgroups
  :demand t
  :config
  (workgroups-mode 1))

(use-package mule
  :config
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (prefer-coding-system 'utf-8))

(use-package firestarter
  :config
  (firestarter-mode 1))

(use-package ansi-color
  :config
  (ansi-color-for-comint-mode-on))

(use-package saveplace
  :demand t)

(use-package python
  :mode (("wscript" . python-mode)))

(use-package undo-tree
  :demand t
  :config
  (global-undo-tree-mode))

(use-package projectile
  :demand t
  :config
  (projectile-global-mode)
  (use-package helm-projectile)
  (helm-projectile-on))

;; Save a list of recent files visited.
(recentf-mode 1)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; Text-mode Hook
(add-hook 'text-mode-hook 'dmd-text-mode-setup)

;; Message-mode hook
(add-hook 'message-mode-hook 'dmd-text-mode-setup)

;; Prog-mode hook
(add-hook 'prog-mode-hook 'hs-minor-mode)
(add-hook 'prog-mode-hook 'subword-mode)
(add-hook 'prog-mode-hook 'glasses-mode)


;;;; Alias
(defalias 'renb 'dmd-rename-buffer)
(defalias 'yes-or-no-p 'y-or-n-p)

(provide 'init)

;;; init.el ends here

;; Local Variables:
;; firestarter: (byte-recompile-file (buffer-file-name) nil 0)
;; End:
