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


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(setq package-enable-at-startup nil)
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(add-to-list 'load-path "~/.emacs.d/elisp")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(defvar el-get-recipe-path)
(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")

(defvar el-get-user-package-directory)
(setq el-get-user-package-directory "~/.emacs.d/el-get-user/init-files")

(defvar my-packages)
(setq my-packages
      '(el-get
        anaphora
        color-theme-sanityinc-tomorrow
        solarized-emacs
        projectile
        swiper
        swiper-helm
        helm-projectile
        helm-bibtex
        helm-ag
        helm
        magit
        git-commit-mode
        magit-svn
        pomodoro
        reftex
        orgit
        org-ref
        org-bullets
        org-mime
        org-mode
        company-mode
        yasnippet
        flycheck-pos-tip
        flycheck
        which-key
        beacon
        elscreen
        adaptive-wrap
        elisp-slime-nav
        paredit
        pyvenv
        elpy
        markdown-mode))

;;; Load Custom
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(el-get 'sync my-packages)

(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))

(eval-and-compile
  (require 'bytecomp)
  (byte-compile-disable-warning 'cl-functions)
  (require 'cl))

(require 'ispell)

;;; Load Custom (2nd time)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; Enabled/Disabled commands
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)

(require 'dmd-utils)
(require 'dmd-quiet)

(global-set-key (kbd "M-/") 'dabbrev-expand)
(global-set-key (kbd "C-x \\") 'align-regexp)
(global-set-key (kbd "C-S-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-S-r") 'isearch-backward-regexp)

(global-set-key (kbd "C-;") 'newline-and-indent)

;; Window switching. (C-x o goes to the next window)
(global-set-key (kbd "C-x C-o") (lambda () (interactive) (other-window -1)))

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

(global-set-key (kbd "M-z") 'zap-to-char-)

;; Move in window
(defun move-to-window-line-top ()
  "Goto the first visible line."
  (interactive)
  (move-to-window-line 0))

(defun move-to-window-line-bottom ()
  "Goto to the last visible line."
  (interactive)
  (move-to-window-line -1))

(global-set-key (kbd "C-c <") 'move-to-window-line-top)
(global-set-key (kbd "C-c >") 'move-to-window-line-bottom)

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

(global-set-key (kbd "C-`") 'dmd-push-mark-no-activate)
(global-set-key (kbd "M-`") 'dmd-jump-to-mark)

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

(global-set-key (kbd "C-v") 'dmd-small-scroll-up-command)
(global-set-key (kbd "M-v") 'dmd-small-scroll-down-command)

;; Remove annoying keybindings
(global-set-key (kbd "C-x C-z") nil)
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "C-x C-c") nil)

(require 'graze-url)
(global-set-key (kbd "C-c y") 'gu-copy-url-at-point)
(global-set-key (kbd "C-c b") 'gu-browse-url)
(global-set-key (kbd "C-c w s") 'gu-search)

;; Quiet!
(with-eval-after-load 'config-quiet
  (global-set-key (kbd "C-c q") 'quiet-mode))

(global-set-key (kbd "<f11>") 'toggle-frame-fullscreen)

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

(global-set-key (kbd "C-x 5 t") 'dmd-terminal-emulator)

(global-set-key (kbd "M-Q") 'unfill-paragraph)

(with-eval-after-load 'compile-cache
  (require 'compile)
  (global-set-key (kbd "<f5>") 'compile-cache)
  (global-set-key (kbd "<f6>") 'recompile))

(global-set-key (kbd "M-\\") 'execute-extended-command)

(global-set-key (kbd "C-;") 'newline-and-indent)

(with-eval-after-load 'comint
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
  (define-key comint-mode-map (kbd "C-c C-l") 'dmd-comint-truncate-buffer))

;;; Hideshow
(add-hook 'prog-mode-hook (lambda () (hs-minor-mode 1)))
(with-eval-after-load 'hideshow
  (define-key hs-minor-mode-map (kbd "C-c -") 'hs-hide-block)
  (define-key hs-minor-mode-map (kbd "C-c _") 'hs-hide-all)
  (define-key hs-minor-mode-map (kbd "C-c =") 'hs-show-block)
  (define-key hs-minor-mode-map (kbd "C-c +") 'hs-show-all))

(with-eval-after-load 'message
  (define-key message-mode-map (kbd "C-c C-c") nil))

(with-eval-after-load 'bibtex
  (define-key bibtex-mode-map (kbd "C-c C-o") 'dmd-bibtex-open)
  (add-hook 'bibtex-mode-hook 'bibtex-set-dialect))

(global-set-key (kbd "C-x #") 'delete-frame)

(require 'env-helper)
(when (file-exists-p "/etc/environment")
  (file-notify-add-watch "/etc/environment"
                         '(change)
                         #'dmd--environment-watcher))

;; Automagically tail log files
(add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-tail-mode))
(with-eval-after-load 'autorevert
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

;;; Octave
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

(with-eval-after-load 'copyright
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

(with-eval-after-load 'mule
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (prefer-coding-system 'utf-8))

(with-eval-after-load 'ansi-color
  (ansi-color-for-comint-mode-on))

(require 'saveplace)

(add-to-list 'auto-mode-alist '("wscript" . python-mode))

(with-eval-after-load 'eldoc
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode))

;; Save a list of recent files visited.
(recentf-mode 1)

;; highlight matching parentheses when the point is on them.
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
(defalias 'isp 'ispell-change-dictionary)

;; tab and indentation configuration
(setq indent-tabs-mode nil)
(setq tab-width 4)

(global-font-lock-mode 1)
(setq font-lock-maximum-decoration t
      font-lock-verbose nil)

;; Set the default font
(defvar *fonts-list* '("-unknown-Inconsolata-normal-normal-normal-*-15-*-*-*-m-0-iso10646-1"
                       "-unknown-DejaVu Sans Mono-normal-normal-normal-*-13-*-*-*-m-0-iso10646-1"))

(add-to-list 'default-frame-alist `(font . ,(find-if #'font-info *fonts-list*)))
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq initial-frame-alist (append initial-frame-alist
                                  (copy-alist default-frame-alist)))
(setq x-selection-timeout 10)

;;; init.el ends here
