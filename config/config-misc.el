;; config-misc.el
;; Copyright (C) 2011, 2012 Grégoire Jadi

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

;; ELPA configuration
(setq package-archives
	  '(("ELPA" . "http://tromey.com/elpa/") 
        ("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)

(require 'linum)

;; disable line number everywhere
(global-linum-mode -1)

;; answer by y and n instead of yes and no
(defalias 'yes-or-no-p 'y-or-n-p)

;; every backup files'll store in ~/backup directory
(setq backup-directory-alist
      '(("." . "~/backup/")))
(setq backup-by-copying t)

;; configure cliboard
(setq x-select-enable-clipboard t
      mouse-drag-copy-region t
      yank-pop-change-selection t
      kill-do-not-save-duplicates t)

;; utf-8 roxx
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(ansi-color-for-comint-mode-on)

;; some configuration (C-h v)
(setq require-final-newline t
      uniquify-buffer-name-style 'forward
      fringe-mode (cons 4 0)
      save-place-file (concat dotfiles-dir "places"))

;; Save a list of recent files visited.
(recentf-mode 1)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; enable ido-mode
(require 'ido)
(fni/add-to-load-path (concat src-dir "ido-ubiquitous/"))
(require 'ido-ubiquitous)

(setq ido-enable-flex-matching t
      ido-everywhere t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-max-prospects 10
      ido-default-file-method 'selected-window
      ido-default-buffer-method 'selected-window
      ido-create-new-buffer 'always
      ido-ubiquitous-enable-compatibility nil

      ;; Display ido results vertically, rather than horizontally
      ido-decorations '("\n-> "
                        ""
                        "\n   "
                        "\n   ..."
                        "[" "]"
                        " [No match]"
                        " [Matched]"
                        " [Not readable]"
                        " [Too big]"
                        " [Confirm]"))

(defun ido-disable-line-truncation ()
  (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)
(defun ido-define-keys ()
  ;; C-n/p is more intuitive in vertical layout
  (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))
(add-hook 'ido-setup-hook 'ido-define-keys)

(ido-mode 1)
(ido-ubiquitous-mode 1)

;; Text-mode Hook
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(add-hook 'text-mode-hook (lambda () (activate-input-method "latin-postfix")))

;; Prog-mode Hook
(add-hook 'prog-mode-hook (lambda () (activate-input-method "ucs")))

;; workgroups configuration
(workgroups-mode)
(setq wg-morph-on nil
      wg-query-for-save-on-emacs-exit nil
      wg-query-for-save-on-workgroups-mode-exit nil)

(setq browse-url-browser-function '(lambda (url) (w3m-browse-url url 'new-session)))

;; markdown configuration
(autoload 'markdown-mode "markdown-mode"
          "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist
             '("\\.md" . markdown-mode))
(add-to-list 'auto-mode-alist
             '("\\.mkd" . markdown-mode))

;; TRAMP configuration
(setq tramp-default-proxies-alist
      '(("\\.\\(org\\|fr\\|com\\|net\\)\\'" "root" "/ssh:daimrod@%h:")))

;; waf config
(setq auto-mode-alist (cons '("wscript" . python-mode) auto-mode-alist))

;; do not ask confirmation to open a file or a buffer
(setq confirm-nonexistent-file-or-buffer nil)

;; undo-tree everywhere
(global-undo-tree-mode)

;; ImageMagick support
(autoload 'eimp-mode "eimp" "Emacs Image Manipulation Package." t)
(add-hook 'image-mode-hook 'eimp-mode)

;; automatically refresh files from disk when they are updated
(global-auto-revert-mode t)

;; hide the cursor in the others windows
(setq-default cursor-in-non-selected-windows nil)

;; Prefer text-mode over fundamental-mode
(setq-default major-mode 'text-mode)

;; enable Zaps mode everywhere
(fni/add-to-load-path (concat src-dir "markit/"))
(require 'markit)
(global-markit-mode t)

;; yaml-mode configuration
(fni/add-to-load-path (concat src-dir "yaml-mode/"))
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; Enable delete-selection-mode
(delete-selection-mode 1)

;; dired configuration
(require 'dired)
(require 'dired-x)

(setq sentence-end-double-space nil)

;; enable rainbow-mode in css-mode
(require 'rainbow-mode)
(add-hook 'css-mode-hook 'rainbow-mode)

(require 'chm-view)

(blink-cursor-mode 0)
(require 'cursor-chg)
(toggle-cursor-type-when-idle 0)
(change-cursor-mode 1)
(setq-default curchg-default-cursor-color "gold"
              curchg-default-cursor-type 'box
              curchg-idle-cursor-type nil)

(require 'woman)
(add-to-list 'woman-manpath "~/.local/share/man")

;;; disable fringes
;;; look at fringe.el::138 defconst fringe-styles
(set-fringe-style 0)

;;; apache-mode configuration
(autoload 'apache-mode "apache-mode" nil t)
(mapc (lambda (pair)
        (add-to-list 'auto-mode-alist pair))
      '(("\\.htaccess\\'"   . apache-mode)
        ("httpd\\.conf\\'"  . apache-mode)
        ("srm\\.conf\\'"    . apache-mode)
        ("access\\.conf\\'" . apache-mode)
        ("sites-\\(available\\|enabled\\)/" . apache-mode)))

;;; browse-kill-ring configuration
(require 'browse-kill-ring)
(global-set-key (kbd "C-M-y") 'browse-kill-ring)

(setq-default user-mail-address "daimrod@gmail.com"
              user-full-name "Grégoire Jadi")

;;; column-number in the modeline
(column-number-mode 1)

;;; recursive minibuffer
(setf enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)

;;; ace-jump-mode configuration
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)

;; 
;; enable a more powerful jump back function from ace jump mode
;;
(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))

(setf ace-jump-mode-scope 'frame)

;;; manual-tagging
(fni/add-to-load-path (concat src-dir "manual-tagging/"))
(require 'manual-tagging)

;;; Emacs-wget
;;; Set downoad directory to current dir
(require 'w3m-wget)
(setf wget-download-directory "./")

;;; Smart Tab
;;; http://www.emacswiki.org/emacs/TabsAreEvil
;;; http://www.emacswiki.org/emacs/SmartTabs
(require 'smart-tab)
(add-hook 'prog-mode-hook 'smart-tab-mode)

;;; Winner mode is a winner!
(winner-mode 1)

;;; Edebug
(require 'edebug)

;; Compilation
(require 'compilation-font)
(add-hook 'compilation-start-hook 'compilation-font-lock-manager)

;; Emacs Async
(fni/add-to-load-path (concat src-dir "emacs-parallel/"))
(require 'parallel)

;; Use The Source Luke!
(setq find-function-C-source-directory (expand-file-name "~/packages/xwidget-emacs/src")) ;

;; Recutils
(autoload 'rec-mode "rec-mode")

;; Cflow
(autoload 'cflow-mode "cflow-mode")

;; Creole
(autoload 'creole-mode "creole-mode")
(add-to-list 'auto-mode-alist '("\\.creole\\'" . creole-mode))

;; ESS
(require 'ess)
(require 'ess-site)

;; Saveplace
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/places")

;; Info configuration
(require 'info)
(add-to-list 'Info-directory-list
             "~/.local/share/info")

;;; Ansi Color
;; These colors are used in (async-)shell-command buffers
(setq ansi-color-map
      [default bold default italic underline success warning error nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
        (foreground-color . "#242424")
        (foreground-color . "#e5786d")
        (foreground-color . "#95e454")
        (foreground-color . "#cae682")
        (foreground-color . "#8ac6f2")
        (foreground-color . "#4682b4")  ; was #333366
        (foreground-color . "#ccaa8f")
        (foreground-color . "#f6f3e8")
        nil nil
        (background-color . "#242424")
        (background-color . "#e5786d")
        (background-color . "#95e454")
        (background-color . "#cae682")
        (background-color . "#8ac6f2")
        (background-color . "#4682b4")  ; was #333366
        (background-color . "#ccaa8f")
        (background-color . "#f6f3e8")
        nil nil])

;; Dispatcher
(fni/add-to-load-path (concat src-dir "dispatcher/"))

(provide 'config-misc)
