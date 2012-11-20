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
(ido-mode t)
(setq ido-enable-flex-matching t
      ido-everywhere t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-max-prospects 10
      ido-default-file-method 'selected-window
      ido-default-buffer-method 'selected-window)

(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; workgroups configuration
(workgroups-mode)
(setq wg-morph-on nil
      wg-query-for-save-on-emacs-exit nil
      wg-query-for-save-on-workgroups-mode-exit nil)

(setq browse-url-browser-function 'w3m-browse-url)

;; markdown configuration
(autoload 'markdown-mode "markdown-mode"
          "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (cons '("\\.md" . markdown-mode) auto-mode-alist))

;; TRAMP configuration
(setq tramp-default-proxies-alist
      '(("\\.\\(org\\|fr\\|com\\|net\\)\\'" "root" "/ssh:daimrod@%h:")))

;; waf config
(setq auto-mode-alist (cons '("wscript" . python-mode) auto-mode-alist))

;; do not ask confirmation to open a file or a buffer
(setq confirm-nonexistent-file-or-buffer nil)

;; never ask confirmation to create a new buffer
(setq ido-create-new-buffer 'always)

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
(eval-after-load "autopair"
  '(delete-selection-mode t))

;; ELPA configuration
(setq package-archives '(("ELPA" . "http://tromey.com/elpa/") 
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))

;; dired configuration
(require 'dired)
(require 'dired-x)

(setq sentence-end-double-space nil)

(require 'key-chord)
(key-chord-mode t)

;; enable rainbow-mode in css-mode
(require 'rainbow-mode)
(add-hook 'css-mode-hook 'rainbow-mode)

(require 'chm-view)

(require 'cursor-chg)
(toggle-cursor-type-when-idle 1)
(change-cursor-mode 1)
(setq-default curchg-default-cursor-color "gold"
              curchg-default-cursor-type 'box
              curchg-idle-cursor-type nil)

(require 'woman)
(add-to-list 'woman-manpath "~/.local/share/man")

;;; disable fringes
;;; look at fringe.el::138 defconst fringe-styles
(set-fringe-style 0)

;;; enable flyspell nearly everywhere
(setq-default flyspell-use-meta-tab nil)
(require 'flyspell)
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;;; apache-mode configuration
(autoload 'apache-mode "apache-mode" nil t)
(mapc (lambda (pair)
        (add-to-list 'auto-mode-alist pair))
      '(("\\.htaccess\\'"   . apache-mode)
        ("httpd\\.conf\\'"  . apache-mode)
        ("srm\\.conf\\'"    . apache-mode)
        ("access\\.conf\\'" . apache-mode)
        ("sites-\\(available\\|enabled\\)/" . apache-mode)))

;;; autopair configuration
(fni/add-to-load-path (concat src-dir "autopair/"))
(require 'autopair)
(autopair-global-mode 1)

;;; browse-kill-ring configuration
(require 'browse-kill-ring)
(global-set-key (kbd "C-M-y") 'browse-kill-ring)

(setq-default user-mail-address "daimrod@gmail.com"
              user-full-name "Grégoire Jadi")

;;; column-number in the modeline
(column-number-mode 1)

;;; semantic-mode
(semantic-mode 1)

;;; recursive minibuffer
(setf enable-recursive-minibuffers t)

;;; enable smex (M-x interface with Ido-style fuzzy matching.)
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
(global-set-key (kbd "C-c C-c C-M-x") 'execute-extended-command)

;;; ace-jump-mode configuration
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
;; you can select the key you prefer to
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

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
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)


(provide 'config-misc)
