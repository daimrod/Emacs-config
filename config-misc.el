;; config-misc.el
;; Copyright (C) 2011 Grégoire Jadi

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

;; line number everywhere
(global-linum-mode 1)

;; answer by y and n instead of yes and no
(defalias 'yes-or-no-p 'y-or-n-p)

;; every backup files'll store in ~/backup directory
(setq backup-directory-alist
      '(("." . "~/backup/")))
(setq backup-by-copying t)

;; use x-clipboard
(setq x-select-enable-clipboard t)

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
(setq wg-morph-on nil)

(setq browse-url-browser-function 'w3m-browse-url)
(global-set-key "\C-xm" 'browse-url-at-point)

;; markdown configuration
(autoload 'markdown-mode "markdown-mode"
          "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (cons '("\\.md" . markdown-mode) auto-mode-alist))

;; TRAMP configuration
(setq tramp-default-proxies-alist
      '(("\\.\\(org\\|fr\\|com\\|net\\)\\'" "root" "/ssh:daimrod@%h:")))

;; ispell configuration
(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra"))

;; enable flyspell
(add-hook 'text-mode-hook
          (lambda()
            (flyspell-mode t)))

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

;; whitespace configuration
(global-whitespace-mode t)
(global-whitespace-newline-mode t)
(setq-default whitespace-style '(face trailing space-before-tab))

;; automatically refresh files from disk when they are updated
(global-auto-revert-mode t)

;; hide the cursor in the others windows
(setq-default cursor-in-non-selected-windows nil)

(provide 'config-misc)
