;;; config-misc.el --- misc configuration

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


(require 'scratch)

;; markdown configuration
(autoload 'markdown-mode "markdown-mode"
          "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist
             '("\\.md" . markdown-mode))
(add-to-list 'auto-mode-alist
             '("\\.mkd" . markdown-mode))

(add-to-list 'auto-mode-alist
             '("\\.m" . octave-mode))

;; ImageMagick support
(autoload 'eimp-mode "eimp" "Emacs Image Manipulation Package." t)
(add-hook 'image-mode-hook 'eimp-mode)

;; enable Zaps mode everywhere
(require 'markit)
(global-markit-mode t)

;; yaml-mode configuration
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

;;; column-number in the modeline
(column-number-mode 1)

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

;;; Emacs-wget
;;; Set downoad directory to current dir
(setf wget-download-directory "./")

;;; Smart Tab
;;; http://www.emacswiki.org/emacs/TabsAreEvil
;;; http://www.emacswiki.org/emacs/SmartTabs
(add-hook 'prog-mode-hook 'smart-tab-mode)

;;; Winner mode is a winner!
(winner-mode 1)

;; Compilation
(add-hook 'compilation-start-hook 'compilation-font-lock-manager)

;; Recutils
(autoload 'rec-mode "rec-mode")

;; Cflow
(autoload 'cflow-mode "cflow-mode")

;; Creole
(autoload 'creole-mode "creole-mode")
(add-to-list 'auto-mode-alist '("\\.creole\\'" . creole-mode))

(setq-default save-place t)
(setq save-place-file "~/.emacs.d/places")

;; Info configuration
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

(global-company-mode 1)

;; gnuplot
(add-to-list 'auto-mode-alist '("\\.gp\\'" . gnuplot-mode))

;; Ace Window
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

;; Diary mode
(add-to-list 'auto-mode-alist
             '("\\.diary" . diary-mode))

(global-flycheck-mode 1)

;; Frame title configuration
(setq frame-title-format
      '((:eval (format "%d " (emacs-pid)))
        ("" invocation-name "@" system-name)))

(firestarter-mode 1)

(provide 'config-misc)

;;; config-misc.el ends here
