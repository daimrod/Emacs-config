;; init.el
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

(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Load path etc.

;; load-path enhancement
(defun fni/add-to-load-path (this-directory &optional with-subdirs recursive)
  "Add THIS-DIRECTORY at the beginning of the load-path, if it exists.
Add all its subdirectories not starting with a '.' if the
optional argument WITH-SUBDIRS is not nil.
Do it recursively if the third argument is not nil."
  (when (and this-directory
             (file-directory-p this-directory))
    (let* ((this-directory (expand-file-name this-directory))
           (files (directory-files this-directory t "^[^\\.]")))

      ;; completely canonicalize the directory name (*may not* begin with `~')
      (while (not (string= this-directory (expand-file-name this-directory)))
             (setq this-directory (expand-file-name this-directory)))

      (message "Adding `%s' to load-path..." this-directory)
      (add-to-list 'load-path this-directory)

      (when with-subdirs
        (while files
               (let ((dir-or-file (car files)))
                 (when (file-directory-p dir-or-file)
                   (if recursive
                       (fni/add-to-load-path dir-or-file 'with-subdirs 'recursive)
                       (fni/add-to-load-path dir-or-file))))
               (setq files (cdr files)))))))

(defvar home-dir "/home/daimrod/")
(defvar dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(defvar elisp-dir (concat dotfiles-dir "elisp/"))
(defvar elpa-dir (concat dotfiles-dir "elpa/"))
(defvar src-dir (concat home-dir "src/elisp/"))

(fni/add-to-load-path dotfiles-dir)
(fni/add-to-load-path elisp-dir t)
(fni/add-to-load-path elpa-dir t)

(setq custom-file (concat dotfiles-dir "custom.el"))

;; These should be loaded on startup rahter than autoloaded on demande
;; since they are likely to be used in every session

(require 'bytecomp)
(byte-compile-disable-warning 'cl-functions)

(require 'cl)
(require 'saveplace)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)
(require 'workgroups)
(require 'scratch)
(require 'verbiste)
(require 'undo-tree)
(require 'alarm)
(require 'pos-tip)

;; Load my configuration
(require 'config-defuns)
(require 'config-bindings)
(require 'config-theme)
(require 'config-lisp)
(require 'config-cc-mode)
(require 'config-ediff)
(require 'config-slime)
(require 'config-yasnippet)
(require 'config-misc)
(require 'config-search)
(require 'config-bbdb)
(require 'config-git)
(require 'config-alias)
(require 'config-org)
(require 'config-auto-complete)
(require 'config-haskell)
(require 'config-prolog)
(require 'config-markdown)
(require 'config-js)
(require 'config-multi-term)
(require 'config-smerge)
;; (require 'config-notmuch)
(require 'config-ocaml)
(require 'config-w3m)
(require 'config-plantuml)
(require 'config-xml)
(require 'config-html)
(require 'config-irc)

(load custom-file 'noerror)

;; enabled/disabled commands
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
