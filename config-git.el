;; config-git.el
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

(defvar magit-dir  (concat src-dir "magit/"))
(fni/add-to-load-path magit-dir t)
(add-to-list 'Info-default-directory-list
             (expand-file-name magit-dir))

(require 'magit)
(require 'magit-svn)
(require 'magit-blame)

(define-key mode-specific-map (kbd "g") 'magit-status)

(add-hook 'magit-mode-hook 'magit-load-config-extensions)

(require 'magit-topgit)
(add-hook 'magit-mode-hook 'turn-on-magit-topgit)

;;; m0ar context for diff thunks
(setq magit-diff-context-lines 5)

(add-hook 'magit-log-edit-mode-hook 'change-log-mode)
(defvar dmd/magit-changelog-mode-map
  (let ((map (copy-keymap magit-log-edit-mode-map)))
    (set-keymap-parent map change-log-mode-map)
    map))

(defadvice change-log-mode (after change-log-magit-setup (&rest args) activate)
  (when (string= (buffer-name)
                 "*magit-edit-log*")
    (use-local-map dmd/magit-changelog-mode-map)
    (setf tab-width 4
          left-margin 0)))

(provide 'config-git)
