;;; config-doc.el ---

;; Copyright (C) 2012 Grégoire Jadi

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

(add-to-list 'Info-directory-list
             (expand-file-name (concat src-dir "ebib")))

(add-to-list 'Info-directory-list
             (expand-file-name (concat src-dir "magit/")))

(add-to-list 'Info-directory-list
             (expand-file-name (concat src-dir
                                       "org-mode/doc")))

(add-to-list 'Info-directory-list
             (expand-file-name (concat src-dir
                                       "mu/mu4e")))

(add-to-list 'Info-directory-list
             (expand-file-name (concat dotfiles-dir
                                       "doc")))

(provide 'config-doc)

;;; config-doc.el ends here
