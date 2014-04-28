;;; config-breadcrumb.el ---

;; Copyright (C) 2014 Grégoire Jadi

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

(fni/add-to-load-path (concat src-dir "breadcrumb"))
(require 'breadcrumb)

(global-set-key (kbd "S-SPC")         'bc-set)            ;; Shift-SPACE for set bookmark
(global-set-key [(meta j)]            'bc-previous)       ;; M-j for jump to previous
(global-set-key [(shift meta j)]      'bc-next)           ;; Shift-M-j for jump to next
(global-set-key [(meta up)]           'bc-local-previous) ;; M-up-arrow for local previous
(global-set-key [(meta down)]         'bc-local-next)     ;; M-down-arrow for local next
(global-set-key [(control c)(j)]      'bc-goto-current)   ;; C-c j for jump to current bookmark
(global-set-key [(control x)(meta j)] 'bc-list)           ;; C-x M-j for the bookmark menu list

(provide 'config-breadcrumb)

;;; config-breadcrumb.el ends here
