;; config-js.el
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

(add-to-list 'auto-mode-alist '("\\.js$" . javascript-mode))

(autoload 'inferior-moz-mode "moz" "MozRepl Inferior Mode" t)
(autoload 'moz-minor-mode "moz" "MozRepl Minor Mode" t)

(add-hook 'javascript-mode-hook 'javascript-moz-setup)

(defun javascript-moz-setup ()
  (moz-minor-mode 1))

(provide 'config-js)
