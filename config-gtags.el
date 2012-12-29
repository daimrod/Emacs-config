;;; config-gtags.el ---

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

;;; Try to guess if GNU Global is installed or not.
(unless (zerop (shell-command "type -p global"))
  (error "global executable not found, be sure GNU Global is installed and in your PATH."))

(require 'gtags)
(add-hook 'c-mode-hook '(lambda () (gtags-mode 1)))
(add-hook 'c++-mode-hook '(lambda () (gtags-mode 1)))

(provide 'config-gtags)

;;; config-gtags.el ends here
