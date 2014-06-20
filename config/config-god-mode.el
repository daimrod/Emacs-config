;;; config-god-mode.el ---

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

(defvar dmd/input-method nil)

(add-hook 'buffer-list-update-hook 'dmd/god-mode-setup)
(add-hook 'god-mode-enabled-hook 'dmd/god-mode-setup)
(add-hook 'god-mode-disabled-hook 'dmd/god-mode-setup)

(defun dmd/god-mode-setup ()
  (with-current-buffer (window-buffer)
    (let ((limited-colors-p (< (length (defined-colors)) 256)))
      (cond (god-local-mode
             (set-face-background 'mode-line (if limited-colors-p "blue" "RoyalBlue4"))

             (setq cursor-type 'box)

             (setq-local dmd/input-method (or current-input-method dmd/input-method))
             (activate-input-method "ucs"))
            (t
             (set-face-background 'mode-line (if limited-colors-p "black" "#222222"))

             (setq cursor-type 'bar)

             (activate-input-method dmd/input-method))))))

(provide 'config-god-mode)

;;; config-god-mode.el ends here
