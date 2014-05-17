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
(defun dmd/save-input-method ()
  (setq-local dmd/input-method (or current-input-method dmd/input-method))
  (activate-input-method "ucs"))

(defun dmd/restore-input-method ()
  (activate-input-method dmd/input-method))

(add-hook 'god-mode-enabled-hook 'dmd/save-input-method)
(add-hook 'god-mode-disabled-hook 'dmd/restore-input-method)

(defun dmd/update-cursor ()
  (setq cursor-type (if (or god-local-mode buffer-read-only)
                        'box
                      'bar)))

(add-hook 'god-mode-enabled-hook 'dmd/update-cursor)
(add-hook 'god-mode-disabled-hook 'dmd/update-cursor)

(defun dmd/update-mode-line ()
  (let ((limited-colors-p (< (length (defined-colors)) 256)))
    (cond (god-local-mode (progn
                            (set-face-background 'mode-line (if limited-colors-p "blue" "RoyalBlue4"))))
          (t (progn
               (set-face-background 'mode-line (if limited-colors-p "black" "#222222")))))))

(add-hook 'after-buffer-switch-hook 'dmd/update-mode-line)
(add-hook 'god-mode-enabled-hook 'dmd/update-mode-line)
(add-hook 'god-mode-disabled-hook 'dmd/update-mode-line)

(provide 'config-god-mode)

;;; config-god-mode.el ends here
