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
(when (null (ignore-errors (call-process "global" nil nil nil "--help")))
  (error "global executable not found, be sure GNU Global is installed and in your PATH."))

(require 'gtags)
(defun gtags-find (&optional with-grep?)
  "Find a tag with `gtags-find-tag' if WITH-GREP? is nil,
otherwise use `gtags-find-with-grep'."
  (interactive "P")
  (if with-grep?
      (gtags-find-with-grep)
    (gtags-find-tag)))

(define-key gtags-mode-map (kbd "M-,") 'gtags-pop-stack)
(define-key gtags-mode-map (kbd "M-.") 'gtags-find)

(add-hook 'c-mode-hook '(lambda () (gtags-mode 1)))
(add-hook 'c++-mode-hook '(lambda () (gtags-mode 1)))

(provide 'config-gtags)

;;; config-gtags.el ends here
