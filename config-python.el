;; config-python.el
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

(defvar python-mode-dir (concat src-dir "python-mode/"))

(fni/add-to-load-path python-mode-dir t t)

(setq py-install-directory python-mode-dir)
(require 'python-mode)

(defun my-run-python (&optional new)
  "Run a python interpreter and display it.

If NEW is set to nil, try to switch to the current python
interpreter, otherwise create a new one even if a python
interpreter already exists."
  (interactive "P")
  (if new
   (run-python nil nil new)
   (pop-to-buffer (process-buffer (python-proc)) t)))

(define-key python-mode-map (kbd "C-c C-z") 'my-run-python)

(defun my-python-set-proc (buffer-name)
  "Set the python interpreter associated to the current buffer.

BUFFER-NAME can be either a old buffer or a new one."
  (interactive "B")
  (setf python-buffer (get-buffer buffer-name))
  (python-set-proc))

(provide 'config-python)
