;;; config-env.el ---

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

(defun dmd--environment-watcher (event)
  (destructuring-bind (descriptor action file &optional file1)
      event
    (when (eq action 'changed)
      (dmd--update-env-from-file file))))

(defun dmd--update-env-from-file (file)
  (with-current-buffer (find-file-noselect file)
    (goto-char (point-min))
    (loop for line = (buffer-substring-no-properties
                      (point-at-bol)
                      (point-at-eol))
          while (string-match "\\([^=]*\\)=\\(.*\\)" line)
          for var = (match-string 1 line)
          for val = (match-string 2 line)
          do (setenv var val)
          while (zerop (forward-line)))))

(file-notify-add-watch "/etc/environment"
                       '(change)
                       #'dmd--environment-watcher)

(provide 'config-env)

;;; config-env.el ends here
