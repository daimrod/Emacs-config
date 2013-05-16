;;; config-irc.el ---

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

(fni/add-to-load-path (concat src-dir "shoes-off/"))
(require 'rcirc)
(require 'shoes-off-log)
(require 'shoes-off)

(defun rcirc-shoes-off-all ()
  (interactive)
  (rcirc-connect "localhost" 7000 "daimrod" "shoes-off@irc.freenode.net" nil nil "shoes-off")
  (rcirc-connect "localhost" 7000 "daimrod" "shoes-off@irc.geeknode.org" nil nil "shoes-off"))

(defun rcirc-stop-all()
  (interactive)
  (mapc 'kill-buffer
        (loop for buffer in (remove-if-not (lambda (buffer)
                                             (eq 'rcirc-mode
                                                 (with-current-buffer buffer major-mode)))
                                           (buffer-list))
              if (null (get-buffer-process buffer))
              collect buffer
              else
              do (kill-buffer buffer))))

(provide 'config-irc)

;;; config-irc.el ends here
