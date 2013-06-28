;;; compilation-font.el ---

;; Copyright (C) 2013 Grégoire Jadi

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

(require 'compile)

(defun compilation-font-lock-manager (process)
  "This function is supposed to be added to
`compilation-start-hook' to disable font locking while filling
the buffer and re-enabling it when the process is finished."
  (with-current-buffer (process-buffer process)
    (font-lock-mode -1))
  (set-process-sentinel process
                          (lambda (process event)
                            (when (string-match "\\(finished\\|exited\\|dumped\\)" event)
                              (with-current-buffer (process-buffer process)
                                (font-lock-mode 1))))))
(provide 'compilation-font)

;;; compilation-font.el ends here
