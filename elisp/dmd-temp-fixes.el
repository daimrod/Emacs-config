;;; dmd-temp-fixes.el --- Temp fixes for Emacs

;; Copyright (C) 2017 Grégoire Jadi

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

(require 'shr)
(defun shr-image-fetched (status buffer start end &optional flags)
  ;; emacs/0001-Search-for-image-from-the-beginning-of-the-fetched-b.patch
  (let ((image-buffer (current-buffer)))
    (when (and (buffer-name buffer)
               (not (plist-get status :error)))
      (url-store-in-cache image-buffer)
      (goto-char (point-min))
      (mm-disable-multibyte)
      (when (or (search-forward "\n\n" nil t)
                (search-forward "\r\n\r\n" nil t))
        (let ((data (shr-parse-image-data)))
          (with-current-buffer buffer
            (save-excursion
              (save-restriction
                (widen)
                (let ((alt (buffer-substring start end))
                      (properties (text-properties-at start))
                      (inhibit-read-only t))
                  (delete-region start end)
                  (goto-char start)
                  (funcall shr-put-image-function data alt flags)
                  (while properties
                    (let ((type (pop properties))
                          (value (pop properties)))
                      (unless (memq type '(display image-size))
                        (put-text-property start (point) type value)))))))))))
    (kill-buffer image-buffer)))

(provide 'dmd-temp-fixes)

;;; dmd-temp-fixes.el ends here
