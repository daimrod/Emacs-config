 ;;; dmd-pulse.el ---

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

(eval-when-compile (require 'cl))

(defgroup dmd-pulse nil
  "Pulse"
  :group 'faces)

(defface dmd-pulse/face
  '((((class color) (background dark))
     (:background "#AAAA33"))
    (((class color) (background light))
     (:background "#FFFFAA")))
  ""
  :group 'dmd-pulse)

(defcustom dmd-pulse/delay 0.05
  ""
  :group 'dmd-pulse)

(defcustom dmd-pulse/iterations 20
  ""
  :group 'dmd-pulse)

(defun dmd-pulse/pulse-line (&optional line)
  (interactive "P")
  (save-excursion
    (when line
      (goto-char (point-min))
      (forward-line (1- line)))
    (dmd-pulse/pulse-region (point-at-bol) (point-at-eol))))

(defun dmd-pulse/pulse-region (start end)
  (lexical-let* ((overlay (make-overlay start end))
                 (iteration 0)
                 (bg-end (color-values (face-background 'default)))
                 (bg-start (color-values (face-background 'dmd-pulse/face)))
                 (frac (list (/ (- (nth 0 bg-end) (nth 0 bg-start)) dmd-pulse/iterations)
                             (/ (- (nth 1 bg-end) (nth 1 bg-start)) dmd-pulse/iterations)
                             (/ (- (nth 2 bg-end) (nth 2 bg-start)) dmd-pulse/iterations)))
                timer)
    (setq timer (run-with-timer 0 dmd-pulse/delay
                                (lambda (&rest args)
                                  (cond ((= iteration dmd-pulse/iterations)
                                         (delete-overlay overlay)
                                         (cancel-timer timer))
                                        (t
                                         (overlay-put overlay 'face
                                                      (list :background
                                                            (pulse-color-values-to-hex
                                                             (list (+ (nth 0 bg-start) (* (nth 0 frac) iteration))
                                                                   (+ (nth 1 bg-start) (* (nth 1 frac) iteration))
                                                                   (+ (nth 2 bg-start) (* (nth 2 frac) iteration))))))
                                         (incf iteration))))))
    overlay))

(provide 'dmd-pulse)

;;; dmd-pulse.el ends here
