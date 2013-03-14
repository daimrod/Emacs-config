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

(require 'hexrgb)
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

(defun dmd-pulse/pulse-line ()
  (interactive)
  (dmd-pulse/pulse-region (point-at-bol) (point-at-eol)))

(defun dmd-pulse/pulse-region (start end)
  (lexical-let ((overlay (make-overlay start end))
                (list-bg (loop with frame = (color-values (face-background 'default))
                               with start = (color-values (face-background 'dmd-pulse/face))
                               for it from 0 to dmd-pulse/iterations

                               for frac = (list (/ (- (nth 0 frame) (nth 0 start)) dmd-pulse/iterations)
                                                (/ (- (nth 1 frame) (nth 1 start)) dmd-pulse/iterations)
                                                (/ (- (nth 2 frame) (nth 2 start)) dmd-pulse/iterations))

                               collect (hexrgb-color-values-to-hex
                                        (list
                                         (+ (nth 0 start) (* (nth 0 frac) it))
                                         (+ (nth 1 start) (* (nth 1 frac) it))
                                         (+ (nth 2 start) (* (nth 2 frac) it))))))
                timer)
    (setq timer (run-with-timer 0 dmd-pulse/delay
                                (lambda (&rest args)
                                  (cond ((null list-bg)
                                         (delete-overlay overlay)
                                         (cancel-timer timer))
                                        (t (overlay-put overlay 'face (list :background (pop list-bg)))
                                           (redisplay))))))))

(provide 'dmd-pulse)

;;; dmd-pulse.el ends here
