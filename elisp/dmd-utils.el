;;; dmd-utils.el ---

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

;;; Tests in `test-equals.el'
(defun equal* (o1 o2 &optional test)
  "Compare O1 and O2 without breaking on circular lists.

Atoms are compared with TEST if it is supplied or else `equal'."
  ;; The trick used to avoid endless loop on circular lists is to
  ;; store (1) a reference of each sublist scanned with its index for
  ;; both objects. That way, if the same reference is encountered (2)
  ;; we know we're in a circular list, then we just need to compare
  ;; (3) the index of this reference for both objects.
  (setf test (or test 'equal))
  (labels ((%equal* (o1 o2 start ht1-mem ht2-mem)
                    (if (not (listp o1))
                        (funcall test o1 o2)
                      (if (not (listp o2))
                          nil
                        (loop
                         named loop
       
                         for l1 = o1 then (cdr l1)
                         for l2 = o2 then (cdr l2)

                         for index upfrom start
                         for previous = (gethash l1 ht1-mem)

                         do (cond
                             ((or (null l1) (null l2)) ; proper list
                              (return-from loop (and (null l1) (null l2))))
                             ((or (atom l1) (atom l2)) ; dotted list
                              (return-from loop (%equal* l1 l2 index ht1-mem ht2-mem)))
                             (previous
                              ;; circular list (2)
                              (return-from loop (= previous (gethash l2 ht2-mem -1)))) ; (3)
                             (t
                              ;; (1) store the tails of both objects
                              (setf (gethash l1 ht1-mem) index
                                    (gethash l2 ht2-mem) index)
                              (unless (%equal* (car l1) (car l2) index ht1-mem ht2-mem)
                                (return-from loop nil)))))))))
    (%equal* o1 o2 0 (make-hash-table) (make-hash-table))))

(provide 'dmd-utils)

;;; dmd-utils.el ends here
