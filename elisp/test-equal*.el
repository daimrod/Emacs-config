;;; test-equal*.el ---

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

(require 'ert)
(require 'dmd-utils)

(ert-deftest dmd-utils-equal* ()
  (should (equal* 'a 'a))

  (should-not (equal* 'a 'b))
  (should-not (equal* 'b 'a))

  (should (equal* '(a b) '(a b)))
  (should (equal* nil nil))
  (should (equal* '(a (b (c))) '(a (b (c)))))

  (should-not (equal* '(a (b (c))) '(a (b (d)))))
  (should-not (equal* '(a (b (d))) '(a (b (c)))))
  
  (should (equal* '(a . b) '(a . b)))
  (should (equal* '(a (b (c . d))) '(a (b (c . d)))))

  (should-not (equal* '(a . b) '(a . c)))
  (should-not (equal* '(a . c) '(a . b)))

  (should (equal* '#1= (a . #1#) '#1= (a . #1#)))

  (should-not (equal* '#1= (a . #1#) '#1= (b . #1#)))
  (should-not (equal* '#1= (b . #1#) '#1= (a . #1#)))

  (should-not (equal* '#1= (a b c . #1#) '(a b c a b c a b c)))
  (should-not (equal* '(a b c a b c a b c) '#1= (a b c . #1#)))

  (should (equal* '#1= (a b c (a b (c d) . #1#))
                  '#1= (a b c (a b (c d) . #1#))))

  (should (equal* '#1= (a #1#) '#1= (a #1#)))
  (should (equal* '#1= (a #1# b) '#1= (a #1# b)))
  (should (equal* '#1= (a b #1# c . d) '#1= (a b #1# c . d)))

  (should-not (equal* '#1= (a #1# b) '#1= (a #1#) ))
  (should-not (equal* '#1= (a #1#) '#1= (a #1# b)))

  (should-not (equal* '#1= (a #1# . b) '#1= (a #1#)))
  (should-not (equal* '#1= (a #1#) '#1= (a #1# . b)))

  (should-not (equal* '#1= (a #1#) '(a (a a a a a a))))
  (should-not (equal* '(a (a a a a a a)) '#1=(a #1#)))
  
  (should (equal* '#1= (a b (c d #1#) #1# . #1#)
                  '#1= (a b (c d #1#) #1# . #1#)))

  (should-not (equal* '#1= (a b (c d #1#) #1# . #1#)
                      '#1= (a b (c d #1#) #1# a)))
  (should-not (equal* '#1= (a b (c d #1#) #1# a)
                      '#1= (a b (c d #1#) #1# . #1#)))

  (should-not (equal* '#1= (a b (c d #1#) #1# . #1#)
                      '#1= (a b (c d #1#) #1# . a)))
  (should-not (equal* '#1= (a b (c d #1#) #1# . a)
                      '#1= (a b (c d #1#) #1# . #1#)))
  
  (should (equal* '#1= (a b #2= (c d #2#) #2# . #1#)
                  '#1= (a b #2= (c d #2#) #2# . #1#)))
  
  (should-not (equal* '#1= (a b #2= (c d #2# c) #2# . #1#)
                      '#1= (a b #2= (c d #2#) #2# . #1#)))
  (should-not (equal* '#1= (a b #2= (c d #2#) #2# . #1#)
                      '#1= (a b #2= (c d #2# c) #2# . #1#)))

  (should-not (equal* '#1= (a b #2= (c d #2# . c) #2# . #1#)
                      '#1= (a b #2= (c d #2#) #2# . #1#)))
  (should-not (equal* '#1= (a b #2= (c d #2#) #2# . #1#)
                      '#1= (a b #2= (c d #2# . c) #2# . #1#)))

  (should-not (equal* '#1= (a b #2= (c d #2# . c) #2# . #1#)
                      '#1= (a b #2= (c d c d . c) #2# . #1#)))
  (should-not (equal* '#1= (a b #2= (c d c d . c) #2# . #1#)
                      '#1= (a b #2= (c d #2# . c) #2# . #1#))))

(provide 'test-equal*)

;;; test-equal*.el ends here
