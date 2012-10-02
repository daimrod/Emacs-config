;;; config-license.el ---

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

(require 'xlicense)

(require 'skeleton)
;;; redefined skeleton (original in copyright.el)
(define-skeleton copyright
  "Insert a copyright by $ORGANIZATION notice at cursor."
  nil
  comment-start
  " Copyright (C) " `(format-time-string "%Y") " by "
  (or (getenv "ORGANIZATION")
      user-full-name)
  '(if (copyright-offset-too-large-p)
       (message "Copyright extends beyond `copyright-limit' and won't be updated automatically."))
  comment-end \n
  comment-start
  " See the file LICENSE for copying permission."
  comment-end \n)

(provide 'config-license)

;;; config-license.el ends here
