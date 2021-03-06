;;; config-search.el ---

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

(setq dmoccur-list
      '(
        ;;name    directory            mask                option
        ("dir"    default-directory    (".*")              dir)
        ("econf"  "~/.emacs.d"         ("\\.el$")          nil)
        ("reconf" (("~/.emacs.d" t))   ("\\.el$")          nil)
        ("config" "~/src/config"       ("\\.org$")         nil)
        ("elisp"  (("~/src/elisp" t)
                   ("~/.emacs.d" t))   ("\\.el$")          nil)
        ("lisp"   (("~/src/lisp" t))   ("\\.lisp$")        nil)
        ))

(provide 'config-search)

;;; config-search.el ends here
