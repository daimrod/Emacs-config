;;; init-org-async.el ---

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

(load "~/.emacs.d/init.el")

;; This needs to be done in `after-init-hook' to override
;; packages provided by ELPA.
(fni/add-to-load-path src-dir t t)

(setf after-init-hook nil)
(mapc (lambda (module)
        (message "Loading %s" module)
        ;; (require module)
        (unless (ignore-errors (require module))
          (warn "Failed to load module `%s'" module)))
      '(org
        ox
        ox-latex
        ox-beamer
        ox-md
        ox-reveal
        config-org))

(provide 'init-org-async)

;;; init-org-async.el ends here
