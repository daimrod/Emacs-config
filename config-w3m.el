;;; config-w3m.el ---

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

(setq w3m-command-arguments
      '("-o" "http_proxy=http://localhost:8250/"))

(flet ((w3m-add-search-engine-with-quickshort
        (search-engine)
        (let ((name (elt search-engine 0))
              (url (elt search-engine 1)))
          (add-to-list 'w3m-search-engine-alist
                       (list name url))
          (add-to-list 'w3m-uri-replace-alist
                       (list (concat "\\`" name ":")
                             'w3m-search-uri-replace
                             name)))))
  (mapc #'w3m-add-search-engine-with-quickshort
        '(("enfr" "http://www.wordreference.com/enfr/%s")
          ("fren" "http://www.wordreference.com/fren/%s")
          ("seeks" "http://s.s/search?q=%s"))))

(provide 'config-w3m)

;;; config-w3m.el ends here
