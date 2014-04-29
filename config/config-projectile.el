;;; config-projectile.el ---

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

(fni/add-to-load-path (concat src-dir "projectile/"))
(require 'projectile)
(projectile-global-mode)

(fni/add-to-load-path (concat src-dir "ag"))
(require 'ag)
(define-key projectile-mode-map (kbd "C-c p a") 'projectile-ag)

(advice-add 'compilation-find-file :around
            (lambda(oldfun &rest args)
              "Try to use `projectile.el' to find a buffer for file FILENAME.
If we cannot find it, fallback to the original function."
              (destructuring-bind (marker filename directory &rest formats) args
                ;; Try to find the filename using projectile
                (let ((root (ignore-errors (projectile-project-root)))
                      file
                      buffer)
                  (when root
                    (dolist (dir (projectile-current-project-dirs))
                      (setq dir (expand-file-name dir root))
                      (setq file (expand-file-name filename dir))
                      (if (file-exists-p file)
                          (setq buffer (find-file-noselect file)))))
                  (or buffer
                      ;; Fall back to the old function `compilation-find-file'
                      (apply oldfun marker filename directory formats)))))
            '((name . compilation-with-projectile-dirs)))

(provide 'config-projectile)

;;; config-projectile.el ends here
