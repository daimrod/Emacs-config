;;; config-python.el ---

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

(defun dmd--autoset-virtualenv ()
  (when (and (derived-mode-p 'python-mode)
             (buffer-file-name))
    (let ((virtualenv-path (if (projectile-project-p)
                               (concat (projectile-project-root) ".virtualenv")
                             (concat (file-name-directory (buffer-file-name)) ".virtualenv"))))
      (setq python-shell-virtualenv-path
            (if (file-exists-p virtualenv-path)
                virtualenv-path
              (when (yes-or-no-p
                     (format "No virtualenv found, would you like to create one? (%s)"
                             virtualenv-path))
                (call-process "virtualenv" nil nil nil "--no-site-packages"
                              virtualenv-path)
                virtualenv-path))))))

(add-hook 'python-mode-hook 'dmd--autoset-virtualenv)

(provide 'config-python)

;;; config-python.el ends here
