;;; config-java.el ---

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

(fni/add-to-load-path (concat src-dir "javadoc-lookup/"))
(add-hook 'java-mode-hook (lambda () (local-set-key (kbd "C-h j") 'javadoc-lookup)))
(require 'javadoc-lookup)
(require 'maven-fetch)
(javadoc-add-roots "/usr/share/doc/openjdk-6-jdk/api")

(provide 'config-java)

;;; config-java.el ends here