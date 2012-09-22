;;; config-scala.el ---

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

(fni/add-to-load-path (concat src-dir "scala-mode/"))
(require 'scala-mode-auto)

(defvar ensime-dir (concat src-dir "ensime/dist_2.9.2/"))
(fni/add-to-load-path (concat ensime-dir "elisp/"))
(require 'ensime)
(setq-default scala-inf-buffer-name ensime-inf-buffer-name)

;; This step causes the ensime-mode to be started whenever
;; scala-mode is started for a buffer. You may have to customize this step
;; if you're not using the standard scala mode.
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

(setq exec-path (cons (concat ensime-dir "bin/") exec-path))

(provide 'config-scala)

;;; config-scala.el ends here# -*- mode: snippet -*-
