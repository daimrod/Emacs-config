;;; config-cedet.el ---

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

;; http://alexott.net/en/writings/emacs-devenv/EmacsCedet.html

(setf semantic-default-submodes
      '(global-semanticdb-minor-mode
        global-semantic-mru-bookmark-mode
        global-semantic-highlight-func-mode
        global-semantic-stickyfunc-mode
        global-semantic-idle-local-symbol-highlight-mode
        global-semantic-idle-scheduler-mode
        global-semantic-idle-summary-mode))

(semantic-mode 1)
(require 'semantic/ia)
(require 'semantic/bovine/gcc)

(setq-mode-local c-mode semanticdb-find-default-throttle
                 '(project unloaded system recursive))

(setf semantic-idle-scheduler-idle-time 1)

(provide 'config-cedet)

;;; config-cedet.el ends here
