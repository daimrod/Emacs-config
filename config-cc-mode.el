;; config-cc-mode.el
;; Copyright (C) 2011, 2012 Grégoire Jadi

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

(setq-default c-basic-offset 4
              tab-width 4 ; or any other preferred value
              cua-auto-tabify-rectangles nil
              compilation-window-height 10)

(setq c-default-style
      '((java-mode . "java")
        (awk-mode . "awk")
        (c-mode . "k&r")
        (other . "linux")))

(provide 'config-cc-mode)
