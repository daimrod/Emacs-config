;; config-theme.el
;; Copyright (C) 2011 Grégoire Jadi

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

(require 'color-theme)
(color-theme-initialize)
(setq color-theme-is-global t
      color-theme-is-cumulative t
      color-theme-load-all-themes nil)

(fni/add-to-load-path (concat src-dir "color-theme-tangotango/"))
(require 'color-theme-tangotango)
(color-theme-tangotango)

;; Set the default font
(set-face-attribute 'default nil
                    :family "Bitstream Vera Sans Mono"
                    :height 135
                    :width 'expanded)

(defun fontify-frame (frame)
  (set-frame-parameter frame 'font "-bitstream-Bitstream Vera Sans Mono-normal-normal-normal-*-16-*-*-*-m-0-iso10646-1"))

(pushnew #'fontify-frame
         after-make-frame-functions)

;; tab and indentation configuration
(setq-default indent-tabs-mode nil)
(setq tab-width 4)

(global-font-lock-mode 1)
(setq font-lock-maximum-decoration t)

(provide 'config-theme)
