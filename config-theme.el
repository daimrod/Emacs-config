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
(add-to-list 'default-frame-alist '(font . "-unknown-Inconsolata-normal-normal-normal-*-20-*-*-*-m-0-iso10646-1"))

(setq initial-frame-alist (append initial-frame-alist (copy-alist default-frame-alist)))

;; tab and indentation configuration
(setq-default indent-tabs-mode nil)
(setq tab-width 4)

(global-font-lock-mode 1)
(setq font-lock-maximum-decoration t
      font-lock-verbose nil)

(provide 'config-theme)
