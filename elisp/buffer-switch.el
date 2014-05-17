;;; buffer-switch.el ---

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

(defvar before-buffer-switch-hook nil)

(defadvice switch-to-buffer (before save-buffer-now activate)
  (when buffer-file-name (run-hooks 'before-buffer-switch-hook)))
(defadvice other-window (before other-window-now activate)
  (when buffer-file-name (run-hooks 'before-buffer-switch-hook)))
(defadvice windmove-up (before other-window-now activate)
  (when buffer-file-name (run-hooks 'before-buffer-switch-hook)))
(defadvice windmove-down (before other-window-now activate)
  (when buffer-file-name (run-hooks 'before-buffer-switch-hook)))
(defadvice windmove-left (before other-window-now activate)
  (when buffer-file-name (run-hooks 'before-buffer-switch-hook)))
(defadvice windmove-right (before other-window-now activate)
  (when buffer-file-name (run-hooks 'before-buffer-switch-hook)))

(defvar after-buffer-switch-hook nil)
(defadvice switch-to-buffer (after save-buffer-now activate)
  (when buffer-file-name (run-hooks 'after-buffer-switch-hook)))
(defadvice other-window (after other-window-now activate)
  (when buffer-file-name (run-hooks 'after-buffer-switch-hook)))
(defadvice windmove-up (after other-window-now activate)
  (when buffer-file-name (run-hooks 'after-buffer-switch-hook)))
(defadvice windmove-down (after other-window-now activate)
  (when buffer-file-name (run-hooks 'after-buffer-switch-hook)))
(defadvice windmove-left (after other-window-now activate)
  (when buffer-file-name (run-hooks 'after-buffer-switch-hook)))
(defadvice windmove-right (after other-window-now activate)
  (when buffer-file-name (run-hooks 'after-buffer-switch-hook)))

(provide 'buffer-switch)

;;; buffer-switch.el ends here
