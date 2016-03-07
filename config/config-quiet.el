;;; config-quiet.el ---

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

(define-minor-mode centerize-mode
  "Centerize a buffer.

Centerize mode is a buffer-local minor mode."
  :lighter " Cent"
  (if centerize-mode
      (progn
        (let ((margin (/ (window-body-width) 4)))
          (setf left-margin-width margin
				right-margin-width margin)
          (set-window-margins (get-buffer-window) margin margin)))
    (setf left-margin-width 0
		  right-margin-width 0)
    (set-window-margins (get-buffer-window) 0 0)))

(defvar saved-mode-line-format nil)
(make-variable-buffer-local 'saved-mode-line-format)

(define-minor-mode mode-line-mode
  "Toggle display of the mode line for the current buffer.

Mode Line mode is a local minor mode."
  :init-value t
  :lighter ""
  (if mode-line-mode
      (setf mode-line-format (or saved-mode-line-format mode-line-format))
    (setf saved-mode-line-format mode-line-format
          mode-line-format nil)))

(defvar saved-header-line-format nil)
(make-variable-buffer-local 'saved-header-line-format)
(define-minor-mode header-line-mode
  "Toggle display of the header line for the current buffer.

Header Line mode is a local minor mode."
  :lighter ""
  (if header-line-mode
      (setf header-line-format (or saved-header-line-format header-line-format))
    (setf saved-header-line-format header-line-format
          header-line-format nil)))

(defcustom quiet-no-view-mode
  '(Info-mode gnus-article-mode)
  "*Major modes where `view-mode' shouldn't be enabled."
  :group 'quiet
  :type '(repeat variable))

(define-minor-mode quiet-mode
  "Read text without visual noise.

Quiet is a local minor mode."
  :lighter " Quiet"
  (if quiet-mode
      (progn
        (header-line-mode -1)
        (mode-line-mode -1)
        (centerize-mode 1)
        (unless (member major-mode quiet-no-view-mode)
          (view-mode 1)
          (ad-activate 'view-mode-disable)))
    
    (header-line-mode 1)
    (mode-line-mode 1)
    (centerize-mode -1)

    ;; Remove the advice _before_ disabling `view-mode' to avoid an
    ;; endless loop.
    (unless (member major-mode quiet-no-view-mode)
      (ad-deactivate 'view-mode-disable)
      (view-mode -1))))

(defadvice view-mode-disable (after disable-quiet-mode)
  "Disable `quiet-mode' when `view-mode' is disabled."
  (quiet-mode -1))

(add-hook 'view-mode-hook
          (lambda ()
            ;; In `view-mode', `view-mode-map' overrides other minor
            ;; mode maps.
            (pushnew (cons 'view-mode view-mode-map)
                     minor-mode-overriding-map-alist
                     :key #'car)))

(provide 'config-quiet)

;;; config-quiet.el ends here
