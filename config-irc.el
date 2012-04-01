;;; config-irc.el ---

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

(fni/add-to-load-path (concat src-dir "erc/"))
(require 'erc)

(fni/add-to-load-path (concat src-dir "znc/"))
(require 'znc)

(setq erc-timestamp-format "[%H:%M] "
      erc-fill-prefix      "        ")

(make-variable-buffer-local 'erc-fill-column)
(add-hook 'window-configuration-change-hook 
          '(lambda ()
             (save-excursion
               (walk-windows
                (lambda (w)
                  (let ((buffer (window-buffer w)))
                    (set-buffer buffer)
                    (when (eq major-mode 'erc-mode)
                      (setq erc-fill-column (- (window-width w) 2)))))))))

(provide 'config-irc)

;;; config-irc.el ends here# -*- mode: snippet -*-
