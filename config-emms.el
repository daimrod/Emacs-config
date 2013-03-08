;;; config-emms.el ---

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

(defvar emms-dir (concat src-dir "emms/"))
(fni/add-to-load-path (concat emms-dir "lisp/"))
(require 'emms-setup)
(emms-standard)
(emms-default-players)
(require 'emms-player-mplayer-quiet)
(add-to-list 'emms-player-list 'emms-player-mplayer-quiet)
(add-to-list 'emms-player-list 'emms-player-mplayer-quiet-playlist)

(defadvice emms-start (after emms-show-track (&rest args) activate)
  (emms-show))

(provide 'config-emms)

;;; config-emms.el ends here
