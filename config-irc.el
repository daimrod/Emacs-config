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

(fni/add-to-load-path (concat src-dir "shoes-off/"))
(require 'rcirc)
(require 'shoes-off-log)
(require 'shoes-off)

(defun-rcirc-command reconnect (arg)
  "Reconnect the server process."
  (interactive "i")
  (unless process
    (error "There's no process for this target"))
  (let* ((server (car (process-contact process)))
         (port (process-contact process :service))
         (nick (rcirc-nick process))
         channels query-buffers)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (eq process (rcirc-buffer-process))
          (remove-hook 'change-major-mode-hook
                       'rcirc-change-major-mode-hook)
          (if (rcirc-channel-p rcirc-target)
              (setq channels (cons rcirc-target channels))
            (setq query-buffers (cons buf query-buffers))))))
    (delete-process process)
    (rcirc-connect server port nick
                   rcirc-default-user-name
                   rcirc-default-full-name
                   channels)))

(provide 'config-irc)

;;; config-irc.el ends here
