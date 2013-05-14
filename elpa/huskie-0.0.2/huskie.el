;;; huskie.el --- chainsaw powered logging

;; Copyright (C) 2012  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Keywords: lisp, processes
;; Version: 0.0.2
;; URL: http://github.com/nicferrier/emacs-huskie
;; Package-Requires: ((anaphora "0.0.6"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Make logging through processes.

;; Named after my trusty Huskavana chainsaw. How I love it.

;; Usage:
;;
;; (huskie-log "something" "logger-name")
;;
;; will send "something" to the "logger-name" which will be
;; automatically mapped to a file in "/tmp/logger-name"
;;
;; The automatic file mapping is done with
;; `huskie-log-default-directory' which is a variable you can let-bind
;; or simply change.
;;
;; (let ((huskie-log-default-directory "/home/nictest"))
;;   (hukie-log "just a test" "my-log"))
;;
;; will send "just a test" to the log process "my-log" and create a
;; file "/home/nictest/my-log"
;;
;; The mapping between filenames and lognames can be defined:
;;
;; (huskie-bind-logname->filename "nictest" "/tmp/my-log")
;; (huskie-log "test100!" "nictest")
;;
;; will send "test100!" to the file "/tmp/my-log" though a logging
;; process called "nictest" will be created.
;;
;; All the logging is done through async processes. A default process
;; is used unless something specific exists for the logname. Setting a
;; script can be done with `huskie-set-script'.
;;
;; The script MUST include a single %s to accept the filename. Failure
;; to do so will result in an error.
;;
;; The script MUST be a shell script.
;;
;; (huskie-set-script
;;    "nictest"
;;    "while read line; do logger -f %s $line ; done")
;; (huskie-log "test 200" "nictest")
;;
;; will send "test 200" through "nictest" via the syslog logger
;; program.


;;; Code:

(require 'anaphora)

(defconst huskie/log-default-script
  "while read line ; do echo $line; echo $line >> %s ; done"
  "The default script for handling logging.")

(defconst huskie/log-script-map
  (make-hash-table :test 'equal)
  "Map of lognames to scripts used for logging.

The script should have a single %s in it which should point to
any filename.")

(defconst huskie/logname-file-map
  (make-hash-table :test 'equal)
  "Map of lognames to filenames.

If you want a logname to map to a specific filename put something
in this map via `huskie-bind-logname->filename'.")

(defvar huskie-log-default-directory
  "/tmp"
  "The default directory where log files go.")

(defun huskie-bind-logname->filename (logname filename)
  "Bind LOGNAME to FILENAME so we use that file when we log."
  (puthash logname filename huskie/logname-file-map))

(defun huskie-set-script (logname shell)
  "Specify that SHELL is used to log LOGNAME.

SHELL MUST include a single %s to accept the filename. Failure
to do so will result in an error.

The filename passed to the %s WILL be checked to test for
filenameness and NOT allow espcaping to shell."
  ;; TODO implement the filename checking of %s in make-log-process
  (puthash logname shell huskie/log-script-map))

(defun huskie/logname->proc-name (logname)
  "Make a proc name from a LOGNAME."
  (format "*log-%s*" logname))

(defun huskie/make-log-process (logname &optional filename)
  "Make a log process logging through LOGNAME script.

Optionally use FILENAME as the destination.  If there is no
FILENAME then just derive one through LOGNAME.

The script for logging is either LOGNAME specific via a lookup in
`huskie/log-script-map' or the default log script
`huskie/log-default-script'."
  (let* ((file
          (or filename
              (concat
               (file-name-as-directory huskie-log-default-directory)
               logname)))
         ;; TODO we MUST check `file' is a filename and not:
         ;;
         ;;    /tmp/blah ; rm -rf /
         (log-name (huskie/logname->proc-name logname))
         (buf-name (concat " " log-name))
         (log-script
          (or (gethash logname huskie/log-script-map)
              huskie/log-default-script)))
    ;; Test the file is a filename
    (assert
     (equal file (shell-quote-argument file)) t
     nil "file %s is unsafe" file)
    (start-process-shell-command
     log-name (get-buffer-create buf-name)
     (format log-script filename))))

(defun huskie-log (text logname)
  "Send TEXT to LOGNAME.

If LOGNAME does not have an existing process it is created via
`huskie/make-log-process'."
  (let ((proc
         (or
          (get-process (huskie/logname->proc-name logname))
          (huskie/make-log-process
           logname
           (awhen (gethash logname huskie/logname-file-map) it)))))
    (process-send-string proc (concat text "\n"))))

(provide 'huskie)

;;; huskie.el ends here
