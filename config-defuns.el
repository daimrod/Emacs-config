;; config-defuns.el
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

(require 'http-post-simple)
(require 'thingatpt)
(require 'imenu)

(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(defun sudo-edit (&optional arg)
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:localhost:" buffer-file-name))))

(defun sbrk-paste ()
  "paste a chunk of code to pastebin.sbrk.org"
  (interactive)
  (let* ((region-active-p (region-active-p))
         (start (if region-active-p (region-beginning) (buffer-end -1)))
         (end (if region-active-p (region-end) (buffer-end 1))))
    (http-post-simple "http://pastebin.sbrk.org/add"
                      `((author ,(getenv "USER"))
                        (code ,(buffer-substring start end))))))

(defun zap-to-char- (arg char)
  "Just like zap-to-char- but do not chop the last CHAR."
  (interactive "p\ncZap to char-: ")
  ;; Avoid "obsolete warnings for translation-table-for-input.
  (with-no-warnings
    (if (char-table-p translation-table-for-input)
        (setq char (or (aref translation-table-for-input char) char))))
  (save-excursion
    (kill-region (point)
                 (progn
                   (search-forward (char-to-string char) nil nil arg)
                   (if (>= arg 0)
                       (- (point) 1)
                     (+ (point) 1))))))

(defun dmd/recompile-config ()
  (interactive)
  (map nil (lambda (file)
             (if (file-exists-p file)
                 (byte-compile-file file)))
       '("~/.gnus.el"))
  (byte-recompile-directory dotfiles-dir 0))

(defun move-to-window-line-top ()
  (interactive)
  (move-to-window-line 0))

(defun move-to-window-line-bottom ()
  (interactive)
  (move-to-window-line -1))

(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))

(defun dmd/small-scroll-up-command (&optional arg)
  (interactive "^P")
  (let ((fun-scroll-up (if (fboundp 'scroll-up-command)
                       'scroll-up-command
                     'scroll-up)))
   (if arg
       (funcall fun-scroll-up arg)
     (funcall fun-scroll-up 5))))

(defun dmd/small-scroll-down-command (&optional arg)
  (interactive "^P")
  (let ((fun-scroll-down (if (fboundp 'scroll-down-command)
                             'scroll-down-command
                           'scroll-down)))
   (if arg
       (funcall fun-scroll-down arg)
     (funcall fun-scroll-down 5))))

(defun dmd/show-big-text (text &optional size font)
  (interactive "sText to show: ")
  (let ((size (number-to-string
               (if (null size)
                   (window-width)
                 size)))
        (font (if (null font)
                  "doh"
                font)))
    (shell-command (format "figlet -w %s -f %s %s"
                           size font text))))

(defun dmd/client-process ()
  "Returns the process associated with the current emacsclient"
  (when (boundp 'server-clients)
    (loop for process in server-clients
          when (eq (selected-frame)
                   (process-get process 'frame))
          return process)))

(defvar dmd/dead-clients nil
  "List of dead clients that should be destroyed.")

(defun dmd/quit-or-hide (rly?)
  "If it this is an instance of a running Emacs daemon, then
if it's the last frame, hide it, otherwise delete it.

If not, use the classic save-buffers-and-kill-emacs function."
  (interactive "P")
  (if (and (boundp 'server-process) (not (null server-process)) (null rly?))
	  (progn
        (condition-case nil
            (make-frame-invisible nil t)
          (error (delete-frame)))
        (add-to-list 'dmd/dead-clients (dmd/client-process)))
    (save-buffers-kill-emacs)))

(defun dmd/delete-zombie-clients (frame)
  "Delete zombie clients, that is, emacsclient that are finished
but still present in the background."
  (when (fboundp 'server-delete-client)
   (dolist (process dmd/dead-clients)
     (server-delete-client process))))

(add-hook 'after-make-frame-functions #'dmd/delete-zombie-clients)

(defun dmd/autocompile ()
  "Byte compile an elisp."
  (interactive)
  (require 'bytecomp)
  (let ((filename (buffer-file-name)))
    (if (string-match "\\.el$" filename)
        (byte-compile-file filename))))

(defvar *evince-extensions* nil
  "List of extentions supported by evince.")
(setf *evince-extensions* '("pdf" "ps" "dvi"))

(defvar *evince-location* "/usr/bin/evince"
  "Where is evince?")

(defun dmd/evince (filename)
  "Open the given FILENAME with evince in a background.

`ido-read-file-name' is used to find the files. Extensions can be
added to `*evince-extensions*'."
  (interactive
   (let* ((exts (copy-list *evince-extensions*))
          (ext (do* ((ret (concat "\\(" (pop exts)) (concat ret "\\|" ext))
                     (ext (pop exts) (pop exts)))
                   ((null exts) (concat ret "\\)")))))
     (list (ido-read-file-name
            "File: " nil nil
            nil
            (ffap-file-at-point)
            '(lambda (filename)
               (and (file-exists-p filename)
                    (or
                     (string= "~" filename)
                     (file-directory-p filename)
                     (string-match (format "^.*\.%s$" ext) filename))))))))
  (shell-command (format "evince \"%s\" & disown" (expand-file-name filename)))
  (message "%s" filename))

(defvar comint-buffer-minimum-size 0)
(defun dmd/comint-truncate-buffer (&optional n)
  "Does what comint-truncate-buffer should do. That is, truncate
the buffer to keep N lines.

If N is not set, use `comint-buffer-minimum-size'."
  (interactive "P")
  (let ((comint-buffer-maximum-size
         (or n
             comint-buffer-minimum-size)))
    (comint-truncate-buffer)))

(defun equal* (o1 o2)
  "Compare O1 and O2 without breaking on circular lists."
  (cl-labels ((%equal* (o1 o2 start ht1-mem ht2-mem)
                       (if (not (listp o1))
                           (equal-including-properties o1 o2)
                         (if (not (listp o2))
                             nil
                           (loop
                            named loop
       
                            for l1 = o1 then (cdr l1)
                            for l2 = o2 then (cdr l2)

                            for index upfrom start
                            for previous = (gethash l1 ht1-mem)

                            do (cond
                                ((or (null l1) (null l2)) ; proper list
                                 (return-from loop (and (null l1) (null l2))))
                                ((or (atom l1) (atom l2)) ; dotted list
                                 (return-from loop (%equal* l1 l2 index ht1-mem ht2-mem)))
                                (previous
                                 ;; circular list
                                 (return-from loop (= previous (gethash l2 ht2-mem -1))))
                                (t
                                 ;; store the tails of both objects
                                 (setf (gethash l1 ht1-mem) index
                                       (gethash l2 ht2-mem) index)
                                 (unless (%equal* (car l1) (car l2) index ht1-mem ht2-mem)
                                   (return-from loop nil)))))))))
    (%equal* o1 o2 0 (make-hash-table) (make-hash-table))))


(provide 'config-defuns)
