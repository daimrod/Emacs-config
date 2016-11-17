;;; dmd-org-mode.el --- My own org-mode utils

;; Copyright (C) 2015 Grégoire Jadi

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

(require 'org-capture)
(require 'org-ref)
(require 'anaphora)
(require 'org-datetree)

(defun dmd--org-capture-headline (headline)
  "Return a function that jumps to HEADLINE in current buffer.

Adapted from `org-capture-set-target-location'."
  (lexical-let ((headline headline))
	(lambda ()
	  (goto-char (point-min))
	  (if (re-search-forward
		   (format org-complex-heading-regexp-format (regexp-quote headline))
		   nil t)
		  (goto-char (point-at-bol))
		(goto-char (point-max))
		(or (bolp) (insert "\n"))
		(insert "* " headline "\n")
		(beginning-of-line 0)))))

(defun dmd--org-capture-datetree ()
  "Jumps to a datetree in current buffer.

Adapted from `org-capture-set-target-location'."
  (org-datetree-find-date-create
   (calendar-gregorian-from-absolute
	(org-today))))

(defun dmd--org-capture-weektree ()
  "Jumps to a weektree in current buffer.

Adapted from `org-capture-set-target-location'."
  (org-datetree-find-iso-week-create
   (calendar-gregorian-from-absolute
	(org-today))))

(defun dmd--org-capture-elfeed ()
  "Jumps to the proper headline in elfeed.org."
  (let ((entry (cond ((eq major-mode 'elfeed-search-mode)
                      (elfeed-search-selected :ignore-region))
                     ((eq major-mode 'elfeed-show-mode)
                      elfeed-show-entry))))
    (when entry
      (let* ((id (elfeed-entry-id entry))
             (feed (first id))
             (url (rest id))
             (title (elfeed-entry-title entry))
			 found?
			 tmp-feed)
		(setq tmp-feed feed)
		(loop for file in rmh-elfeed-org-files
			  do (set-buffer (find-file-noselect file))
			  do (goto-char (point-min))
			  do (setf found?
					   (re-search-forward (format org-complex-heading-regexp-format
												  (format "\\[?\\[?%s]?]?" (regexp-quote feed)))
										  nil t))
			  until found?)
		(if found?
			(goto-char (point-at-bol))
		  (user-error "Couldn't find feed %s" feed))))))

(defun dmd-org-ref-open-bibtex-notes ()
  "From a bibtex entry, open the notes.

Create a heading if the entry doesn't exist.

I never did figure out how to use reftex to make this happen
non-interactively.  the reftex-format-citation function did not
work perfectly; there were carriage returns in the strings, and
it did not put the key where it needed to be.  so, below I replace
the carriage returns and extra spaces with a single space and
construct the heading by hand."
  (interactive)

  (bibtex-beginning-of-entry)
  (let* ((cb (current-buffer))
         (bibtex-entry (buffer-substring (point)
                                         (save-excursion
                                           (bibtex-end-of-entry)
                                           (point))))
         (bibtex-expand-strings t)
         (entry (cl-loop for (key . value) in (bibtex-parse-entry t)
                         collect (cons (downcase key) value)))
         (key (reftex-get-bib-field "=key=" entry)))

    ;; save key to clipboard to make saving pdf later easier by pasting.
    (with-temp-buffer
      (insert key)
      (kill-ring-save (point-min) (point-max)))

    ;; now look for entry in the notes file
    (if  org-ref-bibliography-notes
        (find-file-other-window org-ref-bibliography-notes)
      (error "Org-ref-bib-bibliography-notes is not set to anything"))

    (goto-char (point-min))
    (re-search-forward (format "@.*{%s" key) nil 'end)))

(defun dmd-org-ref-open-bibtex-key-notes (thekey)
  "THEKEY."
  (interactive)
  (let* ((results (org-ref-get-bibtex-key-and-file thekey))
         (key (car results))
         (bibfile (cdr results)))

    ;; look for entry in the notes file
    (if  org-ref-bibliography-notes
        (find-file-other-window org-ref-bibliography-notes)
      (error "Org-ref-bib-bibliography-notes is not set to anything"))

    (goto-char (point-min))
    (re-search-forward (format "@.*{%s" key) nil 'end)))

(defun dmd--update-org-agenda-files ()
  "Update `org-agenda-files' with filename from the org-agenda-files file.

Files found in the `org-agenda-files` file are added to the
`org-agenda-files` variable iff they exist."
  (interactive)
  (let ((sbuf (with-current-buffer (find-file-noselect
                                    (expand-file-name "org-agenda-files" user-emacs-directory))
                (buffer-substring-no-properties (point-min) (point-max)))))
    (setq org-agenda-files nil)
    (dolist (f (s-lines sbuf))
      (if (and (not (string-empty-p f))
               (file-exists-p f))
          (add-to-list 'org-agenda-files f)))))

;;; Nicolas Goaziou, http://article.gmane.org/gmane.emacs.orgmode/67692
(defun org-latex-ignore-heading-filter-headline (headline backend info)
  "Strip headline from HEADLINE.  Ignore BACKEND and INFO.

This function should be added to `org-export-filter-headline-functions'."
  (when (and (org-export-derived-backend-p backend 'latex)
             (string-match "\\`.*ignoreheading.*\n" headline))
    (replace-match "" nil nil headline)))

(defun dmd--org-apply-to-headlines (function &optional prefix)
  "Apply FUNCTION to each headline in the current buffer.

The PREFIX argument is passed to the function."
  (when (and (derived-mode-p 'org-mode)
             (buffer-file-name)
             (org-agenda-file-p))
    (let ((scope (when prefix
                   (message "Process headlines for the [b]uffer, [t]ree, [r]egion, [f]ile, [F]ile with archives, [a]gende, [A]genda with archives?")
                   (let ((r (read-char-exclusive)))
                     (cond ((char-equal r ?b) nil)
                           ((char-equal r ?t) 'tree)
                           ((char-equal r ?r) 'region)
                           ((char-equal r ?f) 'file)
                           ((char-equal r ?F) 'file-with-archives)
                           ((char-equal r ?a) 'agenda)
                           ((char-equal r ?A) 'agenda-with-archives))))))
      (save-excursion
        (ignore-errors (outline-up-heading 42))
        (org-map-entries function t scope)))))

(defun dmd-org-add-ids-to-headlines (&optional prefix)
  "Add ID properties to all headlines in the current buffer.

PREFIX is used to determine the scope."
  (interactive "P")
  (dmd--org-apply-to-headlines 'org-id-get-create prefix))

(defun dmd-org-add-CREATED-to-headlines (&optional prefix)
  "Add \"CREATED\" properties to all headlines in the current buffer.

PREFIX is used to determine the scope."
  (interactive "P")
  (dmd--org-apply-to-headlines 'dmd-org-add-created-prop-if-none prefix))

(defun dmd-org-clock-in-switch-to-state (state)
  "Switch to \"NEXT\" state unless:
- we are in `org-capture-mode'
- if the STATE is \"MEETING\"
- it's a habit (has STYLE=habit or habit \in FILETAG)"
  (cond ((or (string= state "MEETING")
             org-capture-mode
             (equal (org-entry-get (point) "STYLE") "habit")
			 (find "habit" (org-get-tags-at (point)) :test #'string=))
         state)
        (t "NEXT")))

(defun dmd-org-add-created-prop-if-none ()
  "Add a \"CREATED\" properties if none exists."
  (unless (org-entry-get (point) "CREATED")
    (org-set-property "CREATED" (format-time-string "[%Y-%m-%d %a %H:%M]" (org-read-date nil 'totime "today")))))

(defcustom org-agenda-skip-tags nil
  "Tags that should be excluded even if they have a SCHEDULED or DEADLINE property."
  :type '(repeat string)
  :group 'org-agenda)

(defun dmd-org-agenda-skip-tags-entry ()
  "Skip tags found in `org-agenda-skip-tags'."
  (when (find-if (lambda (s)
                   (find s (org-get-tags-at (point)) :test #'string=))
                 org-agenda-skip-tags)
    (org-end-of-subtree t)
    (point)))

(defun dmd-org-set-effort ()
  "Set an effort unless :
- we are in `org-capture-mode'
- there is already an effort"
  (unless (or org-capture-mode
              (org-entry-get (point) "Effort"))
    (org-set-effort)))

(defun dmd-org-active-timestamp-to-inactive-when-rescheduled ()
  "Convert some variable with active timestamp to inactive one."
  (when (eq org-log-note-purpose 'reschedule)
    (setq org-log-note-previous-state
          (substitute ?\] ?\> (substitute ?\[ ?\< org-log-note-previous-state :test #'char-equal)
                      :test #'char-equal))))

(defun dmd-org-indent-buffer ()
  "Indent current buffer."
  (interactive)
  (if (not (derived-mode-p 'org-mode))
      (user-error "Buffer isn't in Org-Mode")
    (org-content)
    (save-excursion
      (goto-char (point-min))
      (while (zerop (forward-line 1))
        (ignore-errors (org-indent-drawer))
        (org-indent-line)))))

(defun dmd-org-skip-bib-file ()
  "Skip bib file.

Can be used in as agenda skip function or when refiling."
  (if (not (file-equal-p org-ref-bibliography-notes (buffer-file-name)))
      t
    (goto-char (point-max))
    nil))

(defun dmd-org-skip-contacts-files ()
  "Skip contacts files.

Can be used in as agenda skip function or when refiling."
  (if (not (find (buffer-file-name) (org-contacts-files) :test #'file-equal-p))
      t
    (goto-char (point-max))
    nil))

(defvar dmd-org-lang-to-ispell-dict
  '(("en" . "english")
    ("fr" . "francais"))
  "Translate org-mode's language to Ispell dictionaries.

Org-mode languages are listed in `org-clock-clocktable-language-setup'.
Ispell dictionaries are listed in `ispell-dictionary-base-alist'.")

(defun dmd-set-ispell-dictionary-from-org ()
  "Infer current language from current org-mode's buffer.

The language can be specified with the #+LANGUAGE export option.
Use it."
  (let ((lang (plist-get (org-export--get-inbuffer-options) :language)))
    (awhen (and lang
                (assoc lang dmd-org-lang-to-ispell-dict))
      (setq ispell-local-dictionary (rest it)))))

(defun dmd-org-babel-tangle-async ()
  "Tangle current buffer asynchronously."
  (interactive)
  (start-process-shell-command
   "org-tangle-async"
   "*org-tangle-async*"
   (format "%s %s"
           (expand-file-name "~/.emacs.d/elisp/tangle.sh")
           (buffer-file-name))))

;;; Org Drill
(defun org-drilll (&optional scope)
  (interactive
   (list (intern
		  (completing-read "Score: "
						   '(file tree agenda directory)))))
  (org-drill scope))

;;;; Always Clocking, always !
(defgroup dmd-always-clocking nil
  "Customize Always Clocking behavior."
  :tag "Always Clocking"
  :group 'dmd-always-clocking)

;;; Customize Always Clocking
(defcustom dmd-always-clocking-timeout (* 60 2)
  "2 minutes before the alert!"
  :type 'int
  :group 'dmd-always-clocking)

(defcustom dmd-always-clocking-message
  "You're not clocking!\nWhat the f*ck are you doing"
  "How would you like to be reminded that you should clock?"
  :type 'string
  :group 'dmd-always-clocking-message)

;;; Internal variables
(defvar dmd-always-clocking-last-check (current-time)
  "When was the last time we checked if you were clocking?")
(defvar dmd-always-clocking-on t
  "Are you clocking?")

;;; Internal Functions
(defun dmd-always-clocking-check ()
  "Check whether we're clocking or not, and act if necessary!"
  (when dmd-always-clocking-on
	(if (org-clocking-p)
		(setq dmd-always-clocking-last-check (current-time))
	  (when (time-less-p (time-add dmd-always-clocking-last-check
								   dmd-always-clocking-timeout)
						 (current-time))
		(org-notify dmd-always-clocking-message)))))

(defun dmd-always-clocking-on ()
  "Enable always-clocking."
  (interactive)
  (setq dmd-always-clocking-on t))

(defun dmd-always-clocking-off ()
  "Disable always clocking."
  (interactive)
  (setq dmd-always-clocking-on nil))

(defun dmd-org-check-agenda-file (file)
  "Make sure FILE exists.  If not, ask user what to do."
  (unless (file-exists-p file)
    (message "Non-existent agenda file %s.  [R]emove from list, [A]bort or [S]kip?"
             (abbreviate-file-name file))
    (let ((r (downcase (read-char-exclusive))))
      (cond
       ((equal r ?r)
        (org-remove-file file)
        (throw 'nextfile t))
       ((equal r ?s)
        (throw 'nextfile t))
       (t (user-error "Abort"))))))

(provide 'dmd-org-mode)

;;; dmd-org-mode.el ends here
