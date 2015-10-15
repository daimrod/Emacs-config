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

(defun dmd-add-org-capture-template ()
  "Add custom capture template.

This function is can be added to `org-mode-hook'."
  (let ((file (buffer-file-name)))
    (when file
      (setq-local org-capture-templates
                  (append `(("T" "Task in current project" entry
                             (file+headline ,file "Task")
                             "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%a" :prepend t :empty-lines 1)
                            ("J" "New journal entry in current project" entry
                             (file+datetree ,file)
                             "* %?" :immediate-finish t :jump-to-captured t :empty-lines 1 :unnarrowed t))
                          org-capture-templates)))))

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
    ;; put new entry in notes if we don't find it.
    (when (re-search-forward key nil 'end)
      (funcall org-ref-open-notes-function)
      t)))

(defun dmd--update-org-agenda-files ()
  "Update `org-agenda-files' with filename from the org-agenda-files file."
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
- it's a habit (has STYLE=habit)"
  (cond ((or (string= state "MEETING")
             org-capture-mode
             (equal (org-entry-get (point) "STYLE") "habit"))
         state)

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
        (t "NEXT")))

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
    ("fr" . "francais")))

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
  (start-process "org-tangle-async"
                 "*org-tangle-async*"
                 (executable-find "emacs")
                 "-Q" "--batch"
                 "--eval"
                 "(progn
(add-to-list 'load-path (expand-file-name \"~/.emacs.d/modules/org-mode/lisp/\"))
(add-to-list 'load-path (expand-file-name \"~/.emacs.d/modules/org-mode/contrib/lisp/\"))
(require 'org)
(require 'ob)
(require 'ob-tangle))"
                   "--eval"
                   (format "(with-current-buffer (find-file-noselect %S)
(org-babel-tangle))" (buffer-file-name))))

(provide 'dmd-org-mode)

;;; dmd-org-mode.el ends here
