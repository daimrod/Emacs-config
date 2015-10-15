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
