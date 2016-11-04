;;; dmd-utils.el ---

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

;;; Tests in `test-equals.el'
(defun equal* (o1 o2 &optional test)
  "Compare O1 and O2 without breaking on circular lists.

Atoms are compared with TEST if it is supplied or else `equal'."
  ;; The trick used to avoid endless loop on circular lists is to
  ;; store (1) a reference of each sublist scanned with its index for
  ;; both objects. That way, if the same reference is encountered (2)
  ;; we know we're in a circular list, then we just need to compare
  ;; (3) the index of this reference for both objects.
  (setf test (or test 'equal))
  (cl-labels ((%equal* (o1 o2 start ht1-mem ht2-mem)
                       (if (not (listp o1))
                           (funcall test o1 o2)
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
                                 ;; circular list (2)
                                 (return-from loop (= previous (gethash l2 ht2-mem -1)))) ; (3)
                                (t
                                 ;; (1) store the tails of both objects
                                 (setf (gethash l1 ht1-mem) index
                                       (gethash l2 ht2-mem) index)
                                 (unless (%equal* (car l1) (car l2) index ht1-mem ht2-mem)
                                   (return-from loop nil)))))))))
    (%equal* o1 o2 0 (make-hash-table) (make-hash-table))))

(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line
of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun dmd-window-width (&optional window)
  "Like `window-width' except that it takes into account text
scaling."
  (setq window (or window (selected-window)))
  (with-current-buffer (window-buffer window)
    (let ((amount (if (boundp 'text-scale-mode-amount)
                      (- text-scale-mode-amount)
                    0))
          (step (if (boundp 'text-scale-mode-step)
                    text-scale-mode-step
                  1)))
      (truncate
       (* (window-width window)
          (expt step amount))))))

(defun dmd-switch-git<->https ()
  "Switch the current remote from git to https or the other way
around depending on the current value.

It uses magit internal."
  (interactive)
  (let* ((remote (magit-get-current-remote))
         (remote-url (magit-get "remote" remote "url")))
    (magit-set
     (cond ((string-match "^git@" remote-url)
            (format
             "https://%s"
             (substitute ?/ ?: (substring remote-url 4) :count 1)))
           ((string-match "^https://" remote-url)
            (format
             "git@%s"
             (substitute ?: ?/ (substring remote-url 8) :count 1)))
           (t (error "Unknown remote URL format `%s'" remote-url)))
     "remote" remote "url")))

(defun dmd-rename-buffer (&optional unique)
  (interactive "P")
  (let ((new-name (read-from-minibuffer "Rename buffer: " (buffer-name))))
    (rename-buffer new-name unique)))

(defun dmd-text-properties (&optional start end)
  "Add properties in the current active region."
  (interactive "r")
  (unless (use-region-p)
    (error "No active region"))
  (add-face-text-property start end (read (read-from-minibuffer "Property: "))))

(defsubst /. (dividend &rest divisors)
  "Like `/' but uses floating number by coercing the DIVIDEND to
float."
  (apply #'/ (coerce dividend 'float) divisors))

(defun dmd-text-mode-setup ()
  (interactive)
  (visual-line-mode 1)
  (require 'adaptive-wrap)
  (adaptive-wrap-prefix-mode 1))

(defun dmd-org-mode-reftex-setup ()
  (interactive)
  (when (and (derived-mode-p 'org-mode)
             (buffer-file-name))
    (setq-local tex-main-file (buffer-file-name))
    (reftex-set-cite-format "[[bib:%l][%2a (%y)]]")
    (setq-local reftex-cite-punctuation '(", " " and " " et al."))
    (reftex-parse-all)))

(defun dmd-html-to-org (&optional prefix)
  (interactive "P")
  (let ((buffer (if prefix
                    (read-buffer "Buffer: " (current-buffer) 'require-match)
                  (current-buffer))))
    (with-current-buffer buffer
      (dmd--format-to-org (buffer-substring (point-min) (point-max)) nil 2))
    (switch-to-buffer pandoc--output-buffer)
    (org-mode)
    (goto-char (point-min))))

(defun dmd--format-to-org (string &optional mode base-header-level)
  (setq base-header-level (or base-header-level 1))
  (setq mode (or mode 'html-mode))
  (ignore-errors (kill-buffer (get-buffer-create "*Pandoc output*")))
  (ignore-errors (kill-buffer (get-buffer-create "*Pandoc input*")))
  (with-current-buffer (get-buffer-create "*Pandoc input*")
    (setq pandoc--output-buffer (get-buffer-create "*Pandoc output*"))
    (goto-char (point-min))
    (insert string)
    (funcall mode)
    (turn-on-pandoc)
    (pandoc--set 'write "org")
    (pandoc--set 'columns 9001)
    (pandoc--set 'base-header-level base-header-level)
    (pandoc--set 'mathml t)
    (pandoc--set 'latexmathml t)
    (pandoc--call-external (current-buffer) nil)
    (with-current-buffer pandoc--output-buffer
      (buffer-substring (point-min) (point-max)))))

(defun dmd--org-feed-parse-html-entry (entry)
  (dmd--format-to-org (plist-get entry :item-full-text)))

(defun dmd--sanitize-org-regexp-matcher (regexp)
  (replace-regexp-in-string "[|/!]" "." regexp))

(defun dmd-bibtex-open (&optional prefix)
  (interactive "P")
  (if prefix
      (dmd--bibtex-open-file)
    (dmd--bibtex-jump-to-org-entry)))

(defun dmd--bibtex-open-file ()
  (let ((raw (cdr (assoc-string "file"
                                (save-excursion
                                  (bibtex-beginning-of-entry)
                                  (bibtex-parse-entry))
                                t))))
    (string-match "{:\\(.*\\):PDF}" raw)
    (org-open-file (match-string 1 raw))))

(defun dmd-scanimage (filename)
  (interactive "FOutputFile: ")
  (async-shell-command (format "scanimage -p -vvv --format=tiff > %s.tiff" filename)))

(advice-add 'org-contacts-remove-ignored-property-values :filter-return
            (lambda (list)
              (cl-remove-duplicates (nreverse list) :test #'string-match-p))
            '((name . remove-duplicates)))

(defun dmd--org-link-to-named-block ()
  "Create an org-link to the named block at point.

Blocks are named with #+NAME."
  (when (eq major-mode 'org-mode)
    (let* ((el (org-element-at-point))
           (name (org-element-property :name el)))
      (when name
        (org-store-link-props
         :link name)))))

(defun dmd--get-title (url)
  "Retrieve the title of the given URL."
  (let* ((buffer (url-retrieve-synchronously url))
         (title (with-current-buffer buffer
                  (goto-char (point-min))
                  (search-forward "<title>")
                  (buffer-substring-no-properties (point)
                                                  (progn
                                                    (search-forward "</title>")
                                                    (search-backward "<"))))))
    (kill-buffer buffer)
    (w3m-decode-entities-string
     (mm-decode-string
      title
      (symbol-name (w3m-url-coding-system url))))))

(defun dmd-doi-to-bib (url)
  "Retrieve bibtex from the given doi URL."
  (interactive "sURL: ")
  (request
   url
   :parser 'buffer-string
   :headers '(("Accept" . "text/bibliography; style=bibtex"))
   :success (function*
             (lambda (&key data &allow-other-keys)
               (with-current-buffer (get-buffer-create "*doi-to-bib*")
                 (delete-region (point-min) (point-max))
                 (insert data)
                 (switch-to-buffer-other-window (current-buffer)))))))

(defun dmd--ask-sign-encrypt ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (cond ((re-search-forward
            (concat "^" (regexp-quote mail-header-separator) "\n") nil t)
           (goto-char (setq insert-loc (match-end 0)))
           (unless (looking-at "<#secure")
             (let ((c (read-char "[s]ign, [e]ncrypt or [n]othing?")))
               (cond ((char-equal c ?s)
                      (mml-secure-message-sign))
                     ((char-equal c ?e)
                      (mml-secure-message-sign-encrypt))
                     ((char-equal c ?n)
                      t)
                     (t (user-error "Command not recognised"))))
             ))
          (t (error
              "The message is corrupted. No mail header separator")))))

(defun dmd--split-python-args-for-doctsring (args)
  "(dmd--split-python-args-for-doctsring \"big_table, keys, other_silly_variable=None\")
-> (\"big_table\" \"keys\" \"other_silly_variable\")"
  (let* ((args-with-default (split-string args "," t split-string-default-separators))
         (args-without-default (mapcar (lambda (s)
                                         (replace-regexp-in-string "=.*$" "" s))
                                       args-with-default)))
    args-without-default))

(defun dmd--format-python-args-for-docstring (args)
  (s-join "
"
          (mapcar (lambda (s)
                    (format "        %s: variable documentation." s))
                  (dmd--split-python-args-for-doctsring args))))

(defun lob-extract-variables (extract_var &optional buffer)
  (setq buffer (or buffer (current-buffer)))
  (goto-char (point-min))
  (let ((ret (loop while (re-search-forward
                          (rx bol (zero-or-more blank)
                              (group (one-or-more (not blank)))
                              (zero-or-more blank) (any "=" ":") (zero-or-more blank)
                              (group (one-or-more not-newline)) eol)
                          nil t)
                   collect (list (string-trim (match-string 1))
                                 (match-string 2)))))
    (if (or (null extract_var) (string= extract_var ""))
        ret
      (second (assoc extract_var ret)) )))

(defun dmd-add-to-load-path (&optional dir)
  (interactive "DAdd directory: ")
  (add-to-list 'load-path (expand-file-name dir)))

(defun dmd--decrypt-mail ()
  (save-excursion
    (goto-char (point-min))
    (let ((start (progn
                   (and (search-forward "-----BEGIN PGP MESSAGE-----" nil t)
                        (match-beginning 0))))
          (end (progn (and (search-forward "-----END PGP MESSAGE-----" nil t)
                           (match-end 0)))))
      (when (and start end)
        (epa-decrypt-region start end)))))

(defun dmd-replace-links-gnus-by-mu4e (&optional buffer)
  "Replace gnus style links by mu4e style links in BUFFER."
  (interactive "bBuffer: ")
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              (rx "[[gnus:"
                  (0+ (not (any "#"))) "#"
                  (group (1+ (not (any "]"))))
                  "]"
                  (group (and (? "[")
                              (0+ (not (any "]")))))
                  "]")
              nil t)
        (replace-match "[[mu4e:msgid:\\1]\\2]"
                       'fixedcase)))))

(provide 'dmd-utils)

;;; dmd-utils.el ends here
