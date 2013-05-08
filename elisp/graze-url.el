;;; graze-url.el ---

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

(require 'cl)

(defgroup graze-url nil
  "Browse URL"
  :group 'external)

(defcustom gu-find-url-functions
  '(gu-find-url-org-mode
    gu-find-url-w3m
    gu-find-url-text-property
    gu-find-url-thing-at-point)
  "A list of functions used to find the url at point."
  :type '(repeat function))

(defcustom gu-browse-url-functions
  '(browse-url-browser-function browse-url-generic)
  "The functions used to browse an URL, the first one is the default."
  :type '(repeat (choice variable function)))

(defcustom gu-search-format
  "http://www.google.com/search?q=%s"
  "The string used to search on the web. `%s' will be replaced by
the terms searched."
  :type 'string)

(defun gu-find-url-org-mode ()
  "Find URL in `org-mode' style."
  (when (every 'fboundp
               '(org-at-regexp-p
                 org-link-unescape
                 org-match-string-no-properties))
    (if (org-at-regexp-p org-bracket-link-regexp)
        (org-link-unescape
         (org-match-string-no-properties 1)))))

(defun gu-find-url-w3m ()
  "Find URL using `emacs-w3m' helper functions."
  (when (and 
         (fboundp 'w3m-url-valid)
         (fboundp 'w3m-anchor))
    (w3m-url-valid (w3m-anchor))))

(defun gu-find-url-text-property ()
  "Find URL stored in text property."
  (get-text-property (point) 'shr-url))

(defun gu-find-url-thing-at-point ()
  "Find URL using `thing-at-point'."
  (when (fboundp 'thing-at-point)
    (thing-at-point 'url)))

(defun gu-find-url-at-point ()
  "Find URL using functions in `gu-find-url-functions'."
  (let (url)
    (loop for fun in gu-find-url-functions
          until (setf url (ignore-errors
                            (funcall fun))))
    url))

(defun gu-copy-url-at-point ()
  "Copy the url at point."
  (interactive)
  (let ((url (gu-find-url-at-point)))
    (if (not url)
        (error "No url found at point")
      (kill-new url)
      (message "%s" url))))

(defun gu-browse-url-interactive-arg (prompt)
  "Ask an URL with the given PROMPT."
  (list (read-string prompt
                     (or (and transient-mark-mode mark-active
                              ;; rfc2396 Appendix E.
                              (replace-regexp-in-string
                               "[\t\r\f\n ]+" ""
                               (buffer-substring-no-properties
                                (region-beginning) (region-end))))
                         (gu-find-url-at-point)))
        current-prefix-arg))

(defun gu-get-browse-url-function (alternative)
  "Return the function selected."
  (let* ((fun (elt gu-browse-url-functions (if (integerp alternative)
                                               (truncate (log alternative 4))
                                             0)))
         (fun (if
                  ;; `gu-****-browse-url-function' can be either a
                  ;; variable or a function, but we need a function.
                  (functionp fun)
                  fun
                (symbol-value fun))))
    (unless (functionp fun)
      (error "%s is not a valid function." fun))
    fun))

(defun gu-browse-url (url &optional alternative)
  (interactive (gu-browse-url-interactive-arg "URL: "))
  (funcall (gu-get-browse-url-function (car-safe alternative))
           url))

(defun gu-search (term &optional alternative)
  (interactive (list (read-string "Search: ")
                     current-prefix-arg))
  (funcall (gu-get-browse-url-function (car-safe alternative))
           (format gu-search-format term)))

(provide 'graze-url)

;;; graze-url.el ends here
