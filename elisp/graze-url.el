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

(require 'w3m-load)
(require 'cl)

(defvar gu-find-url-functions nil
  "A list of functions used to find the url at point.")

(setf gu-find-url-functions
      '((lambda ()
          (when (every 'fboundp
                       '(org-at-regexp-p
                         org-link-unescape
                         org-match-string-no-properties))
            (if (org-at-regexp-p org-bracket-link-regexp)
                (org-link-unescape
                 (org-match-string-no-properties 1)))))
        (lambda ()
          (when (every 'fboundp
                       '(w3m-url-valid
                         w3m-anchor))
            (w3m-url-valid (w3m-anchor))))
        (lambda ()
          (get-text-property (point) 'shr-url))
        (lambda ()
          (when (fboundp 'thing-at-point)
            (thing-at-point 'url)))))

(defun gu-find-url-at-point ()
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

(defun gu-w3m-browse-url-new-session (url)
  "Ask emacs-w3m to browse URL."
  (interactive)
  (w3m-browse-url (w3m-canonicalize-url url) t))

(defun gu-conkeror-browse-url (url)
  "Ask conkeror to browse URL."
  (interactive)
  (start-process (concat "conkeror" url)
                 nil
                 "conkeror"
                 url))

(defvar *gu-first-browse-url-function* 'gu-w3m-browse-url-new-session)
(defvar *gu-second-browse-url-function* 'gu-conkeror-browse-url)

(defun gu-browse-url-interactive-arg (prompt)
  (list (read-string prompt
                     (or (and transient-mark-mode mark-active
                              ;; rfc2396 Appendix E.
                              (replace-regexp-in-string
                               "[\t\r\f\n ]+" ""
                               (buffer-substring-no-properties
                                (region-beginning) (region-end))))
                         (gu-find-url-at-point)))
        current-prefix-arg))

(defun gu-browse-url (url &optional second-function?)
  (interactive (gu-browse-url-interactive-arg "URL: "))
  (funcall (if second-function?
               *gu-second-browse-url-function*
             *gu-first-browse-url-function*)
           url))

(provide 'graze-url)

;;; graze-url.el ends here
