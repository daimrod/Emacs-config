;;; s-buffer.el --- s operations for buffers

;; Copyright (C) 2013  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Keywords: lisp
;; Created: 15th May 2013
;; Url: http://github.com/nicferrier/emacs-s-buffer
;; Package-requires: ((s "1.6.0")(noflet "0.0.1"))
;; Version: 0.0.2

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

;; The awesome s library for EmacsLisp has great extensions for doing
;; string operations. Some of those operations would be useful on
;; buffers. So this is those operations fitted for buffers.

;;; Code:

(require 's)
(require 'noflet)

(defun s-buffer-format (buffer replacer &optional extra)
  "Format the text in BUFFER in place with REPLACER.

The arguments for format-buffer are as for `s-format'."
  (noflet ((replace-regexp-in-string
            (re replcr str &optional fc lit)
            (with-current-buffer
                (get-buffer (get-text-property 0 :buffer str))
              (save-excursion
                (goto-char (get-text-property 0 :point str))
                (while (re-search-forward re nil t)
                  (noflet ((replace-regexp-in-string
                            (re fn str &optional fc lit)
                            (funcall this-fn re fn str fc lit)))
                          (replace-match (funcall replcr nil) nil t)))))))
          (s-format
           (propertize
            "dummystr"
            :buffer buffer
            :point (with-current-buffer buffer (point)))
           replacer extra)
          buffer))

(defun s-buffer|format-internal (original-s-format str replacer extra)
  (noflet ((s-format
            (str replacer &optional extra)
            (funcall original-s-format str replacer extra)))
      (s-buffer-format str replacer extra)))

(defmacro s-buffer-lex-format (buffer)
  "Use scope to resolve the variables in BUFFER.

The buffer form of `s-lex-format'."
  (let ((bufsym (make-symbol "bv")))
    `(let ((,bufsym ,buffer))
       (noflet ((s-format
                 (str replacer &optional extra)
                 (s-buffer|format-internal
                  this-fn str replacer extra)))
               (s-lex-format ,bufsym)
               ,bufsym))))

(provide 's-buffer)

;;; s-buffer.el ends here
