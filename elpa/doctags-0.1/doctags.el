;;; doctags.el --- Generation of tags documentation in Doxygen syntax

;; Copyright (C) 2012  Matthias Meulien

;; Author: Matthias Meulien <orontee@gmail.com>
;; Keywords: convenience, languages
;; URL: https://gitorious.org/doctags
;; Version: 0.1

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

;; This package provides helpers to write technical documentation of
;; source code: Documentation skeletons are generated from Semantic
;; tags.
;;
;; At the present time, the generated skeleton use Doxygen syntax for
;; C-like languages. But it is easy to change. Documenting a tag is
;; done by `doctags-document-current-tag'.

;;; Code:

(require 'custom)

(defgroup doctags nil
  "Generator of tags documentation."
  :group 'tools
  :version "24.0")

(defcustom doctags-c-generator-command-style 'backslash
  "Style of commands."
  :type '(radio (const :tag "backslash" backslash)
		(const :tag "at-sign" at-sign))
  :group 'doctags)

(defcustom doctags-c-generator-use-autobrief nil
  "Whether to use the autobrief style or not."
  :type '(boolean)
  :group 'doctags)
;; WARNING Not implemented!

(defcustom doctags-c-generator-block-style 'qt
  "The style of comment blocks."
  :type '(radio (const :tag "Qt" qt)
		(const :tag "JavaDoc" javadoc))
  :group 'doctags)

(defun doctags-c-generator-type (tag)
  "Return a skeleton describing a C-like comment string
documenting TAG.
This function assumes that TAG is a 'type tag."
  `(nil > ,(doctags-c-generator-block-start) \n
	> ,(doctags-c-generator-command "brief") " " _ \n
	> \n
	> ,(doctags-c-generator-block-end)))

(defun doctags-c-generator-function (tag)
  "Return a skeleton describing a C-like comment string
documenting TAG.
This function assumes that TAG is a 'function tag."
  `(nil > ,(doctags-c-generator-block-start) \n
	> ,(doctags-c-generator-command "brief") " " _ \n
	> \n
	(nil 
	 (,(mapcar (lambda (arg) (car arg)) (semantic-tag-function-arguments tag)) 
	  >  ,(doctags-c-generator-command "param") " " str " " \n))
	,(cond 
	  ((and (not (equal (semantic-tag-get-attribute tag :type) "void"))
		(not (semantic-tag-function-constructor-p tag))
		(not (semantic-tag-function-destructor-p tag)))
	   (list 'nil '> (doctags-c-generator-command "return") " " '\n))
	  (t ""))
	> ,(doctags-c-generator-block-end)))

(defun doctags-c-generator-variable (tag)
  "Return a skeleton describing a C-like comment string
documenting TAG.
This function assumes that TAG is a 'type tag."
  `(nil " //!< "))

(defun doctags-c-generator-command (name)
  "Return NAME prefixed by an escaped backslash or an at-sign
according to `doctags-c-generator-command-style'."
  (cond
   ((eq doctags-c-generator-command-style 'backslash) 
    (concat "\\" name))
   ((eq doctags-c-generator-command-style 'at-sign)
    (concat "@" name))))

(defun doctags-c-generator-block-start ()
  "Return the string to insert to start a new comment block."
  (cond
   ((eq doctags-c-generator-block-style 'qt)
    "/*!")
   ((eq doctags-c-generator-block-style 'javadoc)
    "/**")))

(defun doctags-c-generator-block-end ()
  "Return the string to insert to end a comment block."
  "*/")

(defvar doctags-c-generator
  '((type . (doctags-c-generator-type nil))
    (function . (doctags-c-generator-function nil))
    (variable . (doctags-c-generator-variable t)))
  "The default documentation generator, handling various Doxygen
styles for C-like languages.

This generator works with three tag classes: 'type, 'function and
'variable.")

(defvar doctags-generator 'doctags-c-generator
  "Generators are alist of (CLASS . DATA) where CLASS is a tag
class symbol and DATA is a list of the form (SKEL AFTER) with
SKEL the symbol of a function returning the skeleton to generate
the documentation string for a tag of class CLASS and AFTER is
non-nil whenever the generated documentation must be inserted
after the tag. See `doctags-document-current-tag'.")

(defun doctags-document-current-tag ()
  "Generate documentation for the current tag."
  (interactive)
  (let ((tag (semantic-current-tag)))
    (if tag
	(let* ((class (semantic-tag-class tag))
	       (data (assoc class (symbol-value doctags-generator)))
	       (func (and data (nth 0 (cdr data))))
	       (after (and data (nth 1 (cdr data))))
	       (skeleton-end-newline (not after)))
	  (if (not func)
	      (error "Documentation generator not working with %S tags" class)
	    (if after
		(goto-char (semantic-tag-end tag))
	      (goto-char (semantic-tag-start tag)))
	    (skeleton-insert (funcall func tag))))
      (error "No tag at point"))))
  
(provide 'doctags)
;;; doctags.el ends here
