;;; noflet.el --- locally override functions

;; Copyright (C) 2013  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Keywords: lisp
;; Version: 0.0.7
;; Url: https://github.com/nicferrier/emacs-noflet

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

;; This let's you locally override functions, in the manner of `flet',
;; but with access to the original function through the symbol:
;; `this-fn'.

;;; Code:

(require 'cl-macs)
(eval-when-compile (require 'cl))

(defun noflet|base ()
  "A base function."
  :noflet)

(defun noflet|expand (bindings &rest forms)
  "Expand BINDINGS and evaluate FORMS.

Used by `noflet' to expand it's contents.

Example:

 (noflet|expand
  '((find-file-noselect (file-name)
           (if t
               (this-fn \"/tmp/mytest\")
               (this-fn file-name)))
    (expand-file-name (file-name &optional thing)
     (if t
         (concat \"/tmp/\" file-name)
         (funcall this-fn file-name thing))))
  '(progn (expand-file-name \"/home/nferrier/thing\")))

It should not be necessary ever to call this.  Hence the exotic
name."
  (let*
      ((fsets
        (cl-loop
           for i in bindings
           collect
             (cl-destructuring-bind (name args &rest body) i
               (let ((saved-func-namev (make-symbol "saved-func-name")))
                 (let ((saved-func-namev
                        (intern (format "saved-func-%s"
                                        (symbol-name name)))))
                   `(fset (quote ,name)
                          (lambda ,args
                            (let ((this-fn ,saved-func-namev))
                              ,@body))))))))
       (fresets
        (cl-loop
             for i in bindings
             collect
             (cl-destructuring-bind (name args &rest body) i
               (let ((saved-func-namev (make-symbol "saved-func-name")))
                 (let ((saved-func-namev
                        (intern (format "saved-func-%s"
                                        (symbol-name name)))))
                   `(if
                        (eq (symbol-function (quote noflet|base))
                            ,saved-func-namev)
                        (fmakunbound (quote ,name))
                        (fset (quote ,name) ,saved-func-namev)))))))
       (lets
        (cl-loop
           for i in bindings
           collect
             (cl-destructuring-bind (name args &rest body) i
               (let ((saved-func-namev (make-symbol "saved-func-name")))
                 (let ((saved-func-namev
                        (intern (format "saved-func-%s"
                                        (symbol-name name)))))
                   `(,saved-func-namev
                     (condition-case err
                         (symbol-function (quote ,name))
                       (void-function
                        (symbol-function (quote noflet|base)))))))))))
    `(let ,lets
       (unwind-protect
            (progn
              (progn ,@fsets)
              ,@forms)
         (progn ,@fresets)))))
 
(defmacro noflet (bindings &rest body)
  "Make local function BINDINGS allowing access to the original.

Each of the BINDINGS is done like in `flet':

  (noflet
    ((expand-file-name (file-name &optional default-dir)
       (concat defaulr-dir file-name)))
    (expand-file-name \"~/test\"))

In each of the BINDINGS the original function is accessible with
the name `this-fn':

  (noflet
    ((expand-file-name (file-name &optional default-dir)
       (if (string-match-p \"/fake.*\" file-name)
          (concat default-dir file-name)
          (funcall this-fn file-name default-dir))))
    (expand-file-name \"~/test\"))

This is great for overriding in testing and such like.

If new bindings are introduced the binding is discarded upon
exit.  Even with new bindings there is still a `this-fn'.  It
points to `noflet|base' for all new bindings."
  (declare (debug ((&rest (cl-defun)) cl-declarations body))
           (indent ((&whole 4 &rest (&whole 1 &lambda &body)) &body)))
  (apply 'noflet|expand bindings body))


(defmacro* let-while ((var expression) &rest body)
  "A simple binding loop.

VAR is bound to EXPRESSION repeatedly until `nil'.

BODY is evaluated each time."
  (declare
   (debug (sexp sexp &rest form))
   (indent 1))
  (let ((expression-proc (make-symbol "exprp")))
    `(let ((,expression-proc (lambda () ,expression)))
       (let ((,var (funcall ,expression-proc)))
         (while ,var
           (progn ,@body)
           (setq ,var (funcall ,expression-proc)))))))


(provide 'noflet)
;;; noflet.el ends here