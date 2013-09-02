;;; rec-mode.el --- Major mode for viewing/editing rec files

;; Copyright (C) 2009, 2010, 2011 Jose E. Marchesi

;; Maintainer: Jose E. Marchesi

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; A major mode for editing rec files.
;;
;; To see the structure of this file activate the outline minor mode
;; and execute M-xhide-body

;;; Code:

(require 'compile)

;;;; Customization

(defgroup rec-mode nil
  "rec-mode subsystem"
  :group 'applications
  :link '(url-link "http://www.gnu.org/software/rec"))

(defcustom rec-open-mode 'navigation
  "Default mode to use when switching a buffer to rec-mode.
Valid values are `edit' and `navigation'.  The default is `navigation'"
  :type 'symbol
  :group 'rec-mode)

(defvar rec-recsel "recsel"
  "Name of the 'recsel' utility from the GNU recutils.")

(defvar rec-recinf "recinf"
  "Name of the 'recinf' utility from the GNU recutils.")

(defvar rec-recfix "recfix"
  "Name of the 'recfix' utility from the GNU recutils.")

;;;; Variables and constants that the user does not want to touch (really!)

(defconst rec-keyword-rec "%rec"
  ;; Remember to update `rec-font-lock-keywords' if you change this
  ;; value!!
  "Rec keyword.")

(defvar rec-comment-re "^#.*"
  "regexp denoting a comment line")

(defvar rec-comment-field-re "^\\(#.*\n\\)*\\([a-zA-Z0-1_%-]+:\\)+"
  "regexp denoting the beginning of a record")

(defvar rec-field-name-re
  "^\\([a-zA-Z0-1_%-]+:\\)+"
  "Regexp matching a field name")

(defvar rec-field-value-re
  (let ((ret-re "\n\\+ ?")
        (esc-ret-re "\\\\\n"))
    (concat
     "\\("
     "\\(" ret-re "\\)*"
     "\\(" esc-ret-re "\\)*"
     "\\(" "\\\\[^\n]" "\\)*"
     "[^\\\n]*"
     "\\)*"))
  "Regexp matching a field value")

(defvar rec-field-re
  (concat rec-field-name-re
          rec-field-value-re
          "\n")
  "Regexp matching a field")

(defvar rec-record-re
  (concat rec-field-re "\\(" rec-field-re "\\|" rec-comment-re "\\)*")
  "Regexp matching a record")

(defvar rec-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?# "<" st)   ; Comment start
    (modify-syntax-entry ?\n ">" st)  ; Comment end
    st)
  "Syntax table used in rec-mode")

(defvar rec-font-lock-keywords
  `(("^%\\(rec\\|key\\|unique\\|type\\|typedef\\|prohibit\\|mandatory\\|type\\|doc\\|fsort\\|auto\\|confidential\\|sort\\):" . font-lock-keyword-face)
    (,rec-field-name-re . font-lock-variable-name-face)
    ("^\\+" . font-lock-constant-face))
  "Font lock keywords used in rec-mode")

(defvar rec-mode-edit-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-cn" 'rec-cmd-goto-next-rec)
    (define-key map "\C-cp" 'rec-cmd-goto-previous-rec)
    (define-key map "\C-ce" 'rec-cmd-edit-field)
    (define-key map "\C-ct" 'rec-cmd-show-descriptor)
    (define-key map "\C-c#" 'rec-cmd-count)
    (define-key map "\C-cl" 'rec-cmd-sel)
    (define-key map "\C-cs" 'rec-cmd-search)
    (define-key map "\C-cm" 'rec-cmd-trim-field-value)
    (define-key map "\C-cc" 'rec-cmd-compile)
    (define-key map "\C-cI" 'rec-cmd-show-info)
    (define-key map [remap move-beginning-of-line] 'rec-cmd-beginning-of-line)
    (define-key map (kbd "TAB") 'rec-cmd-goto-next-field)
    (define-key map (concat "\C-c" (kbd "RET")) 'rec-cmd-jump)
    (define-key map "\C-cb" 'rec-cmd-jump-back)
    (define-key map "\C-c\C-c" 'rec-finish-editing)
    map)
  "Keymap for rec-mode")

(defvar rec-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" 'rec-cmd-goto-next-rec)
    (define-key map "p" 'rec-cmd-goto-previous-rec)
    (define-key map "e" 'rec-cmd-edit-field)
    (define-key map "R" 'rec-edit-record)
    (define-key map "T" 'rec-edit-type)
    (define-key map "B" 'rec-edit-buffer)
    (define-key map "A" 'rec-cmd-append-field)
    (define-key map "I" 'rec-cmd-show-info)
    (define-key map "t" 'rec-cmd-show-descriptor)
    (define-key map "l" 'rec-cmd-sel)
    (define-key map "s" 'rec-cmd-search)
    (define-key map "m" 'rec-cmd-trim-field-value)
    (define-key map "c" 'rec-cmd-compile)
    (define-key map "\C-ct" 'rec-find-type)
    (define-key map [remap move-beginning-of-line] 'rec-cmd-beginning-of-line)
    (define-key map "#" 'rec-cmd-count)
    (define-key map (kbd "RET") 'rec-cmd-jump)
    (define-key map (kbd "TAB") 'rec-cmd-goto-next-field)
    (define-key map (kbd "SPC") 'rec-cmd-toggle-field-visibility)
    (define-key map "b" 'rec-cmd-jump-back)
    map)
  "Keymap for rec-mode")

;;;; Parsing functions (rec-parse-*)
;;
;; Those functions read the contents of the buffer (starting at the
;; current position of the pointer) and try to parse field, comment
;; and records structures.

(defun rec-parse-comment ()
  "Parse and return a comment starting at point.

Return a list whose first element is the symbol 'comment and the
second element is the string with the contents of the comment,
including the leading #:

   (comment POSITION \"# foo\")

If the point is not at the beginning of a comment then return nil"
  (let ((there (point))
        comment)
    (when (and (equal (current-column) 0)
               (looking-at rec-comment-re))
      (setq comment (list 'comment
                          there
                          (buffer-substring-no-properties (match-beginning 0)
                                                          (match-end 0))))
      (goto-char (match-end 0))
      ;; Skip a newline if needed
      (when (looking-at "\n") (goto-char (match-end 0))))
    comment))

(defun rec-parse-field-name ()
  "Parse and return a field name starting at point.

Return a list with whose elements are the parts of the field
name.  For the name a:b:c:d: the following list is returned:

   (\"a\" \"b\" \"c\" \"d\")

If the point is not at the beginning of a field name return nil"
  (when (and (equal (current-column) 0)
             (looking-at rec-field-name-re))
    (goto-char (match-end 0))
    (split-string
     (buffer-substring-no-properties (match-beginning 0)
                                     (- (match-end 0) 1))
     ":")))

(defun rec-parse-field-name-from-string (str)
  "Parse and return a field name parsed from a string.

If the string does not contain a field name, then return nil."
  (with-temp-buffer
    (insert str)
    ;; Add a colon to the end if does not exist
    (save-excursion
      (goto-char (point-max))
      (unless (equal (char-before) ?:)
        (insert ?:)))
    (goto-char (point-min))
    (rec-parse-field-name)))

(defun rec-parse-field-value ()
  "Return the field value under the pointer.

Return a string containing the value of the field.

If the pointer is not at the beginning of a field value, return
nil"
  (when (looking-at rec-field-value-re)
    (goto-char (match-end 0))
    (let ((val (buffer-substring-no-properties (match-beginning 0)
                                               (match-end 0))))
      ;; Replace escaped newlines
      (setq val (replace-regexp-in-string "\\\\\n" "" val))
      ;; Replace continuation lines
      (setq val (replace-regexp-in-string "\n\\+ ?" "\n" val))
      ;; Remove the initial blank
      (with-temp-buffer
        (insert val)
        (goto-char (point-min))
        (if (equal (char-after (point)) ? )
            (progn
              (delete-char 1)))
        (setq val (buffer-substring-no-properties (point-min)
                                                  (point-max))))
      val)))

(defun rec-parse-field ()
  "Return a structure describing the field starting from the
pointer.

The returned structure is a list whose first element is the
symbol 'field', the second element is the name of the field and
the second element is the value of the field:

   (field POSITION FIELD-NAME FIELD-VALUE)

If the pointer is not at the beginning of a field
descriptor then return nil"
  (let ((there (point))
        field-name field-value)
    (and (setq field-name (rec-parse-field-name))
         (setq field-value (rec-parse-field-value)))
    (when (and field-name field-value)
        ;; Skip a newline if needed
        (when (looking-at "\n") (goto-char (match-end 0)))
        (list 'field there field-name field-value))))

(defun rec-parse-record ()
  "Return a structure describing the record starting from the pointer.

The returned structure is a list of fields preceded by the symbol
'record':

   (record FIELD-1 FIELD-2 ... FIELD-N)

If the pointer is not at the beginning of a record, then return
nil"
  (let ((there (point))
        record field-or-comment)
    (while (setq field-or-comment (or (rec-parse-field)
                                      (rec-parse-comment)))
      (setq record (cons field-or-comment record)))
    (setq record (list 'record there (reverse record)))))

;;;; Writer functions (rec-insert-*)
;;
;; Those functions dump the written representation of the parser
;; structures (field, comment, record, etc) into the current buffer
;; starting at the current position.

(defun rec-insert-comment (comment)
  "Insert the written form of COMMENT in the current buffer"
  (when (rec-comment-p comment)
    (insert (rec-comment-string comment))))

(defun rec-insert-field-name (field-name)
  "Insert the written form of FIELD-NAME in the current buffer"
  (when (listp field-name)
    (mapcar (lambda (elem)
              (when (stringp elem) (insert elem ":")))
            field-name)))

(defun rec-insert-field-value (field-value)
  "Insert the written form of FIELD-VALUE in the current buffer"
  (when (stringp field-value)
    (let ((val field-value))
      ;; FIXME: Maximum line size
      (insert (replace-regexp-in-string "\n" "\n+ " val)))
    (insert "\n")))

(defun rec-insert-field (field)
  "Insert the written form of FIELD in the current buffer"
  (when (rec-field-p field)
    (when (rec-insert-field-name (rec-field-name field))
      (insert " ")
      (rec-insert-field-value (rec-field-value field)))))

(defun rec-insert-record (record)
  "Insert the written form of RECORD in the current buffer."
  (when (rec-record-p record)
    (mapcar (lambda (elem)
              (cond
               ((rec-comment-p elem) (rec-insert-comment elem))
               ((rec-field-p elem) (rec-insert-field elem))))
            (rec-record-fields record))))

;;;; Operations on record structures
;;
;; Those functions retrieve or set properties of field structures.

(defun rec-record-p (record)
  "Determine if the provided structure is a record."
  (and (listp record)
       (= (length record) 3)
       (equal (car record) 'record)))

(defun rec-record-position (record)
  "Return the start position of the given record."
  (when (rec-record-p record)
    (nth 1 record)))

(defun rec-record-fields (record)
  "Return a list with the fields of the given record."
  (when (rec-record-p record)
    (nth 2 record)))

(defun rec-record-descriptor-p (record)
  "Determine if the given record is a descriptor."
  (not (null (rec-record-assoc '("%rec") record))))

(defun rec-record-assoc (name record)
  "Get a list with the values of the fields in RECORD named NAME.

NAME shall be a field name i.e. a list of field name parts.

If no such field exists in RECORD then nil is returned."
  (if (stringp name)
      (setq name (rec-parse-field-name-from-string name)))
  (when (rec-record-p record)
    (let (result)
      (mapcar (lambda (field)
                (when (and (rec-field-p field)
                           (equal name (rec-field-name field)))
                  (setq result (cons (rec-field-value field) result))))
              (rec-record-fields record))
      (reverse result))))

(defun rec-record-names (record)
  "Get a list of the field names in the record"
  (when (rec-record-p record)
    (let (result)
      (mapcar (lambda (field)
                (when (rec-field-p field)
                  (setq result (cons (rec-field-name field) result))))
              (rec-record-fields record))
      (reverse result))))

;;;; Operations on comment structures
;;
;; Those functions retrieve or set properties of comment structures.

(defun rec-comment-p (comment)
  "Determine if the provided structure is a comment"
  (and (listp comment)
       (= (length comment) 3)
       (equal (car comment) 'comment)))

(defun rec-comment-position (comment)
  "Return the start position of the given comment."
  (when (rec-comment-p comment)
    (nth 1 comment)))

(defun rec-comment-string (comment)
  "Return the string composig the comment, including the initial '#' character."
  (when (rec-comment-p comment)
    (nth 2 comment)))

;;;; Operations on field structures
;;
;; Those functions retrieve or set properties of field structures.

(defun rec-field-p (field)
  "Determine if the provided structure is a field"
  (and (listp field)
       (= (length field) 4)
       (equal (car field) 'field)))

(defun rec-field-position (field)
  "Return the start position of the given field."
  (when (rec-field-p field)
    (nth 1 field)))

(defun rec-field-name (field)
  "Return the name of the provided field"
  (when (rec-field-p field)
    (nth 2 field)))

(defun rec-field-value (field)
  "Return the value of the provided field"
  (when (rec-field-p field)
    (nth 3 field)))

(defun rec-field-set-value (field value)
  "Return FIELD with its value replaced by VALUE."
  (list 'field
        (rec-field-position field)
        (rec-field-name field)
        value))

(defun rec-field-trim-value (field)
  "Return FIELD with its value trimmed."
  (when (rec-field-p field)
    (let ((value (rec-field-value field))
          c)
      (with-temp-buffer
        (insert value)
        (goto-char (point-min))
        (when (looking-at "[ \t\n]+")
          (delete-region (match-beginning 0)
                         (match-end 0)))
        (goto-char (point-max))
        (setq c (char-before))
        (while (and c
                    (or (equal c ?\n)
                        (equal c ?\t)
                        (equal c ? )))
          (backward-char)
          (setq c (char-before)))
        (delete-region (point) (point-max))
        (setq value (buffer-substring-no-properties (point-min)
                                                    (point-max))))
      (rec-field-set-value field value))))

;;;; Get entities under pointer
;;
;; Those functions retrieve structures of the entities under pointer
;; like comments, fields and records.  If the especified entity is not
;; under the pointer then nil is returned.

(defun rec-beginning-of-field-pos ()
  "Return the position of the beginning of the current field, or
nil if the pointer is not on a field."
  (save-excursion
    (beginning-of-line)
    (let (res exit)
      (while (not exit)
        (cond
         ((and (not (= (line-beginning-position) 1))
               (or (looking-at "+")
                   (looking-back "\\\\\n" 2)))
          (forward-line -1))
         ((looking-at rec-field-name-re)
          (setq res (point))
          (setq exit t))
         (t
          (setq exit t))))
      res)))

(defun rec-end-of-field-pos ()
  "Return the position of the end of the current field, or nil if
the pointer is not on a field."
  (let ((begin-pos (rec-beginning-of-field-pos)))
    (when begin-pos
      (save-excursion
        (goto-char begin-pos)
        ;; The following hack is due to the fact that
        ;; the regular expressions search engine is
        ;; hanging on rec-field-re
        (when (looking-at rec-field-name-re)
          (goto-char (match-end 0)))
        (when (looking-at rec-field-value-re)
          ;; The +1 is to include the \n at the beginning of the
          ;; record value, that is part of the field but not part of
          ;; the value
          (+ (match-end 0) 1))))))

(defun rec-beginning-of-comment-pos ()
  "Return the position of the beginning of the current comment,
or nil if the pointer is not on a comment."
  (save-excursion
    (beginning-of-line)
    (when (looking-at rec-comment-re)
      (point))))

(defun rec-end-of-comment-pos ()
  "Return the position of the end of the current comment,
or nil if the pointer is not on a comment."
  (let ((begin-pos (rec-beginning-of-comment-pos)))
    (when begin-pos
      (save-excursion
        (goto-char begin-pos)
        (when (looking-at rec-comment-re)
          (match-end 0))))))

(defun rec-beginning-of-record-pos ()
  "Return the position of the beginning of the current record, or nil if
the pointer is not on a record."
  (save-excursion
    (let (field-pos prev-pos)
      (setq prev-pos (point))
      (while (and (not (equal (point) (point-min)))
                  (or (setq field-pos (rec-beginning-of-field-pos))
                      (setq field-pos (rec-beginning-of-comment-pos))))
        (goto-char field-pos)
        (if (not (equal (point) (point-min)))
            (backward-char)))
      (unless (or (eobp)
                  (looking-at rec-comment-field-re))
          (forward-char))
      (when (looking-at rec-comment-field-re)
        (point)))))

(defun rec-end-of-record-pos ()
  "Return the position of the end of the current record,
or nil if the pointer is not on a record."
  (let ((begin-pos (rec-beginning-of-record-pos)))
    (when begin-pos
      (save-excursion
        (goto-char begin-pos)
        (while (or (looking-at rec-field-name-re)
                   (looking-at rec-comment-re))
          (goto-char (match-end 0))
          (when (or (looking-at rec-field-value-re)
                    (looking-at rec-comment-re))
            (goto-char (+ (match-end 0) 1))))
        (point)))))

(defun rec-current-field ()
  "Return a structure with the contents of the current field.
The current field is the field where the pointer is."
  (save-excursion
    (let ((begin-pos (rec-beginning-of-field-pos)))
      (when begin-pos
        (goto-char begin-pos)
        (rec-parse-field)))))

(defun rec-current-record ()
  "Return a structure with the contents of the current record.
The current record is the record where the pointer is"
  (save-excursion
    (let ((begin-pos (rec-beginning-of-record-pos)))
      (when begin-pos
        (goto-char begin-pos)
        (rec-parse-record)))))

;;;; Visibility
;;
;; These functions manage the visibility in the rec buffer.

(defun rec-narrow-to-record ()
  "Narrow to the current record, if any"
  (let ((begin-pos (rec-beginning-of-record-pos))
        (end-pos (rec-end-of-record-pos)))
    (if (and begin-pos end-pos)
        (narrow-to-region begin-pos end-pos))))

(defun rec-narrow-to-type (type)
  "Narrow to the specified type, if any"
  (let ((begin-pos (or (rec-type-pos type) (point-min)))
        (end-pos (or (rec-type-pos (rec-type-next type)) (point-max))))
    (narrow-to-region begin-pos end-pos)))

;;;; Record collection management
;;
;; These functions perform the management of the collection of records
;; in the buffer.

(defun rec-update-buffer-descriptors ()
  "Get a list of the record descriptors in the current buffer."
  (message "Updating record descriptors...")
  (setq rec-buffer-descriptors
        (save-excursion
          (let ((rec-file-name (if buffer-file-name
                                   buffer-file-name
                                 ""))
                descriptors records)
            ;; Call 'recinf' to get the list of record descriptors in
            ;; sexp format.
            (with-temp-buffer
              (call-process rec-recinf
                            nil ; infile
                            t   ; output to current buffer
                            nil ; display
                            "-S" "-d" rec-file-name)
              (goto-char (point-min))
              (insert "(")
              (goto-char (point-max))
              (insert ")")
              (setq descriptors (read (buffer-substring-no-properties (point-min) (point-max)))))
            ;; Calculate the value of 'rec-buffer-descriptors'.
            (mapcar (lambda (descriptor)
                      (let ((marker (make-marker)))
                        (set-marker marker (rec-record-position descriptor))
                        (setq records (cons (list 'descriptor descriptor marker) records))))
                    descriptors)
            (reverse records))))
  (message ""))

(defun rec-update-buffer-descriptors-xxx ()
  "Get a list of the record descriptors in the current buffer."
  (message "Updating record descriptors...")
  (setq rec-buffer-descriptors
        (save-excursion
          (let (records rec marker)
            (goto-char (point-min))
            (while (and (not (= (point) (point-max)))
                        (re-search-forward
                         (concat "^" (regexp-quote rec-keyword-rec) ":") nil t))
              (rec-beginning-of-record)
              (setq marker (point-marker))
              (setq rec (rec-parse-record))
              (when (rec-record-assoc (list rec-keyword-rec) rec)
                (setq records (cons (list 'descriptor rec marker) records)))
              (if (not (= (point) (point-max)))
                  (forward-char)))
            (reverse records))))
  (message ""))

(defun rec-buffer-types ()
  "Return a list with the names of the record types in the
existing buffer."
  ;; If a descriptor has more than a %rec field, then the first one is
  ;; used.  The rest are ignored.
  (mapcar
   (lambda (elem) (car elem))
   (mapcar
    (lambda (elem)
      (rec-record-assoc (list rec-keyword-rec) (cadr elem)))
    rec-buffer-descriptors)))

(defun rec-type-p (type)
  "Determine if there are records of type TYPE in the current
file."
  (member type (rec-buffer-types)))

(defun rec-goto-type (type)
  "Goto the beginning of the descriptor with type TYPE.

If there are records of type TYPE in the record set then goto the
first record.  Otherwise goto to the record descriptor.

If the type do not exist in the current buffer then
this function returns nil."
  (if (or (not type)
          (equal type ""))
      ;; If there is a regular record in the
      ;; beginning of the file, go there.
      (if (save-excursion
            (goto-char (point-min))
            (unless (looking-at rec-comment-field-re)
              (rec-goto-next-rec))
            (rec-regular-p))
          (progn
            (goto-char (point-min))
            (unless (looking-at rec-comment-field-re)
              (rec-goto-next-rec))
            t)
        nil)
    (let (found
          (descriptors rec-buffer-descriptors))
      (mapcar
       (lambda (elem)
         (when (equal (car (rec-record-assoc (list rec-keyword-rec)
                                             (cadr elem)))
                      type)
           (setq found t)
           (goto-char (nth 2 elem))))
       descriptors)
      found)))

(defun rec-type-pos (type)
  "Return the position where the records of type TYPE start in
the current file.  If no records of type TYPE are defined in the
current file then return nil."
  (when (rec-type-p type)
    (save-excursion
      (rec-goto-type type)
      (point))))

(defun rec-type-next (type)
  "Return the name of the type following TYPE in the file, if
any.  If the specified type is the last appearing in the file,
or the specified type does not exist, then return nil."
  (let ((types (member type (rec-buffer-types))))
    (nth 1 types)))

(defun rec-type-previous (type)
  "Return the name of the type preceding TYPE in the file, if
any.  If the specified type is the first appearing in the file,
or the specified type does not exist, then return nil."
  (let ((types (member type (reverse (rec-buffer-types)))))
    (nth 1 types)))

(defun rec-goto-next-field ()
  "Move the pointer to the beginning of the next field in the
file."
  (let ((pos (save-excursion
               (rec-end-of-field)
               (when (re-search-forward rec-field-name-re nil t)
                 (match-beginning 0)))))
    (when pos
      (goto-char pos)
      t)))

(defun rec-goto-next-rec ()
  "Move the pointer to the beginning of the next record in the
file."
  (let ((pos (save-excursion
               (rec-end-of-record)
               (when (re-search-forward rec-comment-field-re nil t)
                 (match-beginning 0)))))
    (when pos
        (goto-char pos)
        t)))

(defun rec-goto-previous-rec ()
  "Move the pointer to the end of the previous record in the
file."
    (let ((pos (save-excursion
                 (rec-beginning-of-record)
                 (if (not (= (point) (point-min)))
                     (backward-char))
                 (when (and (re-search-backward rec-record-re nil t)
                            (rec-beginning-of-record))
                   (point)))))
      (when pos
        (goto-char pos)
        t)))

(defun rec-type-first-rec-pos (type)
  "Return the position of the first record of the specified TYPE.

If TYPE is nil then return the position of the first regular record in the file.
If there are no regular records in the file, return nil."
  (save-excursion
    (when (or (not type) (rec-type-p type))
      (if type
          (rec-goto-type type)
        (goto-char (point-min)))
      ;; Find the next regular record
      (when (and (rec-goto-next-rec)
                 (rec-regular-p))
        (point)))))

(defun rec-goto-type-first-rec (type)
  "Goto to the first record of type TYPE present in the file.
If TYPE is nil then goto to the first Unknown record on the file.

If the record is found, return its position.
If no such record exist then don't move and return nil."
  (let ((pos (rec-type-first-rec-pos type)))
    (when pos
      (goto-char pos))))

(defun rec-count (&optional type)
  "If TYPE is a string, return the number of records of the
specified type in the current file."
  (let (num
        (rec-file-name (if buffer-file-name
                           buffer-file-name
                         "")))
    (with-temp-buffer
      (if (stringp type)
        (call-process rec-recsel
                      nil ; infile
                      t   ; output to current buffer.
                      nil ; display
                      "-t" type "-c" rec-file-name)
        (call-process rec-recsel
                      nil ; infile
                      t   ; output to current buffer.
                      nil ; display
                      "-c" rec-file-name))
      (setq num (buffer-substring-no-properties (point-min) (point-max))))
    (string-to-number num)))

(defun rec-do (rec-do-func &optional type descriptors)
  "Apply REC-DO-FUNC for each record of type TYPE.  If TYPE is nil
then the function is applied for all the records existing in the
file (including the record descriptors).  A third optional
parameter specify whether to include the record descriptors in
the list of records."
  (save-excursion
    (save-restriction
      (widen)
      (unless (rec-goto-type type)
        (goto-char (point-min))
        (rec-goto-next-rec))
      (let ((in-type t)
            (first-time t)
            exit)
        (while (and (not exit)
                    in-type
                    (if (not first-time)
                        (rec-goto-next-rec)
                      t))
          (setq first-time nil)
          (setq in-type (or (not type)
                            (equal (rec-record-type) type)))
          (when (and in-type
                     (or descriptors
                         (rec-regular-p)))
            (setq exit (not (funcall rec-do-func)))))))))

(defun rec-map (rec-map-func &optional type descriptors)
  "XXX"
  (let (res)
    (rec-do (lambda ()
              (setq res
                    (cons (funcall rec-map-func)
                          res))
              t)
            type
            descriptors)
    (reverse res)))

(defun rec-regular-p ()
  "Return t if the record under point is a regular record.
Return nil otherwise."
  (let ((rec (rec-current-record)))
    (when rec
      (= (length (rec-record-assoc (list rec-keyword-rec) rec))
         0))))

(defun rec-record-type ()
  "Return the type of the record under point.

If the record is of no known type, return nil."
  (let ((descriptor (rec-record-descriptor)))
    (cond
     ((listp descriptor)
      (car (rec-record-assoc (list rec-keyword-rec)
                             (cadr descriptor))))
     ((equal descriptor "")
      "")
     (t
      nil))))

(defun rec-record-descriptor ()
  "Return the record descriptor of the record under point.

Return \"\" if no proper record descriptor is found in the file.
Return nil if the point is not on a record."
  (when (rec-current-record)
    (let ((descriptors rec-buffer-descriptors)
          descriptor type position found
          (i 0))
      (while (and (not found)
                  (< i (length descriptors)))
        (setq descriptor (nth i rec-buffer-descriptors))
        (setq position (marker-position (nth 2 descriptor)))
        (if (and (>= (point) position)
                 (or (= i (- (length rec-buffer-descriptors) 1))
                     (< (point) (marker-position (nth 2 (nth (+ i 1) rec-buffer-descriptors))))))
            (setq found t)
          (setq i (+ i 1))))
      (if found
          descriptor
        ""))))

;;;; Searching functions

(defun rec-search-first (type name value)
  "Return the position of the beginning of the record of type TYPE
containing a field NAME:VALUE.

If such a record is not found then return nil."
  (save-excursion
    (let (found end-of-type record)
      (when (rec-goto-type type)
        (while (and (not found) (not end-of-type)
                    (rec-goto-next-rec))
          ;; Read a record
          (setq record (rec-current-record))
          ;; Check if found
          (if (member value (rec-record-assoc name record))
              (setq found t)
            ;; Check end-of-type
            (if (rec-record-assoc (list rec-keyword-rec) record)
                (setq end-of-type t))))
        (when found (point))))))

;;;; Selecting

(defun rec-sel (what func &optional type write-descriptor)
  "Make a selection on the rec file.

FUNC is a function that will be evaluated XXX.

If some of the fields specified in WHAT does not exist in the
matching records, then they are not included in the result.

NAME is the name of a field, like \"Name\", that will be compared
with \"VALUE\".

If WRITE-DESCRIPTOR is t, then record descriptors are included in
the result buffer."
  (let ((sel-buffer
         (get-buffer-create (generate-new-buffer-name "Rec Sel ")))
        inserted-types)
    (with-current-buffer sel-buffer
      (insert "# -*- mode: rec -*- \n"
              "#\n"
              "# Result of rec-sel with \n# ")
      (print func (lambda (c) (unless (equal c ?\n) (insert c))))
      (insert "\n\n"))
    (message "Searching...")
    (rec-do
     (lambda ()
       (save-excursion
         (let* ((rec (rec-parse-record))
                (type (rec-record-type))
                (descriptor (rec-record-descriptor)))
           (when (apply func (list rec))
             ;; Matching record
             ;; Print it (the requested fields)
             (with-current-buffer sel-buffer
               (let (buffer-read-only)
                 (when (and descriptor
                            (not (member type inserted-types)))
                   (when write-descriptor
                     ;; Insert the type descriptor
                     (rec-insert-record (cadr descriptor)))
                   (insert "\n")
                   (setq inserted-types
                         (cons type inserted-types)))
                 (rec-insert-record rec what)
                 (insert "\n")))))
         t))
     type)
    (with-current-buffer sel-buffer
      (insert "\n"
              "# End of rec-sel\n")
      (goto-char (point-min))
      (rec-mode))
    (switch-to-buffer-other-window sel-buffer)
    (message "")))

;;;; Selection macros
;;
;; Note that in the context of the body in the following macros `rec'
;; is a record data structure.

(defmacro rec-field-eq (name val)
  `(let ((value (rec-record-assoc ,name rec)))
     (if value
         (equal (car value) ,val)
       nil)))

(defmacro rec-field-count (name)
  `(lenth (rec-record-assoc ,name rec)))

;;;; Navigation

(defun rec-show-type (type)
  "Show the records of the given type"
  (widen)
  (unless (rec-goto-type type)
    (message "No records of the requested type were found."))
  ;; Show the first data record of this type, if it exists.
  (if (and (not type)
           (save-excursion
             (let ((record-type (rec-record-type)))
               (and (rec-goto-next-rec)
                    (equal (rec-record-type) record-type)))))
      (rec-goto-next-rec))
  (rec-show-record))

(defun rec-show-record ()
  "Show the record under the point"
  (setq buffer-read-only t)
  (rec-narrow-to-record)
  (use-local-map rec-mode-map)
  ;; TODO: Update field names for autocompletion
  ;;  (let ((names (rec-record-field-names (rec-current-record)))))
  (rec-set-head-line nil)
  (rec-set-mode-line (rec-record-type)))

;;;; Mode line and Head line

(defun rec-set-mode-line (str)
  "Set the modeline in rec buffers."
  (when str
    (setq mode-line-buffer-identification
          (list 20
                "%b " str))))

(defun rec-set-head-line (str)
  "Set the headline in rec buffers."
  (setq header-line-format str))

;;;; Fast selection

(defun rec-fast-selection (names prompt)
  "Fast group tag selection with single keys.

NAMES is an association list of the form:

    ((\"NAME1\" char1) ...)

Each character should identify only one name."
  ;; Adapted from `org-fast-tag-selection' in org.el by Carsten Dominic
  ;; Thanks Carsten! :D
  (let* ((maxlen (apply 'max (mapcar (lambda (name)
                                       (string-width (car name))) names)))
         (buf (current-buffer))
         (fwidth (+ maxlen 3 1 3))
         (ncol (/ (- (window-width) 4) fwidth))
         name count result char i key-list)
    (save-window-excursion
      (set-buffer (get-buffer-create " *Rec Fast Selection*"))
      (delete-other-windows)
      (split-window-vertically)
      (switch-to-buffer-other-window (get-buffer-create " *Rec Fast Selection*"))
      (erase-buffer)
      (insert prompt ":")
      (insert "\n\n")
      (setq count 0)
      (while (setq name (pop names))
        (setq key-list (cons (cadr name) key-list))
        (insert "[" (cadr name) "] "
                (car name)
                (make-string (- fwidth 4 (length (car name))) ?\ ))
        (when (= (setq count (+ count 1)) ncol)
          (insert "\n")
          (setq count 0)))
      (goto-char (point-min))
      (if (fboundp 'fit-window-to-buffer)
          (fit-window-to-buffer))
      (catch 'exit
        (while t
          (message "[a-z0-9...]: Select entry   [RET]: Exit")
          (setq char (let ((inhibit-quit t)) (read-char-exclusive)))
          (cond
           ((= char ?\r)
            (setq result nil)
            (throw 'exit t))
           ((member char key-list)
            (setq result char)
            (throw 'exit t)))))
      result)))

(defun rec-selection-list ()
  "XXX"
  (let (search-list)
    ;; Add standard searches
    (add-to-list 'search-list
                 (list "Generic search" ?E))
    ;; Add custom searches
    (append search-list
            (mapcar
             (lambda (elem)
               (if (and (listp elem)
                        (>= (length elem) 4))
                   (let ((char (car elem))
                         (descr (cadr elem)))
                     (list descr char))
                 (error "Invalid entry in rec-custom-searches")))
             rec-custom-searches))))

(defun rec-init-selections ()
  "XXX"
  (let (res)
    (rec-do
     (lambda ()
       (let* ((rec (rec-parse-record))
              (name (rec-record-assoc (list "Name") rec))
              (letter (rec-record-assoc (list "Letter") rec))
              (fields (rec-record-assoc (list "Field") rec))
              (type (rec-record-assoc (list "Type") rec))
              (expr (rec-record-assoc (list "Predicate") rec)))
         (when (and (equal (length name) 1)
                    (equal (length letter) 1)
                    (equal (length (car letter)) 1)
                    (equal (length expr) 1))
           (add-to-list 'res
                        (list (aref (car letter) 0)
                              (car name)
                              (mapcar (lambda (elem)
                                        (rec-parse-field-name-from-string elem)) fields)
                              (read (car expr))
                              type)))
         t))
     "RecModeSelection")
    (setq rec-custom-searches res)))

;;;; Rec Idle mode
;;
;; This section is heavily inspired in semantic-idle.el

(defvar rec-idle-scheduler-timer nil
  "*Timer used to schedule tasks in idle time.")

(defvar rec-idle-scheduler-work-timer nil
  "*Timer used to schedule tasks in idle time that may take a
  while.")

(defcustom rec-idle-scheduler-idle-time 2
  "*Time in seconds of idle before scheduling events.
This time should be short enough to ensure that idle-scheduler
will be run as soon as Emacs is idle."
  :group 'rec
  :type 'number
  :set (lambda (sym val)
         (set-default sym val)
         (when (timerp rec-idle-scheduler-timer)
           (cancel-timer rec-idle-scheduler-timer)
           (setq rec-idle-scheduler-timer nil)
           (rec-idle-scheduler-setup-timers))))

(defcustom rec-idle-scheduler-work-idle-time 60
  "*Time in seconds of idle before scheduling big work.
This time should be long enough that once any big work is
started, it is unlikely the user would be ready to type again
right away."
  :group 'rec
  :type 'number
  :set (lambda (sym val)
         (set-default sym val)
         (when (timerp rec-idle-scheduler-work-timer)
           (cancel-timer rec-idle-scheduler-work-timer)
           (setq semantic-idle-scheduler-work-timer nil)
           (rec-idle-scheduler-setup-timers))))

(defun rec-idle-scheduler-setup-timers ()
  "Lazy initialization of the auto parse idle timer."
  (or (timerp rec-idle-scheduler-timer)
      (setq rec-idle-scheduler-timer
            (run-with-idle-timer
             rec-idle-scheduler-idle-time t
             #'rec-idle-scheduler-function)))
  (or (timerp rec-idle-scheduler-work-timer)
      (setq rec-idle-scheduler-work-timer
            (run-with-idle-timer
             rec-idle-scheduler-work-idle-time t
             #'rec-idle-scheduler-work-function))))

(defun rec-idle-scheduler-kill-timer ()
  "Kill the rec idle timer."
  (if (timerp rec-idle-scheduler-timer)
      (cancel-timer rec-idle-scheduler-timer))
  (setq rec-idle-scheduler-timer nil))

(defcustom rec-idle-scheduler-mode-hook nil
  "*HOok run at the end of function `rec-idle-scheduler-mode'."
  :group 'semantic
  :type 'hook)

(defvar rec-idle-scheduler-mode nil
  "Non-nil if idle-scheduler minor mode is enabled.
Use the command `rec-idle-scheduler-mode' to change this variable.")
(make-variable-buffer-local 'rec-idle-scheduler-mode)

(defcustom rec-idle-scheduler-max-buffer-size 0
  "*Maximum size in bytes of buffers where idle-scheduler is enabled.
If this value is less than or equal to 0, idle-schedule is enabled in
all buffers regardless of their size."
  :group 'rec
  :type 'number)

(defsubst rec-idle-scheduler-enabled-p ()
  "Return non-nil if idle-scheduler is enabled for this buffer.
idle-scheduler is disabled when debugging or if the buffer size
exceeds the `rec-idle-scheduler-max-buffer-size' threshold."
  (and rec-idle-scheduler-mode
       (or (<= rec-idle-scheduler-max-buffer-size 0)
           (< (buffer-size) rec-idle-scheduler-max-buffer-size))))

(defun rec-idle-scheduler-mode-setup ()
  "Setup option `rec-idle-scheduler-mode'.
The minor mode can be turned on only if rec is available.  When
minor mode is enabled process the current buffer if needed.
Return non-nil if the minor mode is enabled."
  (if rec-idle-scheduler-mode
      (if (not (featurep 'rec-mode))
          (progn
            ;; Disable minor mode if rec-mode not available
            (setq rec-idle-scheduler-mode nil)
            (error "Buffer %s was not set up idle time scheduling"
                   (buffer-name)))
        (rec-idle-scheduler-setup-timers)))
  rec-idle-scheduler-mode)

(defun rec-idle-scheduler-mode (&optional arg)
  "Minor mode to auto analyze buffer following a change.
When this mode is off, a buffer is only rescanned for record
types when some command requests the list of available types.
When idle-scheduler is enabled, Emacs periodically checks to see
if the buffer is out of date, and reanalyzes while the user is
idle (not typing).

With prefix argument ARG, turn on if positive, otherwise off.
The minor mode can be turned on only if rec-mode feature is
available and the current buffer is in rec mode.  Return non-nil
if the minor mode is enabled."
  (interactive
   (list (or current-prefix-arg
             (if rec-idle-scheduler-mode 0 1))))
  (setq rec-idle-scheduler-mode
        (if arg
            (>
             (prefix-numeric-value arg)
             0)
          (not rec-idle-scheduler-mode)))
  (rec-idle-scheduler-mode-setup)
  (run-hooks 'rec-idle-scheduler-mode-hook)
  (if (interactive-p)
      (message "rec-idle-scheduler minor mode %sabled"
               (if rec-idle-scheduler-mode "en" "dis")))
  ;; FIXME: add a note in the modeline
  rec-idle-scheduler-mode)

(defun rec-idle-scheduler-function ()
  "Function run when after `rec-idle-scheduler-idle-time'.
This function will reanalyze the current buffer, and if successful,
call additional functions registered with the timer calls."
  (when (zerop (recursion-depth))
    (let ((debug-on-error nil))
      (save-match-data (rec-idle-core-handler)))))

;;;; Commands
;;
;; The following functions are implementing commands available in the
;; modes.

(defun rec-cmd-edit-field ()
  "Edit the contents of the field under point in a separate
buffer"
  (interactive)
  (let* (edit-buf
         (field (rec-current-field))
         (field-value (rec-field-value field))
         (field-name (rec-field-name field))
         (pointer (rec-beginning-of-field-pos))
         (prev-buffer (current-buffer)))
    (if field-value
        (progn
          (setq edit-buf (get-buffer-create "Rec Edit"))
          (set-buffer edit-buf)
          (delete-region (point-min) (point-max))
          (rec-edit-field-mode)
          (make-local-variable 'rec-field-name)
          (setq rec-field-name field-name)
          (make-local-variable 'rec-marker)
          (setq rec-marker (make-marker))
          (set-marker rec-marker pointer prev-buffer)
          (make-local-variable 'rec-buffer)
          (setq rec-prev-buffer prev-buffer)
          (setq rec-pointer pointer)
          (insert field-value)
          (switch-to-buffer-other-window edit-buf)
          (goto-char (point-min))
          (message "Edit the value of the field and use C-c C-c to exit"))
      (message "Not in a field"))))

(defun rec-finish-editing-field ()
  "Stop editing the value of a field."
  (interactive)
  (let ((marker rec-marker)
        (prev-pointer rec-pointer)
        (edit-buffer (current-buffer))
        (name rec-field-name)
        (value (buffer-substring-no-properties (point-min) (point-max))))
    (delete-window)
    (switch-to-buffer rec-prev-buffer)
    (let ((buffer-read-only nil))
      (kill-buffer edit-buffer)
      (goto-char marker)
      (rec-delete-field)
      (rec-insert-field (list 'field
                              name
                              value))
      (goto-char prev-pointer)))
  (rec-init-selections))

(defun rec-beginning-of-field ()
  "Goto to the beginning of the current field"
  (interactive)
  (let ((pos (rec-beginning-of-field-pos)))
    (when pos
      (goto-char pos))))

(defun rec-end-of-field ()
  "Goto to the end of the current field"
  (interactive)
  (let ((pos (rec-end-of-field-pos)))
    (when pos
      (goto-char pos))))

(defun rec-beginning-of-record ()
  "Goto to the beginning of the current record"
  (interactive)
  (let ((pos (rec-beginning-of-record-pos)))
    (when pos
      (goto-char pos))))

(defun rec-end-of-record ()
  "Goto to the end of the current record"
  (interactive)
  (let ((pos (rec-end-of-record-pos)))
    (when pos
      (goto-char pos))))

(defun rec-kill-field ()
  "Kill the current field"
  (interactive)
  (let ((begin-pos (rec-beginning-of-field-pos))
        (end-pos (rec-end-of-field-pos)))
    (when (and begin-pos end-pos)
      (kill-region begin-pos end-pos))))

(defun rec-copy-field ()
  "Copy the current field"
  (interactive)
  (let ((begin-pos (rec-beginning-of-field-pos))
        (end-pos (rec-end-of-field-pos)))
    (when (and begin-pos end-pos)
      (copy-region-as-kill begin-pos end-pos))))

(defun rec-delete-field ()
  "Delete the current field"
  (interactive)
  (let ((begin-pos (rec-beginning-of-field-pos))
        (end-pos (rec-end-of-field-pos)))
    (when (and begin-pos end-pos)
      (delete-region begin-pos end-pos))))

(defun rec-copy-record ()
  "Copy the current record"
  (interactive))

(defun rec-find-type ()
  "Goto the beginning of the descriptor with a given type."
  (interactive)
  (let ((type (completing-read "Record type: "
                               (save-restriction
                                 (widen)
                                 (rec-buffer-types)))))
    (if (equal type "") (setq type nil))
    (rec-show-type type)))

(defun rec-cmd-goto-next-field ()
  "Move the pointer to the beginning of the next field in the
record.  Interactive version."
  (interactive)
  (if (save-excursion
        (not (rec-goto-next-field)))
      (if rec-editing
          (progn
            (goto-char (point-min))
            (unless (looking-at rec-field-name-re)
              (rec-goto-next-field)))
      (rec-beginning-of-record))
    (rec-goto-next-field)))

(defun rec-cmd-goto-next-rec ()
  "Move the pointer to the beginning of the next record in the
file.  Interactive version."
  (interactive)
  (widen)
  (let ((record-type (rec-record-type)))
    (if (save-excursion
          (and (rec-goto-next-rec)
               (equal (rec-record-type) record-type)
               (not (rec-record-descriptor-p (rec-current-record)))))
        (rec-goto-next-rec)
      (if (not (rec-record-type))
          (message "No more records")
        (message (concat "No more records of type "
                         (rec-record-type))))))
  (unless rec-editing
    (rec-show-record)))

(defun rec-cmd-goto-previous-rec ()
  "Move the pointer to the beginning of the previous record in
the file.  Interactive version."
  (interactive)
  (widen)
  (let ((record-type (rec-record-type)))
    (if (save-excursion
          (and (rec-goto-previous-rec)
               (equal (rec-record-type) record-type)
               (not (rec-record-descriptor-p (rec-current-record)))))
        (rec-goto-previous-rec)
      (if (not (rec-record-type))
          (message "No more records")
        (message (concat "No more records of type "
                         (rec-record-type))))))
  (unless rec-editing
    (rec-show-record)))

(defun rec-cmd-jump ()
  "Jump to the first record containing the reference under
point."
  (interactive)
  (widen)
  (let (size field name value)
    (if (setq field (rec-current-field))
        (progn (setq name (rec-field-name field))
               (setq value (rec-field-value field))
               (if (or (= (length name) 2)
                       (= (length name) 3))
                   (progn
                     (let* ((field-type (nth 0 name))
                            (field-name (nth 1 name))
                            (pos (rec-search-first field-type
                                                   (list field-name)
                                                   value)))
                       (if pos
                           (progn
                             (setq rec-jump-back (point-marker))
                             (goto-char pos)
                             (unless rec-editing
                               (rec-narrow-to-record)))
                         (message "Not found.")
                         (unless rec-editing
                           (rec-show-record)))))
                 (message "Not in a reference.")
                 (unless rec-editing
                   (rec-show-record))))
      (message "Not in a reference.")
      (save-excursion
        (rec-goto-previous-rec)
        (unless rec-editing
          (rec-show-record))))))

(defun rec-cmd-jump-back ()
  "Undo the previous jump"
  (interactive)
  (if rec-jump-back
      (progn
        (widen)
        (goto-char (marker-position rec-jump-back))
        (unless rec-editing
          (rec-show-record))
        (setq rec-jump-back nil))
    (message "No previous position to jump")))

(defun rec-edit-record ()
  "Go to the record edition mode"
  (interactive)
  (setq rec-editing t)
  (setq buffer-read-only nil)
  (use-local-map rec-mode-edit-map)
  (rec-set-head-line "Editing record - use C-cC-c to return to navigation mode")
  (rec-set-mode-line "Edit record")
  (setq rec-update-p t)
  (setq rec-preserve-last-newline t))

(defun rec-edit-type ()
  "Go to the type edition mode"
  (interactive)
  (setq rec-editing t)
  (setq buffer-read-only nil)
  (use-local-map rec-mode-edit-map)
  (widen)
  (rec-narrow-to-type (rec-record-type))
  (setq rec-update-p t)
  (goto-char (point-min))
  (rec-set-head-line (concat "Editing type "
                             "'" (rec-record-type) "'"
                             " - use C-cC-c to return to navigation mode"))
  (rec-set-mode-line "Edit type"))

(defun rec-edit-buffer ()
  "Go to the buffer edition mode"
  (interactive)
  (setq rec-editing t)
  (setq buffer-read-only nil)
  (use-local-map rec-mode-edit-map)
  (widen)
  (setq rec-update-p t)
  (goto-char (point-min))
  (rec-set-head-line "Editing buffer - use C-cC-c to return to navigation mode")
  (rec-set-mode-line "Edit buffer"))

(defun rec-finish-editing ()
  "Go back from the record edition mode"
  (interactive)
  (or (rec-current-record)
      (rec-goto-next-rec)
      (rec-goto-previous-rec))
  (when rec-preserve-last-newline
    (save-excursion
      (goto-char (point-max))
      (unless (equal (char-before) ?\n)
        (insert ?\n))))
  (when rec-update-p
    (save-restriction
      (widen)
      (rec-update-buffer-descriptors))
    (setq rec-update-p nil))
  (rec-show-record)
  (rec-set-head-line nil)
  (rec-set-mode-line (rec-record-type))
  (setq rec-editing nil)
  (rec-init-selections)
  (message "End of edition"))

(defun rec-cmd-show-descriptor ()
  "Show the descriptor record of the current record.

This jump sets jump-back."
  (interactive)
  (let ((type (rec-record-type)))
    (when type
      (setq rec-jump-back (point-marker))
      (if rec-editing
          (rec-goto-type type)
        (rec-show-type type)))))

(defun rec-cmd-count ()
  "Display a message in the minibuffer showing the number of
records of the current type"
  (interactive)
  (message "Counting records...")
  (let ((type (rec-record-type)))
    (message (concat (number-to-string (rec-count type))
                     (if (or (not type)
                             (equal type ""))
                         " records"
                       (concat " records of type " type))))))

(defun rec-cmd-append-field ()
  "Goto the end of the record and switch to edit record mode."
  (interactive)
  (unless rec-editing
    (rec-edit-record)
    (goto-char (point-max))
    (insert "\n")
    (backward-char)))

(defun rec-cmd-sel ()
  "XXX"
  (interactive)
  (let ((res (rec-fast-selection
              (rec-selection-list)
              "Selection Type")))
    (cond
     ((equal res ?E)
      ;; Prompt the user for an expression and search
      (let (what func name type)
        (setq type (read-from-minibuffer "Type of record (empty for all): "))
        (if (equal type "")
            (setq type nil)
          (when (not (member type (rec-buffer-types)))
            (error (concat "cannot find type " type))))
        (while (not (equal
                     (setq name
                           (read-from-minibuffer "Field to print (empty when done): "))
                     ""))
          (add-to-list 'what
                       (rec-parse-field-name-from-string name)))
        (setq func (read-from-minibuffer "Search expression: "))
        (rec-sel what (read (concat "(lambda (rec) " func ")")) type)))
     (res
      ;; Launch the appropriate expression
      (let* ((search (assoc res rec-custom-searches))
             (what (nth 2 search))
             (func (nth 3 search))
             (type (nth 4 search)))
        (rec-sel what (list 'lambda (list 'rec) func) type))))))

(defun rec-cmd-trim-field-value ()
  "Trim the value of the field under point, if any."
  (interactive)
  (save-excursion
    (let ((buffer-read-only nil)
          (field (rec-current-field)))
      (setq field (rec-field-trim-value field))
      (rec-delete-field)
      (rec-insert-field field))))

(defun rec-cmd-compile ()
  "Compile the current file with recfix."
  (interactive)
  (let ((cur-buf (current-buffer))
        (cmd (concat rec-recfix " "))
        (tmpfile (make-temp-file "rec-mode-")))
    (if buffer-file-name
        (setq cmd (concat cmd buffer-file-name))
      (with-temp-file tmpfile
        (insert-buffer cur-buf))
      (setq cmd (concat cmd tmpfile)))
    (compilation-start cmd)))

(defun rec-cmd-show-info ()
  "Show information about the recfile in the modeline."
  (interactive)
  (let ((cur-buf (current-buffer))
        (filename (if buffer-file-name
                      buffer-file-name
                    (make-temp-file "rec-mode-")))
        (msg ""))
    (if (not buffer-file-name)
        (with-temp-file tmpfile
          (insert-buffer cur-buf)))
    (with-temp-buffer
      (call-process rec-recinf
                    nil ; infile
                    t   ; output to current buffer
                    nil ; display
                    filename)
      (setq msg (buffer-substring-no-properties (point-min)
                                                (point-max))))
    ;; Delete temporary file.
    (if (not buffer-file-name)
        (delete-file filename))
    ;; Show the message.
    (setq msg (replace-regexp-in-string "\n$" "" msg))
    (setq msg (replace-regexp-in-string "\n" ", " msg))
    (message msg)))

(defun rec-cmd-beginning-of-line ()
  "Move the point to the beginning of the current line.

If the current line is part of the value of a field then go to
the first character of the line being part of the value."
  (interactive)
  (beginning-of-line)
  ;; Skip a field name or a continuation line.
  (cond
   ((looking-at rec-field-name-re)
    (rec-parse-field-name)
    (when (looking-at " ") (forward-char 1)))
   ((looking-at "\+ ?")
    (forward-char 1)
    (when (looking-at " ") (forward-char 1)))))

;;;; Definition of modes

(defun rec-mode ()
  "A major mode for editing rec files.

Commands:
\\{rec-mode-map}"
  (interactive)
  (kill-all-local-variables)
  ;; Local variables
  (make-local-variable 'font-lock-defaults)
  (make-local-variable 'rec-type)
  (make-local-variable 'rec-buffer-descriptors)
  (make-local-variable 'rec-jump-back)
  (make-local-variable 'rec-update-p)
  (make-local-variable 'rec-preserve-last-newline)
  (make-local-variable 'rec-editing)
  (make-local-variable 'rec-field-names)
  (make-local-variable 'rec-custom-searches)
  (setq rec-editing nil)
  (setq rec-jump-back nil)
  (setq rec-update-p nil)
  (setq rec-preserve-last-newline nil)
  (setq rec-field-names nil)
  (setq rec-custom-searches nil)
  (setq font-lock-defaults '(rec-font-lock-keywords))
  (use-local-map rec-mode-map)
  (set-syntax-table rec-mode-syntax-table)
  (setq mode-name "Rec")
  (setq major-mode 'rec-mode)
  ;; Goto the first record of the first type (including the Unknown)
  (rec-update-buffer-descriptors)
  ;; Initialize the value of rec-custom-searches
;;  (rec-init-selections)
  (if (equal rec-open-mode 'navigation)
    (progn
      (setq buffer-read-only t)
      (setq rec-type (car (rec-buffer-types)))
      (rec-show-type rec-type))
    ;; Edit mode
    (use-local-map rec-mode-edit-map)
    (setq rec-editing t)
    (rec-set-mode-line "Edit buffer")))

(defvar rec-edit-field-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'rec-finish-editing-field)
    map)
  "Keymap for rec-edit-field-mode")

(defun rec-edit-field-mode ()
  "A major mode for editing rec field values.

Commands:
\\{rec-edit-field-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map rec-edit-field-mode-map)
  (setq mode-name "Rec Edit")
  (setq major-mode 'rec-edit-field-mode))

(provide 'rec-mode)

;; Local variables:
;; outline-regexp: ";;;;"
;; End:

;;; rec-mode.el ends here
