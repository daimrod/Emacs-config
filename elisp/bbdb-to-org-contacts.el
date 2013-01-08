(require 'bbdb)
(require 'bbdb-com)

(defvar bbdb-to-org-contacts-record-prefix "*")

(defvar bbdb-to-org-contacts-record-blanks "  ")

(defun bbdb-to-org-contacts (to-file)
  "outputs a org-contacts file"
  (interactive (list (read-file-name "Save in file: ")))
  (let* ((filename (expand-file-name to-file))
         (records (bbdb-records)))
    (find-file filename)
    (while records
      (bbdb-record-to-org-record (car records))
      (setq records (cdr records)))))


(defun bbdb-record-to-org-record (record)
  "converts a single record"
  (let* ((name    (bbdb-record-name record))
         (company (bbdb-record-company record))
         (net     (bbdb-record-net record))
         (aka     (bbdb-record-aka record))
         (phone   (bbdb-record-phones record))
         (address (bbdb-record-addresses record))
         (notes   (bbdb-record-notes record)))
    
    (insert
     (format "%s %s\n" bbdb-to-org-contacts-record-prefix name))
    (insert
     (format "%s :PROPERTIES:\n" bbdb-to-org-contacts-record-blanks))

    (when aka
      (insert
       (format "%s :AKA:\t%s\n" bbdb-to-org-contacts-record-blanks
               (mapconcat (function (lambda(str) str)) aka ", "))))

    (when net
      (insert
       (format "%s :EMAIL:\t%s\n" bbdb-to-org-contacts-record-blanks
               (mapconcat (function (lambda(str) str)) net " "))))

    (when company
      (insert
       (format "%s :COMPANY:\t%s\n" bbdb-to-org-contacts-record-blanks company)))

    (when phone
      (insert
       (mapconcat
        (function (lambda(rec) 
                    (if (stringp (elt rec 1))
                        (format "%s :PHONE_%s:\t%s"
                                bbdb-to-org-contacts-record-blanks
                                (upcase (elt rec 0))
                                (elt rec 1))
                      (let ((len (length rec))
                            (count 2)
                            (output (format "%d" (elt rec 1))))
                        (while (< count len)
                          (setq output
                                (concat output
                                        (format "-%d"
                                                (elt rec count))))
                          (setq count (1+ count)))
                        (format "%s :PHONE_%s:\t%s"
                                bbdb-to-org-contacts-record-blanks
                                (upcase (elt rec 0)) output)))))
        phone "\n"))
      (insert "\n"))

    (insert
     (format "%s :END:\n" bbdb-to-org-contacts-record-blanks))

    (when notes
      (insert
       (format "%s - %s\n" bbdb-to-org-contacts-record-blanks notes)))))


(provide 'bbdb-to-org-contacts)
