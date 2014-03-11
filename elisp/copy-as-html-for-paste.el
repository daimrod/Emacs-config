;;; copy-as-html-for-paste.el
;; Author: tungd on #emacs
;;
;; I wanted some code highlighting to paste into Google Docs,
;; and people suggest `htmlize' the buffer, open the HTML file
;; in browser then copy-paste the content into the docs. This
;; works, but it is not pragmatic.
;;
;; Then I have free time after the lunch, I set out to really
;; understand the process. Emacs can do this, for sure, since Chrome
;; is already able to set rich clipboard, just need to figure out 
;; how.
;; 
;; Google give me this http://www.jwz.org/doc/x-cut-and-paste.html
;;
;; Basically when copy, the apps only register with the system that
;; it's going to provide the content for the next paste, and nothing
;; else. So there's really no such thing as the clipboard, and that's
;; why we lose the clipboard every time the app we want to copy quit.
;; When paste, the two apps communicate and negotiate what's the common
;; format they want to exchange the clipboard data in.
;;
;; Emacs doesn't know about `text/html' format, so it will only 
;; provide plain text when asked for paste.

(require 'select)
(require 'htmlize)

(defun xselect-convert-to-html (_selection _type value)
  (cond ((and (consp value) 
              (markerp (car value))
              (markerp (cdr value)))
         (let ((value (xselect--selection-bounds value)))
           (with-current-buffer (nth 2 value)
             (htmlize-region-for-paste (nth 0 value) (nth 1 value)))))
        (t (xselect--encode-string 'UTF8_STRING value t))))

(add-to-list 'selection-converter-alist
             (cons 'text/html 'xselect-convert-to-html) t)

;;;###autoload
(defun copy-as-html-for-paste (&optional beg end)
  (interactive "*r")
  (let* ((beg (or beg (point-min)))
         (end (or end (point-max))))
    (x-set-selection
     'CLIPBOARD
     (cons (copy-marker beg) (copy-marker end)))))

(provide 'copy-as-html-for-paste)
