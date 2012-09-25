;;; http-post-simple-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (http-post-simple-multipart http-post-simple) "http-post-simple"
;;;;;;  "http-post-simple.el" (20577 57316 0 0))
;;; Generated autoloads from http-post-simple.el

(autoload 'http-post-simple "http-post-simple" "\
Send FIELDS to URL as an HTTP POST request, returning the response
and response headers.
FIELDS is an alist, eg ((field-name . \"value\")); all values
need to be strings, and they are encoded using CHARSET,
which defaults to 'utf-8

\(fn URL FIELDS &optional CHARSET)" nil nil)

(autoload 'http-post-simple-multipart "http-post-simple" "\
Send FIELDS and FILES to URL as a multipart HTTP POST, returning the
response and response headers.
FIELDS is an alist, as for `http-post-simple', FILES is an a list of
\(fieldname \"filename\" \"file MIME type\" \"file data\")*

\(fn URL FIELDS FILES &optional CHARSET)" nil nil)

;;;***

;;;### (autoloads nil nil ("http-post-simple-pkg.el") (20577 57316
;;;;;;  729142 888000))

;;;***

(provide 'http-post-simple-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; http-post-simple-autoloads.el ends here
