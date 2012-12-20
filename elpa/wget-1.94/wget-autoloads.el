;;; wget-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (wget-web-page wget) "wget" "wget.el" (20691 15741
;;;;;;  0 0))
;;; Generated autoloads from wget.el

(autoload 'wget "wget" "\
Wget interface to download URI asynchronously.
If argument ARG is non-nil, ask some options.
Called with prefix argument, turn argument ARG t.

If you are in dired mode which is seeing ftp directory,
`wget' regard current line file name as URI.

\(fn URI &optional ARG)" t nil)

(autoload 'wget-web-page "wget" "\
Wget interface to download whole Web page.
If argument ARG is non-nil, ask options.
Called with prefix argument, turn argument ARG t.

Second argument URI is string.
wget-web-page downlod whole Web page from it following relative link.

\(fn URI &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("lpath.el" "w3-wget.el" "w3m-wget.el"
;;;;;;  "wget-pkg.el" "wget-sysdep.el") (20691 15741 598058 893000))

;;;***

(provide 'wget-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; wget-autoloads.el ends here
