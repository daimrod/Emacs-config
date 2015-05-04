;;; debbugs-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "debbugs-browse" "debbugs-browse.el" (21831
;;;;;;  12250 753123 208000))
;;; Generated autoloads from debbugs-browse.el

(autoload 'debbugs-browse-mode "debbugs-browse" "\
Browse GNU Debbugs bug URLs with debbugs-gnu or debbugs-org.
With a prefix argument ARG, enable Debbugs Browse mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.
The customer option `debbugs-browse-function' controls, which of
the two packages is used for showing bugs.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "debbugs-gnu" "debbugs-gnu.el" (21831 12250
;;;;;;  221123 185000))
;;; Generated autoloads from debbugs-gnu.el

(autoload 'debbugs-gnu-search "debbugs-gnu" "\
Search for Emacs bugs interactively.
Search arguments are requested interactively.  The \"search
phrase\" is used for full text search in the bugs database.
Further key-value pairs are requested until an empty key is
returned.  If a key cannot be queried by a SOAP request, it is
marked as \"client-side filter\".

\(fn)" t nil)

(autoload 'debbugs-gnu "debbugs-gnu" "\
List all outstanding bugs.

\(fn SEVERITIES &optional PACKAGES ARCHIVEDP SUPPRESS TAGS)" t nil)

(autoload 'debbugs-gnu-usertags "debbugs-gnu" "\
List all user tags for USERS, which is (\"emacs\") by default.

\(fn &rest USERS)" t nil)

(autoload 'debbugs-gnu-bugs "debbugs-gnu" "\
List all BUGS, a list of bug numbers.

\(fn &rest BUGS)" t nil)

;;;***

;;;### (autoloads nil "debbugs-org" "debbugs-org.el" (21831 12249
;;;;;;  133123 137000))
;;; Generated autoloads from debbugs-org.el

(autoload 'debbugs-org-search "debbugs-org" "\
Search for bugs interactively.
Search arguments are requested interactively.  The \"search
phrase\" is used for full text search in the bugs database.
Further key-value pairs are requested until an empty key is
returned.

\(fn)" t nil)

(autoload 'debbugs-org "debbugs-org" "\
List all outstanding bugs.

\(fn SEVERITIES &optional PACKAGES ARCHIVEDP SUPPRESS TAGS)" t nil)

(autoload 'debbugs-org-mode "debbugs-org" "\
Minor mode for providing a debbugs interface in org-mode buffers.

\\{debbugs-org-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'debbugs-org-bugs "debbugs-org" "\
List all BUGS, a list of bug numbers.

\(fn &rest BUGS)" t nil)

;;;***

;;;### (autoloads nil nil ("debbugs-pkg.el" "debbugs.el") (21831
;;;;;;  12251 426133 451000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; debbugs-autoloads.el ends here
