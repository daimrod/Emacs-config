;;; debbugs-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "../elpa/packages/debbugs/debbugs-browse" "../elpa/packages/debbugs/debbugs-browse.el"
;;;;;;  (22062 14679 794846 230000))
;;; Generated autoloads from ../elpa/packages/debbugs/debbugs-browse.el

(autoload 'debbugs-browse-mode "../elpa/packages/debbugs/debbugs-browse" "\
Browse GNU Debbugs bug URLs with debbugs-gnu or debbugs-org.
With a prefix argument ARG, enable Debbugs Browse mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.
The customer option `debbugs-browse-function' controls, which of
the two packages is used for showing bugs.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "../elpa/packages/debbugs/debbugs-gnu" "../elpa/packages/debbugs/debbugs-gnu.el"
;;;;;;  (22062 15528 426870 167000))
;;; Generated autoloads from ../elpa/packages/debbugs/debbugs-gnu.el

(autoload 'debbugs-gnu-search "../elpa/packages/debbugs/debbugs-gnu" "\
Search for Emacs bugs interactively.
Search arguments are requested interactively.  The \"search
phrase\" is used for full text search in the bugs database.
Further key-value pairs are requested until an empty key is
returned.  If a key cannot be queried by a SOAP request, it is
marked as \"client-side filter\".

\(fn)" t nil)

(autoload 'debbugs-gnu "../elpa/packages/debbugs/debbugs-gnu" "\
List all outstanding bugs.

\(fn SEVERITIES &optional PACKAGES ARCHIVEDP SUPPRESS TAGS)" t nil)

(autoload 'debbugs-gnu-usertags "../elpa/packages/debbugs/debbugs-gnu" "\
List all user tags for USERS, which is (\"emacs\") by default.

\(fn &rest USERS)" t nil)

(autoload 'debbugs-gnu-bugs "../elpa/packages/debbugs/debbugs-gnu" "\
List all BUGS, a list of bug numbers.

\(fn &rest BUGS)" t nil)

;;;***

;;;### (autoloads nil "../elpa/packages/debbugs/debbugs-org" "../elpa/packages/debbugs/debbugs-org.el"
;;;;;;  (22062 14679 794846 230000))
;;; Generated autoloads from ../elpa/packages/debbugs/debbugs-org.el

(autoload 'debbugs-org-search "../elpa/packages/debbugs/debbugs-org" "\
Search for bugs interactively.
Search arguments are requested interactively.  The \"search
phrase\" is used for full text search in the bugs database.
Further key-value pairs are requested until an empty key is
returned.

\(fn)" t nil)

(autoload 'debbugs-org "../elpa/packages/debbugs/debbugs-org" "\
List all outstanding bugs.

\(fn SEVERITIES &optional PACKAGES ARCHIVEDP SUPPRESS TAGS)" t nil)

(autoload 'debbugs-org-mode "../elpa/packages/debbugs/debbugs-org" "\
Minor mode for providing a debbugs interface in org-mode buffers.

\\{debbugs-org-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'debbugs-org-bugs "../elpa/packages/debbugs/debbugs-org" "\
List all BUGS, a list of bug numbers.

\(fn &rest BUGS)" t nil)

;;;***

;;;### (autoloads nil nil ("../elpa/packages/debbugs/debbugs.el")
;;;;;;  (22062 14679 794846 230000))

;;;***

(provide 'debbugs-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; debbugs-autoloads.el ends here
