;;; elpakit-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (elpakit-test elpakit elpakit-eval elpakit-make-multi)
;;;;;;  "elpakit" "elpakit.el" (20750 17785 0 0))
;;; Generated autoloads from elpakit.el

(autoload 'elpakit-make-multi "elpakit" "\
Make a multi-package.

If the directory you are in is a package then it is built,
otherwise this asks you for a package directory.

Opens the directory the package has been built in.

\(fn PACKAGE-DIR)" t nil)

(autoload 'elpakit-eval "elpakit" "\
Eval all the elisp files in PACKAGE-LIST.

\(fn PACKAGE-LIST)" nil nil)

(autoload 'elpakit "elpakit" "\
Make a package archive at DESTINATION from PACKAGE-LIST.

\(fn DESTINATION PACKAGE-LIST &optional DO-TESTS)" nil nil)

(autoload 'elpakit-test "elpakit" "\


\(fn PACKAGE-LIST INSTALL TEST)" t nil)

;;;***

;;;### (autoloads nil nil ("elpakit-pkg.el") (20750 17785 542416
;;;;;;  505000))

;;;***

(provide 'elpakit-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; elpakit-autoloads.el ends here
