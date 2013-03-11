;;; elpakit-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (elpakit-isearch-hook-jack-in elpakit-test elpakit-start-server
;;;;;;  elpakit-list-processes elpakit elpakit-eval elpakit-make-multi)
;;;;;;  "elpakit" "elpakit.el" (20797 39419 0 0))
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

PACKAGE-LIST is either a list of local directories to package or
a list of two items beginning with the symbol `:archive' and
specifying an existing directory archive in `packages-archives'.

If PACKAGE-LIST is not an `:archive' reference then the package
directories specified are turned into packages in the
DESTINATION.

If PACKAGE-LIST is an `:archive' reference then the specified
archive is copied to DESTINATION.

For example, if `pacckage-archives' is:

 '((\"local\" . \"/tmp/my-elpakit\")(\"gnu\" . \"http://gnu.org/elpa/\"))

and `elpakit' is called like this:

 (elpakit \"/tmp/new-elpakit\" (list :archive \"local\"))

then /tmp/my-elpakit will be copied to /tmp/new-elpakit.

\(fn DESTINATION PACKAGE-LIST &optional DO-TESTS)" nil nil)

(autoload 'elpakit-list-processes "elpakit" "\
List running elpakit processes.

Uses `elpakit-process-list-mode' to display the currently running
elpakit processes from batch tests and daemons.

\(fn)" t nil)

(autoload 'elpakit-start-server "elpakit" "\
Start a server with the PACKAGE-LIST.

If TEST is `t' then we run tests in the daemon.

If EXTRA-LISP is a list then that is passed into the daemon to be
executed as extra initialization.  If EXTRA-LISP is specified
then automatic requiring of the INSTALL is not performed.  If
EXTRA-LISP and TEST is specified then tests are done *after*
EXTRA-LISP.  EXTRA-LISP must do the require in that case.

If PRE-LISP is a list then it is passed into the daemon as Lisp
to be executed before initialization.  This is where any
customization that you need should go.

You can manage running servers with the `elpakit-list-processes'
command.

\(fn PACKAGE-LIST INSTALL &key TEST PRE-LISP EXTRA-LISP)" nil nil)

(autoload 'elpakit-test "elpakit" "\
Run tests on package INSTALL of the specified PACKAGE-LIST.

TEST is an ERT test selector.

If EXTRA-LISP is a list then that is passed into the test-process
to be executed as extra initialization.  If EXTRA-LISP and TEST
is specified then tests are done *after* EXTRA-LISP.  EXTRA-LISP
must do the require in that case.

If PRE-LISP is a list then it is passed into the test-process as
Lisp to be executed before initialization.  This is where any
customization that you need should go.

You can manage running processes with the `elpakit-list-processes'
command.

\(fn PACKAGE-LIST INSTALL TEST &key PRE-LISP EXTRA-LISP)" t nil)

(autoload 'elpakit-isearch-hook-jack-in "elpakit" "\
Jack in Elpakit to isearch. Call from `elisp-mode-hook'.

Adds `elpakit-multi-occur' to `isearch' with `M-o'.

Use something like:

 (add-hook 'emacs-lisp-mode-hook 'elpakit-isearch-hook-jack-in)

in your configuration file to make it happen.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil nil ("elpakit-pkg.el") (20797 39419 862417
;;;;;;  326000))

;;;***

(provide 'elpakit-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; elpakit-autoloads.el ends here
