;;; gtk-look-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads (gtk-lookup-symbol gtk-lookup) "gtk-look" "gtk-look.el"
;;;;;;  (20919 22347 0 0))
;;; Generated autoloads from gtk-look.el

(let ((loads (get 'gtk-lookup 'custom-loads))) (if (member '"gtk-look" loads) nil (put 'gtk-lookup 'custom-loads (cons '"gtk-look" loads))))

(autoload 'gtk-lookup-symbol "gtk-look" "\
Lookup Gtk/Gnome documentation for SYMBOL.
SYMBOL is a string name of a function, variable, type, etc, in
Gtk, Gnome and related libraries like Pango.  The symbol is
sought in \"devhelp\" indexes (see `gtk-lookup-devhelp-indices'),
and the HTML documentation is displayed with `browse-url'.

The lookup tries first case-sensitively, then insensitively, for
ease of typing in a name.

Interactively SYMBOL is prompted for, with completions from the
available documentation.  The default is the function, variable,
type, etc at point.  Transformations are applied to make a C name
from forms used in

    * Gtk2-Perl   (URL `http://gtk2-perl.sourceforge.net/')
    * Guile-Gnome (URL `http://www.gnu.org/software/guile-gnome/')
    * Guile-Gtk   (URL `http://www.gnu.org/software/guile-gtk/')

For example with point on a Perl call like \"Gtk2::Label->new\"
the default offered is \"gtk_label_new\".  This is independent of
the major mode, so you can have code in one style and comments in
another.  If `browse-url' displays in a buffer you can even
lookup from the browser buffer if there's no link already
\(sample code, or a few cross references from Gtk to Pango).

When point is on a \"method\" name like just \"set_size_request\"
in Gtk2-Perl or Guile-Gnome the default is expanded to the full
name like \"gtk_widget_set_size_request\" if unique.  Or if
there's multiple candidates then a *Completions* window is
presented which you can switch to with \\<minibuffer-local-completion-map>\\[switch-to-completions] and select in the
usual way.

`browse-url' is used to display the documentation.  If it
displays in an Emacs buffer (like say `w3m' does) then that's put
in an \"other window\" below the current, similar to
`info-lookup' on Info docs.  You can customize
`browse-url-browser-function' to choose the viewer and with some
regexps there you can even have one browser for Gtk documents
\"file:///usr/share/gtk-doc/html/...\" and another browser for
other things.

The `completing-read' for the symbol demands a match, since
gtk-lookup-symbol can only go to the links available in the
devhelp indexes.  The full set of Gtk symbols is pretty big, so
you might try one of the completions add-ons like Icicles to help
searching or browsing.

The gtk-look home page is
URL `http://user42.tuxfamily.org/gtk-look/index.html'

\(fn SYMBOL)" t nil)

;;;***

;;;### (autoloads nil nil ("gtk-look-pkg.el") (20919 22347 214528
;;;;;;  689000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; gtk-look-autoloads.el ends here
