;;; gtk-look.el --- lookup Gtk and Gnome documentation.

;; Copyright 2004, 2006, 2007, 2008, 2009, 2010 Kevin Ryde

;; Author: Kevin Ryde <user42@zip.com.au>
;; Version: 19
;; Keywords: tools, c
;; URL: http://user42.tuxfamily.org/gtk-look/index.html
;; EmacsWiki: GtkLook

;; gtk-look.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; gtk-look.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.


;;; Commentary:

;; M-x gtk-lookup-symbol displays HTML documentation for Gtk and Gnome
;; functions and variables, similar to what M-x info-lookup-symbol does for
;; info files.  The documentation is expected to be HTML files with devhelp
;; indexes, like the Debian packages libgtk2.0-doc etc.  See the
;; `gtk-lookup-symbol' docstring below for more.

;;; Install:

;; Put gtk-look.el in one of your `load-path' directories, and in your
;; .emacs add
;;
;;     (autoload 'gtk-lookup-symbol "gtk-look" nil t)
;;
;; This makes M-x gtk-lookup-symbol available, but you'll probably want to
;; bind it to a key.  C-h C-j is one possibility, being close to C-h C-i for
;; `info-lookup-symbol'.  For instance to do that globally,
;;
;;     (define-key global-map [?\C-h ?\C-j] 'gtk-lookup-symbol)

;;; Emacsen:

;; Designed for Emacs 21 and 22.  Works in XEmacs 21 if you copy
;; `file-expand-wildcards' from Emacs, and byte-compile to avoid slowness in
;; the other compatibility code below.

;;; History:

;; Version 1 - the first version
;; Version 2 - correction to usual .devhelp file locations
;; Version 3 - recognise devhelp2 format
;; Version 4 - home page link, more parens on funcs in index,
;;             fix lookup done from within an existing browser buffer
;; Version 5 - make browse other window work in xemacs
;; Version 6 - use with-compression-mode and display-warning, when available
;; Version 7 - gtk2-perl support, no longer use info-look for symbol at point
;; Version 8 - fix perl Glib::G_FOO and UIManager symbol munging
;; Version 9 - leave cache uninitialized if C-g interrupt during cache build,
;;             fix camel case VScrollbar symbol munging
;; Version 10 - fix preferring .devhelp2 files over .devhelp when not .gz
;; Version 11 - munging for Gnome2, Gtk2::GladeXML
;;            - require-match since must choose one of the available links
;; Version 12 - propagate upper/lower case from gtk2-perl methods
;; Version 13 - avoid `with-auto-compression-mode' in Emacs 21 as it's buggy
;;              when byte compiled
;; Version 14 - perl SomeNewClass->signal_query etc lookup method name alone
;;            - fix infinite loop from symbol at point when at end of buffer,
;;              as reported by Neil Roberts
;; Version 15 - actually use gtk-lookup-assoc-string-ignore-case
;; Version 16 - restore buffer and window if browse-url throws an error
;; Version 17 - fix Gtk2::Gdk::GDK_FOO constants just Gtk2::GDK_FOO
;; Version 18 - special case for Glib::ParamSpec->param_spec
;; Version 19 - tie-ins for completing-help.el and icicles.el

;;; Code:

(require 'browse-url)

;;----------------------------------------------------------------------------
;; emacs21 bugs

(defmacro gtk-lookup--with-auto-compression (&rest body)
  "Evaluate BODY forms with `auto-compression-mode' enabled.
This is `with-auto-compression-mode', made available in XEmacs 21
and workarounds in Emacs 21 (its `with-auto-compression-mode'
doesn't work properly byte-compiled)."

  (if (eval-when-compile
        (and (fboundp 'with-auto-compression-mode)
             (not (and (featurep 'emacs) ;; but no good in emacs21
                       (= emacs-major-version 21)))))
      ;; emacs22 up
      `(with-auto-compression-mode ,@body)

    ;; emacs21 has `with-auto-compression-mode', but it's buggy when byte
    ;; compiled as it tries to call `jka-compr-installed-p' without loading
    ;; jka-compr.el
    ;;
    ;; xemacs21 doesn't have `with-auto-compression-mode'.  It also doesn't
    ;; have an `auto-compression-mode' variable (which emacs has) to get the
    ;; current state, hence the use of the `jka-compr-installed-p' here.
    ;;
    `(let ((gtk-lookup--with-auto-compression--old-state
            (and (fboundp 'jka-compr-installed-p) ;; if jka-compr loaded
                 (jka-compr-installed-p))))

       ;; turn on if not already on
       ;; xemacs21 has a toggle-auto-compression which takes a "no message"
       ;; arg, but not emacs21
       (if (not gtk-lookup--with-auto-compression--old-state)
           (auto-compression-mode 1))

       (unwind-protect
           (progn ,@body)
         ;; turn off again if it was off before
         (if (not gtk-lookup--with-auto-compression--old-state)
             (auto-compression-mode -1))))))

;;----------------------------------------------------------------------------
;; variables

;;;###autoload
(defgroup gtk-lookup nil
  "GTK/GNOME documentation lookup."
 :prefix "gtk-lookup-"
 :group 'languages ;; same as info-look
 :link  '(url-link
          :tag "gtk-look.el home page"
          "http://user42.tuxfamily.org/gtk-look/index.html"))

(defvar gtk-lookup-cache 'uninitialized
  "Cache of targets for `gtk-lookup-symbol'.
The current format is (NAME LINK DIR TYPE).
NAME is a function or datatype string.
DIR is a directory and LINK is a filename plus \"#anchor\".
TYPE is a symbol `function', `constant', etc.
Being an alist means this can be passed to `completing-read' and
friends.

DIR and LINK are separate to save a little memory as the (DIR
TYPE) tail can be shared among links in the one manual.

If `gtk-lookup-cache' is not yet initialized the value is the
symbol `uninitialized'.  `gtk-lookup-cache-init' should be used
to ensure it's initialized.")

;; must have gtk-lookup-reset ready for gtk-lookup-devhelp-indices initial
;; :set to use
(defun gtk-lookup-reset ()
  "Discard data cached for `gtk-lookup-symbol'.
This can be used to get newly installed documents recognised."
  (interactive)
  (setq gtk-lookup-cache 'uninitialized))

;; note this defcustom is after gtk-lookup-reset so the :set method here can
;; call gtk-lookup-reset immediately for setting the initial value
(defcustom gtk-lookup-devhelp-indices
  '(;; usual place (see /usr/share/doc/devhelp-common/README
    "/usr/share/gtk-doc/html/*/*.devhelp*"
    ;; possible locally installed stuff
    "/usr/local/share/gtk-doc/html/*/*.devhelp*")
  "List of devhelp index files containing GTK/GNOME documentation.
Shell wildcards like \"*.devhelp\" can be used, and gzip \".gz\"
compressed files are allowed (if you have gzip for
`auto-compression-mode').

Usually these files are under /usr/share/gtk-doc/html, and
possibly /usr/local/share/gtk-doc.

If you change this variable you should call `gtk-lookup-reset' to
clear previously cached data.  This is done automatically from
the `customize' interface."

 :set (lambda (sym val)
        (custom-set-default sym val)
        (gtk-lookup-reset))
 :type '(repeat string)
 :group 'gtk-lookup)

(defvar gtk-lookup-history nil
  "Symbols previously looked up with `gtk-lookup-symbol'.")


;;----------------------------------------------------------------------------
;; generic

(defun gtk-lookup-string-suffix-ci-p (suff str)
  "Return true if string SUFF is a suffix of STR, ignoring case."
  (and (>= (length str) (length suff))
       (if (eval-when-compile (fboundp 'compare-strings)) ;; not in xemacs21
           (eq t (compare-strings str (- (length str) (length suff)) nil
                                  suff nil nil
                                  t)) ;; ignore case
         (setq suff (upcase suff))
         (setq str (upcase str))
         (string-equal suff
                       (substring str (- (length str) (length suff)))))))
;; as long as case conversion tables are considered constant ...
(put 'gtk-lookup-string-suffix-ci-p 'pure t)

(defsubst gtk-lookup-assoc-string-ignore-case (key alist)
  "Lookup a string KEY in ALIST, case insensitively."
  (if (eval-when-compile (fboundp 'assoc-string))
      (assoc-string key alist t)
    (assoc-ignore-case key alist)))

(defun gtk-lookup-browse-url-other-window (url)
  "`browse-url' but in an \"other-window\" if it uses an Emacs window."

  ;; this convoluted code copes with various types of browser that
  ;; `browse-url' might invoke: perhaps an external program in its own X
  ;; window, perhaps something in an emacs buffer.  And when in a buffer it
  ;; might switch to an "other window" itself or just use the current
  ;; window; and perhaps the current buffer (and window) is already the
  ;; browser buffer
  ;;
  (interactive (browse-url-interactive-arg "URL: "))
  (let ((orig-win-conf (current-window-configuration))
        (orig-buffer   (current-buffer))
        (orig-window   (selected-window)))
    (unwind-protect
        (with-temp-buffer
          (let ((dummy-buf (current-buffer)))
            (switch-to-buffer-other-window (current-buffer))
            (let ((other-window (get-buffer-window dummy-buf)))
              (select-window other-window)
              (browse-url url)

              (cond ((and (eq dummy-buf (window-buffer other-window))
                          (eq orig-buffer (window-buffer orig-window)))
                     ;; browse-url didn't touch the buffers, it left the
                     ;; original and dummy current, so it's an external
                     ;; window system program; let window configs go back
                     ;; how they were
                     )

                    ((eq orig-buffer (window-buffer other-window))
                     ;; browse-url has changed dummy-buf to orig-buf in the
                     ;; other-window, which means we were in the browser
                     ;; buffer already and shouldn't have split with "other
                     ;; window"; so put window configs back how they were,
                     ;; but don't change point in the browser buffer as
                     ;; that's the new document position
                     (let ((point (window-point other-window)))
                       (set-window-configuration orig-win-conf)
                       (setq orig-win-conf nil)
                       (with-current-buffer orig-buffer
                         (goto-char point))))

                    (t
                     ;; browse-url has selected a buffer; but it might have
                     ;; done "other window" itself (eg. w3m-browse-url
                     ;; does); don't let two "other window" invocations
                     ;; leave our original buffer at the bottom and the
                     ;; browser at the top, instead force our orig-window
                     ;; back to orig-buffer, and let the other window we
                     ;; made show the browser buffer
                     (setq orig-win-conf nil)
                     (let ((browser-buffer (window-buffer other-window)))
                       (select-window other-window)
                       (switch-to-buffer browser-buffer)
                       (select-window orig-window)
                       (switch-to-buffer orig-buffer)))))))

      ;; restore windows for non-buffer browse-url or an error
      (if orig-win-conf
          (set-window-configuration orig-win-conf)))))


;;----------------------------------------------------------------------------
;; cached .devhelp file contents

(defun gtk-lookup-cache-init ()
  "Initialize `gtk-lookup-cache', if not already done.
The return is the `gtk-lookup-cache' list."
  (when (eq gtk-lookup-cache 'uninitialized)
    ;; build in `result' and only after that set gtk-lookup-cache, so as not
    ;; to leave a half built cache if killed (C-g) part-way through
    (let ((result nil)
          (case-fold-search nil)
          (filelist
           ;; `file-truename' here and `remove' below will eliminate
           ;; any duplicate filenames arising from symlinks or repeat
           ;; matches of wildcards in gtk-lookup-devhelp-indices
           (sort (mapcar 'file-truename
                         (apply 'append
                                (mapcar 'file-expand-wildcards
                                        gtk-lookup-devhelp-indices)))
                 'string<)))

      (unless filelist
        (if (eval-when-compile (fboundp 'display-warning)) ;; not in emacs21
            (display-warning 'gtk-look "No devhelp files found")
          (message "No devhelp files found")))

      ;; if there's a .devhelp2 then don't look at the old .devhelp
      (dolist (filename filelist)
        (when (string-match "\\.devhelp2\\(\\.gz\\)?\\'" filename)
          (let ((base (substring filename 0 (match-beginning 0))))
            (setq filelist (delete (concat base ".devhelp")
                                   filelist))
            (setq filelist (delete (concat base ".devhelp.gz")
                                   filelist)))))

      (gtk-lookup--with-auto-compression
       (with-temp-buffer
         (while filelist
           (let ((filename (pop filelist)))
             (message "Processing %s" filename)

             ;; skip duplicates turned up by the wildcard expansions
             (setq filelist (delete filename filelist))

             (let ((dir (file-name-directory filename))
                   tail-list)
               ;; In Emacs 21.3 jka-compr doesn't erase the buffer
               ;; properly under the "replace" argument to
               ;; insert-file-contents, so use erase-buffer instead.
               ;; (Fixed in Emacs 22.)
               (erase-buffer)
               (insert-file-contents filename)

               ;; "<function ...>" is devhelp 1 format
               ;;
               ;; "<keyword type=...>" is devhelp 2 format
               ;; the name field can be
               ;;     "enum foo"
               ;;     "foo()"
               ;;     "foo ()"
               ;;
               ;; The field values are xml entity-encoded like "&rquot;"
               ;; etc, but that doesn't (or shouldn't) occur in C symbols.
               ;;
               (while (re-search-forward "\
<\\(function\\|keyword type=\"\\([^\"]*\\)\"\\)\
 name=\"\\([^\"]*\\)\"\
 link=\"\\([^\"]+\\)\"\
" nil t)
                 (let ((type (match-string 2))
                       (name (match-string 3))
                       (link (match-string 4))
                       tail)

                   ;; lose trailing "()" or " ()" on name of functions, and
                   ;; set `type' for functions in devhelp 1 format
                   (when (string-match " ?()\\'" name)
                     (setq name (substring name 0 (match-beginning 0)))
                     (setq type "function"))

                   ;; lose leading "enum" or "union" from name, and set
                   ;; `type' for devhelp 1 format
                   (when (string-match "\\`\\(enum\\|struct\\|union\\) "
                                       name)
                     (setq name (substring name (match-end 0)))
                     (setq type (match-string 1 name)))

                   ;; devhelp 1 textual index entries usually have a space,
                   ;; exclude them
                   (if (and (not type)
                            (string-match " " name))
                       (setq type ""))

                   ;; devhelp 1 notice "struct" type from link
                   (if (and (not type)
                            (string-match "-struct\\'" link))
                       (setq type "struct"))

                   ;; devhelp 1 seen with some dubious empty name="",
                   ;; exclude them
                   (when (equal "" name)
                     (setq type ""))

                   ;; devhelp 2 exclude "property" and "signal" entries
                   ;; Could think about having them under qualified
                   ;; "GtkWidget::foo" or something.
                   (when (member type '("property" "signal"))
                     (setq type ""))

                   ;; devhelp 2 textual index entries have type="", exclude
                   ;; them
                   (when (not (equal type ""))

                     (if type (setq type (intern type)))
                     (setq tail (list dir type))
                     ;; share cons cells
                     (setq tail (or (car (assoc tail tail-list))
                                    (prog1 tail
                                      (push tail tail-list))))
                     (push (cons name (cons link tail))
                           result)))))))))

      (setq gtk-lookup-cache result)))
  gtk-lookup-cache)


;;-----------------------------------------------------------------------------
;; symbol thing-at-point

(defun gtk-lookup-symbol-method-candidates (method)
  "Return a list of Gtk symbols (strings) having METHOD as a suffix.
For example \"set_parent\" gives a list
\(\"gtk_widget_set_parent\" \"gnome_dialog_set_parent\" ...).

The method must match after a \"_\" separator, so for instance
\"parent\" doesn't give \"gtk_widget_unparent\"."

  (gtk-lookup-cache-init)
  (let ((ret (and (gtk-lookup-assoc-string-ignore-case method gtk-lookup-cache)
                  (list method))))    ;; whole name if exists
    (setq method (concat "_" method)) ;; and otherwise at _ boundary
    (dolist (elem gtk-lookup-cache ret)
      (let ((name (car elem)))
        (if (gtk-lookup-string-suffix-ci-p method name)
            (setq ret (cons name ret)))))))

(defun gtk-lookup-canonicalize-symbol (str)
  "Return canonicalized Gtk function name etc from string STR.
Various transformations are applied to turn Gtk2-Perl, Guile-Gtk
and Guile-Gnome into C names.  For example Scheme func
\"gdk-keyval-to-lower\" becomes \"gdk_keyval_to_lower\", or Perl
\"Gtk2::TreeStore->new\" becomes \"Gtk_Tree_Store_new\".

Not much attention is paid to upper/lower case in the transformed
return.  It's basically left like the input, anticipating a
case-insensitive lookup by `completing-read' in
`gtk-lookup-symbol-interactive-arg'."

  (when str
    (let ((case-fold-search nil))
      ;; note xemacs21 replace-match doesn't take a "subexp" arg when
      ;; replacing in a string (only in a buffer)

      ;; gtk2-perl "Glib::G_PRIORITY_LOW" -> "G_PRIORITY_LOW", to avoid a
      ;; doubling to "g_G_..."
      (if (string-match "\\`Glib::\\(G_\\)" str)
          (setq str (replace-match "\\1" t nil str)))

      ;; gtk2-perl "Gtk2::GTK_PRIORITY_RESIZE" -> "GTK_PRIORITY_RESIZE", and
      ;; "Gtk2::GDK_PRIORITY_EVENTS" -> "GDK_PRIORITY_EVENTS", to avoid a
      ;; doubling to "gtk_GTK_..." or "gtk_GDK_..."
      (if (string-match "\\`Gtk2::\\(G[DT]K_\\)" str)
          (setq str (replace-match "\\1" t nil str)))

      ;; gtk2-perl "Glib::ParamSpec->param_spec" -> "g_param_spec_param", an
      ;; inconsistency in the method naming
      (if (equal "Glib::ParamSpec->param_spec" str)
          (setq str "g_param_spec_param"))

      ;; gtk2-perl "Glib" -> "G"
      (if (string-match "\\`\\(Glib\\)\\(::\\|->\\)" str)
          (setq str (replace-match "G\\2" t nil str)))
      ;; gtk2-perl "Gtk2::Gdk", "Gtk2::Glade", "Gtk2::Pango" lose "Gtk2::" part
      ;; (Gtk2::Pango has been renamed to just Pango, but keep it for
      ;; compatibility)
      (if (string-match "\\`\\(Gtk2::\\)\\(Gdk\\|Glade\\|Pango\\)" str)
          (setq str (replace-match "\\2" t nil str)))
      ;; gtk2-perl "Gtk2" -> "Gtk", "Gnome2" -> "Gnome", losing the "2"
      (if (string-match "\\`\\(Gtk\\|Gnome\\)2\\(::\\|->\\)" str)
          (setq str (replace-match "\\1\\2" t nil str)))

      ;; guile-gnome classes "<gtype-instance>" -> "gtypeInstance"
      ;; base types as per gtype-name->scheme-name-alist in utils.scm
      (when (string-match "\\`<\\(.*\\)>\\'" str)
        (setq str (match-string 1 str))
        (while (string-match "\\(-\\)\\(.\\)" str)
          (setq str (replace-match (upcase (match-string 2 str)) t t str))))
      ;; guile-gnome "gtype:gtype" -> "G_TYPE_gtype"
      ;; and "gtype:gboolean" -> "G_TYPE_boolean" by stripping the "g" if
      ;; there's no match with it, but a match without
      (when (string-match "\\`\\(gtype:\\)g?" str)
        (let ((alt (replace-match "G_TYPE_" t t str)))
          (setq str (replace-match "G_TYPE_g" t t str))
          (gtk-lookup-cache-init)
          (and (not (gtk-lookup-assoc-string-ignore-case str gtk-lookup-cache))
               (gtk-lookup-assoc-string-ignore-case alt gtk-lookup-cache)
               (setq str alt))))

      (if (string-match "[_-]" str)
          ;; function or constant
          (progn
            ;; gtk2-perl camel case class like "TreeStore" -> "Tree_Store"
            (while (string-match "\\([a-z]\\)\\([A-Z]\\)" str)
              (setq str (replace-match "\\1_\\2" t nil str)))

            ;; gtk2-perl camel case like "UIManager" -> "UI_Manager"
            ;; but only two or more like UI, a single VScrollbar unchanged
            (while (string-match "\\([A-Z]\\{2,\\}\\)\\([A-Z][a-z]\\)" str)
              (setq str (replace-match "\\1_\\2" t nil str)))

            ;; gtk2-perl "Foo->bar->quux" becomes quux, since can't know
            ;; what class the bar() method returns
            (when (string-match "->.*->" str)
              (setq str (substring str (match-end 0))))

            ;; gtk2-perl "::" separator becomes "_"
            (while (string-match "::" str)
              (setq str (replace-match "_" t t str)))

            ;; gtk2-perl "Foo->bar" becomes "Foo_bar" if that's known, or
            ;; "bar" if not.  Checking Foo is a known class allows for
            ;; method calls on subclasses, eg.
            ;; "MyNewClass->signal_add_emission_hook".
            ;;
            ;; The upper/lower case of the "Foo" PRE part is adjusted to
            ;; follow the "bar" POST part.  This means Gtk2->check_version
            ;; gives the function gtk_check_version() whereas
            ;; Gtk2->CHECK_VERSION gives the macro GTK_CHECK_VERSION().
            ;; If there's only one such function then this doesn't matter,
            ;; the insensitive assoc in `gtk-lookup-symbol' will find it.
            ;; But if there's both an upper and a lower like check_version
            ;; then it should get the right one.
            ;;
            (when (string-match "->" str)
              (let ((pre  (substring str 0 (match-beginning 0)))
                    (post (substring str (match-end 0))))
                (setq str (concat (if (string-match "\\`[a-z]" post)
                                      (downcase pre)
                                    (upcase pre))
                                  "_" post))

                (unless (gtk-lookup-assoc-string-ignore-case
                         str (gtk-lookup-cache-init))
                  (setq str post))))

            ;; lisp "-" becomes "_"
            (while (string-match "-" str)
              (setq str (replace-match "_" t t str))))

        ;; one word class name

        ;; gtk2-perl "::" separators eg. "Gtk::Object" -> "GtkObject",
        ;; including subclassing forms like "Gtk::Label::" -> "GtkLabel",
        (while (string-match "::" str)
          (setq str (replace-match "" t t str))))))

  str)

(defun gtk-lookup-symbol-bounds-of-thing-at-point ()
  "Find the bounds of a `gtk-lookup-symbol' symbol at point.
The return is a pair (BEG . END) of buffer positions, or nil if
point is not at or within a symbol."

  ;; For perl style "Gtk2::Foo->bar" demand the left side start with a
  ;; capital letter like "Gtk2::Label->new", so as to distinguish it from a
  ;; method call like "$label->set_text".  For the latter the return is just
  ;; the "set_text" part (when point is with that "set_text").
  ;;
  ;; The desired match is the one earliest in the buffer which covers point.
  ;; `re-search-backwards' is no good for that, as it stops at the first
  ;; match, not the earliest possible.  `thing-at-point-looking-at' is
  ;; better, but the optional "(...)?" perl class part ends up with only a
  ;; partial match (like only the "Store" part of "TreeStore->"), not the
  ;; biggest surrounding point.  So the strategy is to look forwards from the
  ;; beginning of the line for the first which covers point.
  ;;
  (save-excursion
    (let ((case-fold-search nil)
          (orig-point (point))
          (re "\\([A-Z][a-zA-Z0-9_:]*[a-zA-Z0-9_]->\\)?[a-zA-Z_][a-zA-Z0-9_:-]*[a-zA-Z0-9]\\|<[a-zA-Z0-9_-]+>"))
      (beginning-of-line)
      (let (found)
        (while (and (setq found (re-search-forward re nil t))
                    (< (match-end 0) orig-point)))
        (and found
             (<= (match-beginning 0) orig-point)
             (cons (match-beginning 0) (match-end 0)))))))

(put 'gtk-lookup-symbol 'bounds-of-thing-at-point
     'gtk-lookup-symbol-bounds-of-thing-at-point)


;;-----------------------------------------------------------------------------
;; completing-help.el tie-in
;;
;; The default `completing-help-alist-get-info' displays the url link from
;; `gtk-lookup-cache', which is not particularly interesting.

(defvar completing-help-groups) ;; quieten the byte compiler

(defvar gtk-lookup-completing-help-group
  '(:predicate gtk-lookup-completing-help-p
    :get       gtk-lookup-completing-help-get
    :info-head " - "
    :info-tail ""))

(defun gtk-lookup-completing-help-p ()
  "Return non-nil when completing from `gtk-lookup-cache'."
  (and (not (eq gtk-lookup-cache 'uninitialized))
       (eq minibuffer-completion-table gtk-lookup-cache)))

(defun gtk-lookup-completing-help-get (str)
  "Return a help string for Gtk symbol STR.
Currently this is like \"function, in GtkWidget.html\".
The .devhelp indexes don't have a short description for the symbols."
  (let ((elem (assoc str (gtk-lookup-cache-init))))
    (when elem
      (let ((link (nth 1 elem))
            (type (nth 3 elem)))
        (string-match "#\\|\\'" link) ;; strip anchor part
        (setq link (substring link 0 (match-beginning 0)))
        (if type
            (format "%s, in %s" type link) ;; have type in .devhelp2
          (format "in %s" link))))))       ;; no type in .devhelp

(eval-after-load "completing-help"
  '(if (boundp 'gtk-lookup-completing-help-group) ;; in case we're unloaded
       (add-to-list 'completing-help-groups
                    'gtk-lookup-completing-help-group)))

;;----------------------------------------------------------------------------
;; icicles.el tie-in

(defvar icicle-candidate-help-fn) ;; quieten the byte compiler

(defun gtk-lookup-icicle-help (str)
  "Display help for STR in `icicles-mode'.
Page descriptions are shown with `icicle-msg-maybe-in-minibuffer'
because they're just a single line."
  (let ((desc (gtk-lookup-completing-help-get str)))
    (if desc
        (icicle-msg-maybe-in-minibuffer "%s" desc))))


;;-----------------------------------------------------------------------------
;; read and lookup

(defvar gtk-lookup-initial-completion-list nil
  "Initial completions to display for `gtk-lookup-symbol-interactive-arg'.
This is let-bound by `gtk-lookup-symbol-interactive-arg' and is
nil at other times.")

(defun gtk-lookup-display-initial-completion-list ()
  "Display initial method completions for `gtk-lookup-symbol'."
  (if (>= (length gtk-lookup-initial-completion-list) 2)
      (with-output-to-temp-buffer "*Completions*"
        (display-completion-list gtk-lookup-initial-completion-list)))
  (setq gtk-lookup-initial-completion-list nil))

(add-hook 'minibuffer-setup-hook
          'gtk-lookup-display-initial-completion-list)

(defun gtk-lookup-symbol-interactive-arg ()
  "Symbol argument read for interactive `gtk-lookup-symbol'.
Return a one-element list (\"symbol\") which is the user-selected
symbol name string."
  (let* ((completion-ignore-case t)
	 (enable-recursive-minibuffers t)
         (icicle-candidate-help-fn 'gtk-lookup-icicle-help)
	 (default (gtk-lookup-canonicalize-symbol
                   (thing-at-point 'gtk-lookup-symbol)))
         (gtk-lookup-initial-completion-list
          (and default
               (gtk-lookup-symbol-method-candidates default))))
    (if (= 1 (length gtk-lookup-initial-completion-list))
        ;; one method match, offer full name as the default
        (setq default (car gtk-lookup-initial-completion-list)))

    (let ((symbol (gtk-lookup-canonicalize-symbol
                   (completing-read
                    (if default
                        (format "Describe symbol (default %s): " default)
                      "Describe symbol: ")
                    (gtk-lookup-cache-init)
                    nil  ;; predicate
                    t    ;; require-match
                    nil  ;; initial-input
                    'gtk-lookup-history
                    default))))
      (list (or symbol default "")))))

;;;###autoload
(defun gtk-lookup-symbol (symbol)
  "Lookup Gtk/Gnome documentation for SYMBOL.
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
URL `http://user42.tuxfamily.org/gtk-look/index.html'"

  (interactive (gtk-lookup-symbol-interactive-arg))
  (gtk-lookup-cache-init)
  (let ((entry (or (assoc symbol gtk-lookup-cache) ;; exact match preferred
                   (gtk-lookup-assoc-string-ignore-case
                    symbol gtk-lookup-cache))))    ;; otherwise case-fold
    (or entry
        (error "Unknown symbol %s" symbol))
    (gtk-lookup-browse-url-other-window (concat "file://"
                                                (nth 2 entry)     ;; dir
                                                (nth 1 entry))))) ;; link


;;-----------------------------------------------------------------------------

(defun gtk-look-unload-function ()
  "Undo gtk-look.el stuff."

  (put 'gtk-lookup-symbol 'bounds-of-thing-at-point nil)

  ;; mustn't leave an unbound var in `completing-help-groups' or it will
  ;; error out (as of completing-help.el 3.13)
  (if (boundp 'completing-help-groups)
      (setq completing-help-groups
            (remq 'gtk-lookup-completing-help-group completing-help-groups)))

  ;; and do normal unload-feature actions too,
  ;; including `minibuffer-setup-hook' addition removed automatically
  nil)
 
(provide 'gtk-look)

;;; gtk-look.el ends here
