;;; ws-trim-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "ws-trim" "ws-trim.el" (21401 21791 339057
;;;;;;  593000))
;;; Generated autoloads from ws-trim.el

(defvar ws-trim-method-hook '(ws-trim-leading ws-trim-trailing) "\
*The kind of trimming done by the WS Trim mode and functions.
A single or a list of functions which are run on each line that's
getting trimmed.  Supplied trim functions:

`ws-trim-trailing'        Delete trailing whitespace.
`ws-trim-leading-spaces'  Replace unnecessary leading spaces with tabs.
`ws-trim-leading-tabs'    Replace leading tabs with spaces.
`ws-trim-leading'         Replace leading tabs or spaces according to
                          `indent-tabs-mode'.  If it's nil, leading
                          tabs are replaced with spaces, otherwise
                          it's the other way around.
`ws-trim-tabs'            Replace all tabs with spaces.

This is a perfectly normal hook run by `run-hooks' and custom
functions can of course be used.  There's no inherent restriction to
just whitespace trimming either, for that matter.  Each function
should modify the current line and leave point somewhere on it.")

(autoload 'ws-trim-line "ws-trim" "\
Trim whitespace on the current line.
Do this according to the hook `ws-trim-method-hook'.  With a prefix
argument, ask for the trim method to use instead.

\(fn ARG)" t nil)

(autoload 'ws-trim-region "ws-trim" "\
Trim whitespace on each line in the region.
Do this according to the hook `ws-trim-method-hook'.  With a prefix
argument, ask for the trim method to use instead.

\(fn ARG)" t nil)

(autoload 'ws-trim-buffer "ws-trim" "\
Trim whitespace on each line in the buffer.
Do this according to the hook `ws-trim-method-hook'.  With a prefix
argument, ask for the trim method to use instead.

\(fn ARG)" t nil)

(defvar ws-trim-mode nil "\
If non-nil, WS Trim mode is active.
This mode automatically trims whitespace on text lines.  The kind of
trimming is specified by the hook `ws-trim-method-hook'.  You can
either trim every line in the buffer or just the lines you edit
manually, see the variable `ws-trim-level' for details.  This mode
runs the hook `ws-trim-mode-hook' when activated.

Please note that there are other common functions, e.g. `indent-to',
`newline-and-indent' (often bound to LFD or RET), `fill-paragraph',
and the variable `indent-tabs-mode', that also trims whitespace in
various circumstances.  They are entirely independent of this mode.

To automatically enable WS Trim mode in any major mode, put
`turn-on-ws-trim' in the major mode's hook, e.g. in your .emacs:

  (add-hook 'emacs-lisp-mode-hook 'turn-on-ws-trim)

You can also activate WS Trim mode automagically in all modes where
it's likely to be useful by putting the following in .emacs:

  (global-ws-trim-mode t)

Exactly when WS Trim is activated are by default controlled by a
heuristic, see the function `ws-trim-mode-heuristic' for details.  You
can get more control over the process through the variable
`global-ws-trim-modes'.

This variable automatically becomes buffer local when modified.  It
should not be set directly; use the commands `ws-trim-mode' or
`turn-on-ws-trim' instead.")

(defvar ws-trim-level 0 "\
*How thorough automatic whitespace trimming should be in WS Trim mode.
If 3 or greater, all lines in the buffer are kept trimmed at all
times (if the buffer is modifiable).
If 2, all lines in the buffer are trimmed when the buffer is modified
for the first time.
If 1, only modified lines are trimmed.
If 0, only single modified lines are trimmed, i.e. operations that
modify more than one line don't cause any trimming (newline is an
exception).

The current line is never trimmed on any level, unless the buffer is
about to be written.  In that case the current line is treated as any
other line.

The default level is 0, which is very restrictive.  This is
particularly useful when you edit files which are compared with diff
\(e.g. for patches), because parts that you don't change manually are
kept unchanged.  You can also do block operations over several lines
without risking strange side effects (e.g. paste patches into mails).

This variable automatically becomes buffer local when changed.  Use
the function `set-default' to set the value it defaults to in all new
buffers.  If you want even more control it's best to put a suitable
function onto `ws-trim-mode-hook'.  Changes of `ws-trim-level' might
not take effect immediately; it's best set when the mode is
initialized.")

(defvar ws-trim-mode-line-string " Trim" "\
*Modeline string for WS Trim mode.
Set to nil to remove the modeline indicator for ws-trim.")

(defvar ws-trim-mode-hook nil "\
A normal hook which is run when WS Trim mode is turned on.
This hook is run by `run-hooks' and can therefore be buffer local.

Some care might be necessary when putting functions on this hook due
to the somewhat strange circumstances under which it's run.
Specifically, anything put here might indirectly be run from
`post-command-hook' or `find-file-hooks'.  Don't worry about it if you
just want to do something simple, e.g. setting some variables.")

(autoload 'turn-on-ws-trim "ws-trim" "\
Unconditionally turn on WS Trim mode.
See the variable `ws-trim-mode' for further info on this mode.

\(fn)" t nil)

(autoload 'ws-trim-mode "ws-trim" "\
Toggle WS Trim mode, which automatically trims whitespace on lines.
A positive prefix argument turns the mode on, any other prefix turns
it off.

See the variable docstring for details about this mode.

\(fn &optional ARG)" t nil)

(defvar global-ws-trim-mode nil "\
If non-nil, automagically turn on WS Trim mode in many major modes.
How it's done is controlled by the variable `ws-trim-global-modes'.

This variable should not be changed directly; use the command
`global-ws-trim-mode' instead.")

(defvar ws-trim-global-modes 'guess "\
*Controls which major modes should have WS Trim mode turned on.
Global WS Trim mode must first be activated, which is done by the
command `global-ws-trim-mode'.

If nil, no modes turn on WS Trim mode.
If t, all modes turn on WS Trim mode.
If `guess', then a heuristic is used to determine whether WS Trim mode
should be activated in the mode in question.  See
`ws-trim-mode-heuristic' for details.
If a list, then all modes whose `major-mode' symbol names matches some
entry in it turn on WS Trim mode.
If a list begins with `not', all modes but the ones mentioned turn on
WS Trim mode.
If a list begins with `guess', then the remaining elements must in
turn be lists as above.  All modes not specified in any of these lists
will use the heuristic.  E.g:

  (setq ws-trim-global-modes '(guess (Info-mode) (not c-mode c++-mode)))

turns on WS Trim in Info-mode (God knows why), off in C mode and
C++ mode, and uses the heuristic for all other modes.")

(autoload 'global-ws-trim-mode "ws-trim" "\
Toggle Global WS Trim mode.
A positive prefix argument turns the mode on, any other prefix turns
it off.

When this mode is active, WS Trim mode is automagically turned on or
off in buffers depending on their major modes.  The behavior is
controlled by the `ws-trim-global-modes' variable.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; ws-trim-autoloads.el ends here
