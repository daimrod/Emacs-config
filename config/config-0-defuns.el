;; -*- lexical-binding: t; -*-
;; config-0-defuns.el
;; Copyright (C) 2011, 2012 Grégoire Jadi

;; Author: Grégoire Jadi <gregoire.jadi@gmail.com>

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(defcustom org-bib-notes-file (expand-file-name "~/org/bib.org")
  ""
  :group 'org
  :type 'file)

(defcustom org-bibtex-file (expand-file-name "~/org/.bib.bib")
  ""
  :group 'org
  :type 'file)

(defun sudo-edit (&optional arg)
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:localhost:" (read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:localhost:" buffer-file-name))))

(defun sbrk-paste ()
  "paste a chunk of code to pastebin.sbrk.org"
  (interactive)
  (let* ((region-active-p (region-active-p))
         (start (if region-active-p (region-beginning) (buffer-end -1)))
         (end (if region-active-p (region-end) (buffer-end 1))))
    (http-post-simple "http://pastebin.sbrk.org/add"
                      `((author ,(getenv "USER"))
                        (code ,(buffer-substring start end))))))

(defun zap-to-char- (arg char)
  "Just like zap-to-char- but do not chop the last CHAR."
  (interactive "p\ncZap to char-: ")
  ;; Avoid "obsolete warnings for translation-table-for-input.
  (with-no-warnings
    (if (char-table-p translation-table-for-input)
        (setq char (or (aref translation-table-for-input char) char))))
  (save-excursion
    (kill-region (point)
                 (progn
                   (search-forward (char-to-string char) nil nil arg)
                   (if (>= arg 0)
                       (- (point) 1)
                     (+ (point) 1))))))

(defun move-to-window-line-top ()
  (interactive)
  (move-to-window-line 0))

(defun move-to-window-line-bottom ()
  (interactive)
  (move-to-window-line -1))

(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))

(defun dmd/small-scroll-up-command (&optional arg)
  (interactive "^P")
  (let ((fun-scroll-up (if (fboundp 'scroll-up-command)
                           'scroll-up-command
                         'scroll-up)))
    (if arg
        (funcall fun-scroll-up arg)
      (funcall fun-scroll-up 5))))

(defun dmd/small-scroll-down-command (&optional arg)
  (interactive "^P")
  (let ((fun-scroll-down (if (fboundp 'scroll-down-command)
                             'scroll-down-command
                           'scroll-down)))
    (if arg
        (funcall fun-scroll-down arg)
      (funcall fun-scroll-down 5))))

(defun dmd/show-big-text (text &optional size font)
  (interactive "sText to show: ")
  (let ((size (number-to-string
               (if (null size)
                   (window-width)
                 size)))
        (font (if (null font)
                  "doh"
                font)))
    (shell-command (format "figlet -w %s -f %s %s"
                           size font text))))

(defun dmd/client-process ()
  "Returns the process associated with the current emacsclient"
  (when (boundp 'server-clients)
    (loop for process in server-clients
          when (eq (selected-frame)
                   (process-get process 'frame))
          return process)))

(defvar dmd/dead-clients nil
  "List of dead clients that should be destroyed.")

(defun dmd/quit-or-hide (rly?)
  "If it this is an instance of a running Emacs daemon, then
if it's the last frame, hide it, otherwise delete it.

If not, use the classic save-buffers-and-kill-emacs function."
  (interactive "P")
  (if (and (boundp 'server-process) (not (null server-process)) (null rly?))
	  (progn
        (condition-case nil
            (make-frame-invisible nil t)
          (error (delete-frame)))
        (add-to-list 'dmd/dead-clients (dmd/client-process)))
    (save-buffers-kill-emacs)))

(defun dmd/delete-zombie-clients (frame)
  "Delete zombie clients, that is, emacsclient that are finished
but still present in the background."
  (when (fboundp 'server-delete-client)
    (dolist (process dmd/dead-clients)
      (server-delete-client process))))

(add-hook 'after-make-frame-functions #'dmd/delete-zombie-clients)

(defun dmd/autocompile ()
  "Byte compile an elisp and reload it."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when (eq major-mode 'emacs-lisp-mode)
      (byte-compile-file filename)
      (load (file-name-sans-extension filename)))))

(defvar *evince-extensions* nil
  "List of extentions supported by evince.")
(setf *evince-extensions* '("pdf" "ps" "dvi"))

(defvar *evince-location* "/usr/bin/evince"
  "Where is evince?")

(defun dmd/evince (filename)
  "Open the given FILENAME with evince in a background.

`read-file-name' is used to find the files. Extensions can be
added to `*evince-extensions*'."
  (interactive
   (let* ((exts (copy-list *evince-extensions*))
          (ext (do* ((ret (concat "\\(" (pop exts)) (concat ret "\\|" ext))
                     (ext (pop exts) (pop exts)))
                   ((null exts) (concat ret "\\)")))))
     (list (read-file-name
            "File: " nil nil
            nil
            (ffap-file-at-point)
            '(lambda (filename)
               (and (file-exists-p filename)
                    (or
                     (string= "~" filename)
                     (file-directory-p filename)
                     (string-match (format "^.*\.%s$" ext) filename))))))))
  (shell-command (format "evince \"%s\" & disown" (expand-file-name filename)))
  (message "%s" filename))

(defvar comint-buffer-minimum-size 0)
(defun dmd/comint-truncate-buffer (&optional n)
  "Does what comint-truncate-buffer should do. That is, truncate
the buffer to keep N lines.

If N is not set, use `comint-buffer-minimum-size'."
  (interactive "P")
  (let ((comint-buffer-maximum-size
         (or n
             (- (line-number-at-pos (point-max)) (line-number-at-pos)))))
    (comint-truncate-buffer)))

(defun toggle-fullscreen ()
  "Toggle full screen on X11"
  (interactive)
  (when (eq window-system 'x)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth))))

(defcustom terminal-emulator "xterm"
  "*A terminal emulator to use."
  :group 'external)

(defcustom terminal-emulator-parameters nil
  "*A terminal emulator to use."
  :group 'external
  :type '(repeat string))

(defun dmd-terminal-emulator ()
  "Open a terminal emulator using `terminal-emulator'."
  (interactive)
  (let ((process-environment
         (remove-if (lambda (env)
                      (string-match-p "^TMUX=" env))
                    process-environment)))
    (apply
     #'start-process
     "Terminal Emulator"
     nil
     (etypecase terminal-emulator
       (string terminal-emulator)
       (function (funcall terminal-emulator))
       (symbol (symbol-value terminal-emulator)))
     terminal-emulator-parameters)))

(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line
of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun insert-notebook-note ()
  (interactive)
  (insert (org-make-link-string
           (format-time-string "%A %d %B %Y (%H:%M)")
           (format-time-string "@%H%M"))))

(defun dmd/window-width (&optional window)
  "Like `window-width' except that it takes into account text
scaling."
  (setq window (or window (selected-window)))
  (with-current-buffer (window-buffer window)
    (let ((amount (if (boundp 'text-scale-mode-amount)
                      (- text-scale-mode-amount)
                    0))
          (step (if (boundp 'text-scale-mode-step)
                    text-scale-mode-step
                  1)))
      (truncate
       (* (window-width window)
          (expt step amount))))))

(defun dmd/kill-buffer (&optional kill-process)
  "Kill a buffer or its process with a prefix.
The buffer name is selected interactively by typing a substring.
For details of keybindings, see `ido-switch-buffer'."
  (interactive "P")
  (if kill-process
      (let ((proc (get-buffer-process
                   (ido-buffer-internal
                    nil nil "Kill buffer process: "
                    (buffer-name (current-buffer))
                    nil 'ignore))))
        (delete-process proc)
        (message "Process `%S' killed" proc))
    (ido-buffer-internal 'kill 'kill-buffer "Kill buffer: " (buffer-name (current-buffer)) nil 'ignore)))

(defun dmd/switch-git<->https ()
  "Switch the current remote from git to https or the other way
around depending on the current value.

It uses magit internal."
  (interactive)
  (let* ((remote (magit-get-current-remote))
         (remote-url (magit-get "remote" remote "url")))
    (magit-set
     (cond ((string-match "^git@" remote-url)
            (format
             "https://%s"
             (substitute ?/ ?: (substring remote-url 4) :count 1)))
           ((string-match "^https://" remote-url)
            (format
             "git@%s"
             (substitute ?: ?/ (substring remote-url 8) :count 1)))
           (t (error "Unknown remote URL format `%s'" remote-url)))
     "remote" remote "url")))

(defun dmd/rename-buffer (&optional unique)
  (interactive "P")
  (let ((new-name (read-from-minibuffer "Rename buffer: " (buffer-name))))
    (rename-buffer new-name unique)))

(defun dmd/text-properties (&optional start end)
  "Add properties in the current active region."
  (interactive "r")
  (unless (use-region-p)
    (error "No active region"))
  (add-face-text-property start end (read (read-from-minibuffer "Property: "))))

(defun youtube-dl (url)
  (interactive "MURL: ")
  (let* ((buffer (get-buffer-create (format "*Youtube DL %s*" url)))
         (default-directory (if (string-match-p "~/Music" default-directory)
                                default-directory
                                "~/Music/Random/"))
         (proc (start-process "youtube-dl"
                              buffer
                              "/bin/bash"
                              "-c"
                              (format "source ~/.virtualenv/youtube-dl/bin/activate && youtube-dl --no-part --extract-audio --audio-format mp3 --audio-quality 128K %s" url))))
    (set-process-sentinel proc (lambda (proc event)
                                 (if (string= event "finished\n")
                                     (kill-buffer buffer)
                                   (user-error "A problem occured in %s" (buffer-name buffer)))))))

(defun dmd/dired-exec-lisp (function)
  (interactive "XExec: ")
  (funcall function (dired-get-marked-files)))

(defun dmd/doc-view-info ()
  "Open a buffer with the current doc's info as text."
  (interactive)
  (let ((buffer (concat "*Info of "
                        (file-name-nondirectory buffer-file-name)
                        "*")))
    (if (get-buffer buffer)
        (kill-buffer buffer))
    (call-process "/usr/bin/pdfinfo" nil buffer nil buffer-file-name)
    (switch-to-buffer buffer)
    (read-only-mode 1)
    (goto-char (point-min))))

(defun dmd/doc-view-external ()
  "Open the current document using an external program."
  (interactive)
  (start-process "doc-view external" (generate-new-buffer " *DocView External Viewer*")
                 "/usr/bin/evince" buffer-file-name))

(defun dmd/open-pdf (file)
  (interactive "fFile: ")
  (list file))

(el-dispatcher-make 'dmd/open-pdf
                    '(("doc-view" . find-file)
                      ("mupdf" . (lambda (file)
                                 (start-process (format "mupdf %S" file)
                                                nil
                                                "mupdf" file)))
                      ("evince" . (lambda (file)
                                  (start-process (format "evince %S" file)
                                                 nil
                                                 "evince" file)))))

(defadvice emms-start (after emms-show-track (&rest args) activate)
  (emms-show))

(defun dmd/read-lines (filename &optional visit beg end replace)
  "Return a list of lines in FILENAME."
  (with-temp-buffer
    (insert-file-contents filename visit beg end replace)
    (split-string (buffer-string) "\n" t)))

(defsubst /. (dividend &rest divisors)
  "Like `/' but uses floating number by coercing the DIVIDEND to
float."
  (apply #'/ (coerce dividend 'float) divisors))

(defun dmd-text-mode-setup ()
  (interactive)
  (visual-line-mode 1)
  (adaptive-wrap-prefix-mode 1))

(defun dmd-org-mode-reftex-setup ()
  (interactive)
  (when (and (derived-mode-p 'org-mode)
             (buffer-file-name))
    (setq-local tex-main-file (buffer-file-name))
    (reftex-set-cite-format "[[bib:%l][%2a (%y)]]")
    (setq-local reftex-cite-punctuation '(", " " and " " et al."))
    (reftex-parse-all)))

(defun dmd-html-to-org (&optional prefix)
  (interactive "P")
  (let ((buffer (if prefix
                    (read-buffer "Buffer: " (current-buffer) 'require-match)
                  (current-buffer))))
    (with-current-buffer buffer
      (dmd--format-to-org (buffer-substring (point-min) (point-max)) nil 2))
    (switch-to-buffer pandoc--output-buffer)
    (org-mode)
    (goto-char (point-min))))

(defun dmd--format-to-org (string &optional mode base-header-level)
  (setq base-header-level (or base-header-level 1))
  (setq mode (or mode 'html-mode))
  (ignore-errors (kill-buffer (get-buffer-create "*Pandoc output*")))
  (ignore-errors (kill-buffer (get-buffer-create "*Pandoc input*")))
  (with-current-buffer (get-buffer-create "*Pandoc input*")
    (setq pandoc--output-buffer (get-buffer-create "*Pandoc output*"))
    (goto-char (point-min))
    (insert string)
    (funcall mode)
    (turn-on-pandoc)
    (pandoc--set 'write "org")
    (pandoc--set 'columns 9001)
    (pandoc--set 'base-header-level base-header-level)
    (pandoc--set 'mathml t)
    (pandoc--set 'latexmathml t)
    (pandoc--call-external (current-buffer) nil)
    (with-current-buffer pandoc--output-buffer
      (buffer-substring (point-min) (point-max)))))

(defun dmd--org-feed-parse-html-entry (entry)
  (dmd--format-to-org (plist-get entry :item-full-text)))

(defun dmd--sanitize-org-regexp-matcher (regexp)
  (replace-regexp-in-string "[|/!]" "." regexp))

(defun dmd-bibtex-open (&optional prefix)
  (interactive "P")
  (if prefix
      (dmd--bibtex-open-file)
    (dmd--bibtex-jump-to-org-entry)))

(defun dmd--bibtex-open-file ()
  (let ((raw (cdr (assoc-string "file"
                                (save-excursion
                                  (bibtex-beginning-of-entry)
                                  (bibtex-parse-entry))
                                t))))
    (string-match "{:\\(.*\\):PDF}" raw)
    (org-open-file (match-string 1 raw))))

(defun dmd--bibtex-jump-to-org-entry ()
  (let ((bib-buffer (find-file-noselect org-bib-notes-file))
        (label (cdr (assoc-string "=key="
                                  (save-excursion
                                    (bibtex-beginning-of-entry)
                                    (bibtex-parse-entry))
                                  t)))
        position)
    (setq todo-only nil)                ; required by org-make-tags-matcher
    (if (not label)
        (user-error "Could not find any bibtex entry")
      (with-current-buffer bib-buffer
        (setq position (first
                        (org-scan-tags '(lambda () (point))
                                       (cdr (org-make-tags-matcher
                                             (format "BIBTEX={%s}"
                                                     (dmd--sanitize-org-regexp-matcher label))))
                                       todo-only))))
      (if (not position)
          (user-error "Could not find any matching entry in %s for %s" bib-buffer label)
        (switch-to-buffer bib-buffer)
        (goto-char position)))))

(defun dmd-scanimage (filename)
  (interactive "FOutputFile: ")
  (async-shell-command (format "scanimage -p -vvv --format=tiff > %s.tiff" filename)))

(advice-add 'org-contacts-remove-ignored-property-values :filter-return
            (lambda (list)
              (cl-remove-duplicates (nreverse list) :test #'string-match-p))
            '((name . remove-duplicates)))

(defun dmd--org-link-to-named-block ()
  "Create an org-link to the named block at point.

Blocks are named with #+NAME."
  (when (eq major-mode 'org-mode)
    (let* ((el (org-element-at-point))
           (name (org-element-property :name el)))
      (when name
        (org-store-link-props
         :link name)))))

(defun dmd--org-latex-link (link desc info)
  "Convert a bib link to a citation (e.g. bib:foo93 -> \cite{foo93})."
  (let* ((type (org-element-property :type link))
         (path (org-element-property :path link)))
    (when (or (string= type "bib")
              (and (string= type "file")
                   (file-equal-p path org-bibtex-file)))
      (format "\\cite{%s}" (org-element-property :search-option link)))))

(advice-add 'org-latex-link :around (lambda (oldfun &rest args)
                                      "Run `dmd--org-latex-link'"
                                      (or (apply 'dmd--org-latex-link args)
                                          (apply oldfun args)))
            '((name . dmd--org-latex-link)))

(defun dmd--get-title (url)
  (let* ((buffer (url-retrieve-synchronously url))
         (title (with-current-buffer buffer
                  (goto-char (point-min))
                  (search-forward "<title>")
                  (buffer-substring-no-properties (point)
                                                  (progn
                                                    (search-forward "</title>")
                                                    (search-backward "<"))))))
    (kill-buffer buffer)
    (w3m-decode-entities-string
     (mm-decode-string
      title
      (symbol-name (w3m-url-coding-system url))))))

(defun dmd--org-apply-to-headlines (function &optional prefix)
  (when (and (derived-mode-p 'org-mode)
             (buffer-file-name)
             (org-agenda-file-p))
    (let ((scope (when prefix
                   (message "Process headlines for the [b]uffer, [t]ree, [r]egion, [f]ile, [F]ile with archives, [a]gende, [A]genda with archives?")
                   (let ((r (read-char-exclusive)))
                     (cond ((char-equal r ?b) nil)
                           ((char-equal r ?t) 'tree)
                           ((char-equal r ?r) 'region)
                           ((char-equal r ?f) 'file)
                           ((char-equal r ?F) 'file-with-archives)
                           ((char-equal r ?a) 'agenda)
                           ((char-equal r ?A) 'agenda-with-archives))))))
      (save-excursion
        (ignore-errors (outline-up-heading 42))
        (org-map-entries function t scope)))))

(defun dmd-org-add-ids-to-headlines (&optional prefix)
  "Add ID properties to all headlines in the current buffer."
  (interactive "P")
  (dmd--org-apply-to-headlines 'org-id-get-create prefix))

(defun dmd-org-add-CREATED-to-headlines (&optional prefix)
  "Add \"CREATED\" properties to all headlines in the current buffer."
  (interactive "P")
  (dmd--org-apply-to-headlines 'dmd-org-add-created-prop-if-none prefix))

(defun dmd-doi-to-bib (url)
  (interactive "sURL: ")
  (request
   url
   :parser 'buffer-string
   :headers '(("Accept" . "text/bibliography; style=bibtex"))
   :success (function*
             (lambda (&key data &allow-other-keys)
               (with-current-buffer (get-buffer-create "*doi-to-bib*")
                 (delete-region (point-min) (point-max))
                 (insert data)
                 (switch-to-buffer-other-window (current-buffer)))))))

(defun dmd-org-clock-in-switch-to-state (state)
  "Switch to \"NEXT\" state unless:
- we are in `org-capture-mode'
- if the STATE is \"MEETING\"
- it's a habit (has STYLE=habit)"
  (cond ((or (string= state "MEETING")
             org-capture-mode
             (equal (org-entry-get (point) "STYLE") "habit"))
         state)
        (t "NEXT")))

(defun dmd-org-add-created-prop-if-none ()
  "Add a \"CREATED\" properties if none exists."
  (unless (org-entry-get (point) "CREATED")
    (org-set-property "CREATED" (format-time-string "[%Y-%m-%d %a %H:%M]" (org-read-date nil 'totime "today")))))

(defcustom org-agenda-skip-tags nil
  "Tags that should be excluded even if they have a SCHEDULED or DEADLINE property."
  :type '(repeat string)
  :group 'org-agenda)

(defun dmd-org-agenda-skip-tags-entry ()
  (when (find-if (lambda (s)
                   (find s (org-get-tags-at (point)) :test #'string=))
                 org-agenda-skip-tags)
    (org-end-of-subtree t)
    (point)))

(defun dmd-org-set-effort ()
  "Set an effort unless :
- we are in `org-capture-mode'
- there is already an effort"
  (unless (or org-capture-mode
              (org-entry-get (point) "Effort"))
    (org-set-effort)))

(defun dmd-org-active-timestamp-to-inactive-when-rescheduled ()
  "Convert some variable with active timestamp to inactive one."
  (when (eq org-log-note-purpose 'reschedule)
    (setq org-log-note-previous-state
          (substitute ?\] ?\> (substitute ?\[ ?\< org-log-note-previous-state :test #'char-equal)
                      :test #'char-equal))))

(defun dmd-org-indent-buffer ()
  "Indent current buffer"
  (interactive)
  (if (not (derived-mode-p 'org-mode))
      (user-error "Buffer isn't in Org-Mode")
    (org-content)
    (save-excursion
      (goto-char (point-min))
      (while (zerop (forward-line 1))
      (ignore-errors (org-indent-drawer))
      (org-indent-line)))))

(defun dmd-org-skip-bib-file ()
  (if (not (file-equal-p org-bib-notes-file (buffer-file-name)))
      t
    (goto-char (point-max))
    nil))

(defun dmd-org-skip-contacts-files ()
  (if (not (find (buffer-file-name) (org-contacts-files) :test #'file-equal-p))
      t
    (goto-char (point-max))
    nil))

(defun dmd--ask-sign-encrypt ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (cond ((re-search-forward
            (concat "^" (regexp-quote mail-header-separator) "\n") nil t)
           (goto-char (setq insert-loc (match-end 0)))
           (unless (looking-at "<#secure")
             (let ((c (read-char "[s]ign, [e]ncrypt or [n]othing?")))
               (cond ((char-equal c ?s)
                      (mml-secure-message-sign))
                     ((char-equal c ?e)
                      (mml-secure-message-sign-encrypt))
                     ((char-equal c ?n)
                      t)
                     (t (user-error "Command not recognised"))))
             ))
          (t (error
              "The message is corrupted. No mail header separator")))))

(defun dmd--split-python-args-for-doctsring (args)
  "(dmd--split-python-args-for-doctsring \"big_table, keys, other_silly_variable=None\")
-> (\"big_table\" \"keys\" \"other_silly_variable\")"
  (let* ((args-with-default (split-string args "," t split-string-default-separators))
         (args-without-default (mapcar (lambda (s)
                                         (replace-regexp-in-string "=.*$" "" s))
                                       args-with-default)))
    args-without-default))

(defun dmd--format-python-args-for-docstring (args)
  (s-join "
"
          (mapcar (lambda (s)
                    (format "        %s: variable documentation." s))
                  (dmd--split-python-args-for-doctsring args))))

(provide 'config-0-defuns)
