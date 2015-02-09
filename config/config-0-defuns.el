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

(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(defun sudo-edit (&optional arg)
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:localhost:" (ido-read-file-name "File: ")))
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

`ido-read-file-name' is used to find the files. Extensions can be
added to `*evince-extensions*'."
  (interactive
   (let* ((exts (copy-list *evince-extensions*))
          (ext (do* ((ret (concat "\\(" (pop exts)) (concat ret "\\|" ext))
                     (ext (pop exts) (pop exts)))
                   ((null exts) (concat ret "\\)")))))
     (list (ido-read-file-name
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

(defun dmd/terminal-emulator ()
  "Open a terminal emulator using `terminal-emulator'."
  (interactive)
  (start-process
   "Terminal Emulator"
   nil
   (etypecase terminal-emulator
     (string terminal-emulator)
     (function (funcall terminal-emulator))
     (symbol (symbol-value terminal-emulator)))))

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

(defun dmd/ido-kill-buffer (&optional kill-process)
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
                              (format "source ~/.python-virtualenvs/youtube-dl/bin/activate && youtube-dl --no-part --extract-audio --audio-format mp3 --audio-quality 128K %s" url))))
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

(defun ido-disable-line-truncation ()
  (set (make-local-variable 'truncate-lines) nil))

(defun ido-define-keys ()
  ;; C-n/p is more intuitive in vertical layout
  (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))

(defun dmd/text-mode-setup ()
  (interactive)
  (activate-input-method default-input-method)
  (visual-line-mode 1)
  (adaptive-wrap-prefix-mode 1))

(defun dmd-org-mode-reftex-setup ()
  (interactive)
  (when (and (derived-mode-p 'org-mode)
             (buffer-file-name))
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

(advice-add 'narrow-to-region :before
            '(lambda (&rest args)
               "Narrow to indirect buffer when called with a prefix argument."
               (when current-prefix-arg
                 (switch-to-buffer (clone-indirect-buffer nil nil))))
            '((name . narrow-to-indirect-region)))

(defun dmd-pomodoro (&optional time-prefix)
  (interactive "P")
  (let ((time (if time-prefix
                  (prefix-numeric-value time-prefix)
                25)))
    (run-at-time (format "%d min" time) nil 'dmd--pomodoro-end)))

(defun dmd--pomodoro-end ()
  (let* ((files (directory-files (expand-file-name "~/Music/Win/") t "\\.mp3$"))
         (file (elt files (random (length files)))))
    (emms-play-file file)))

(provide 'config-0-defuns)
