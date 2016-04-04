;;; init.el --- Initialization  -*- firestarter: (byte-recompile-file (buffer-file-name) nil 0); -*-

;; Copyright (C) 2015 Grégoire Jadi

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

;;; Commentary:

;;; Code:

(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))

(defvar elisp-dir (expand-file-name "elisp/" user-emacs-directory))
(defvar config-dir (expand-file-name "config/" user-emacs-directory))
(defvar autoloads-dir (expand-file-name "autoloads/" user-emacs-directory))
(defvar modules-dir (expand-file-name "modules/" user-emacs-directory))
(defvar elpa-dir (expand-file-name "elpa/packages" user-emacs-directory))
(defvar package-user-dir elpa-dir)

(defcustom src-dir (expand-file-name "~/src/elisp/")
  "The source directory where third-part modules are located."
  :group 'dmd-config)

(add-to-list 'load-path config-dir)
(add-to-list 'load-path elisp-dir)
(add-to-list 'load-path autoloads-dir)

(eval-and-compile
  (require 'bytecomp)
  (byte-compile-disable-warning 'cl-functions)
  (require 'cl))

(defvar dmd-config-modules
  (cl-loop for config-file in (directory-files config-dir nil "^config-.*.el$")
           collect (intern (file-name-base config-file)))
  "List of available configuration modules.")

;;; Default load-path
(dolist (root-dir (list elpa-dir modules-dir))
  (dolist (dir (directory-files root-dir t "^[^.]"))
	(when (file-directory-p dir)
	  (add-to-list 'load-path dir))))

;;; Custom load-path
(add-to-list 'load-path (expand-file-name "magit/lisp/" modules-dir))
(add-to-list 'load-path (expand-file-name "pdf-tools/pdf-tools-0.70/"
										  modules-dir))
(add-to-list 'load-path (expand-file-name "org-mode/lisp" modules-dir))
(add-to-list 'load-path (expand-file-name "org-mode/contrib/lisp" modules-dir))
(add-to-list 'load-path (expand-file-name "mu/mu4e" modules-dir))

;;; Custom Themes
(add-to-list 'custom-theme-load-path (expand-file-name
									  "modules/color-theme-sanityinc-tomorrow"
									  user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name
                                        "modules/solarized-emacs"
                                        user-emacs-directory))
(add-to-list 'custom-theme-load-path
			 (expand-file-name
			  "modules/replace-colorthemes/"
			  user-emacs-directory))

;;; Load Custom
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(require 'pdf-tools-autoloads nil t)
(when (fboundp 'pdf-tools-install)
  (pdf-tools-install))

;; Enabled/Disabled commands
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)

(with-eval-after-load 'info
  (defvar Info-directory-list)
  (defvar Info-additional-directory-list)
  (setq Info-directory-list (append Info-directory-list
                                    Info-default-directory-list
                                    Info-additional-directory-list
                                    (list
                                     (expand-file-name "../info" data-directory)
                                     (expand-file-name "doc" user-emacs-directory))))
  (require 'info-look))
(require 'info)

(require 'ispell)

(global-set-key (kbd "M-/") 'dabbrev-expand)
(global-set-key (kbd "C-x \\") 'align-regexp)
(global-set-key (kbd "C-S-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-S-r") 'isearch-backward-regexp)

(require 'emacs-w3m-autoloads)
(with-eval-after-load 'w3m
  (require 'w3m-util)
  (defun dmd--w3m-go-to-title-in-page ()
    (interactive)
    (let ((title (w3m-buffer-title (current-buffer)))
          (max-cut 10))
      (when title
        (goto-char (point-min))
        (while (and (not (search-forward-regexp title nil t))
                    (> (length title) max-cut)
                    (setq title (subseq title 0 (1- (length title))))))))))

(with-eval-after-load 'windmove
  (global-set-key (kbd "S-<up>") 'windmove-up)
  (global-set-key (kbd "S-<down>") 'windmove-down)
  (global-set-key (kbd "S-<right>") 'windmove-right)
  (global-set-key (kbd "S-<left>") 'windmove-left))
(require 'windmove)

;; Window switching. (C-x o goes to the next window)
(global-set-key (kbd "C-x C-o") (lambda () (interactive) (other-window -1)))

;; Use another version of zap-to-char (don't chopd the last char)
(defun zap-to-char- (arg char)
  "Kill up to and not including ARGth occurrence of CHAR.
Case is ignored if ‘case-fold-search’ is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found."
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

(global-set-key (kbd "M-z") 'zap-to-char-)

;; Move in window
(defun move-to-window-line-top ()
  "Goto the first visible line."
  (interactive)
  (move-to-window-line 0))

(defun move-to-window-line-bottom ()
  "Goto to the last visible line."
  (interactive)
  (move-to-window-line -1))

(global-set-key (kbd "C-c <") 'move-to-window-line-top)
(global-set-key (kbd "C-c >") 'move-to-window-line-bottom)

;; Inactive marks -- Quickmove between mark without disturbing transient-mark-mode
(defun dmd-push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region.
Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

(defun dmd-jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))

(global-set-key (kbd "C-`") 'dmd-push-mark-no-activate)
(global-set-key (kbd "M-`") 'dmd-jump-to-mark)

;; Scrolling -- Scroll up and down slowly by default (one line at time)
(defcustom dmd-small-scrolling 5
  "How many lines should be scrolled with `dmd-small-scroll-[up/down]-command'."
  :type 'integer
  :group 'dmd-config)

(defun dmd-small-scroll-up-command (&optional arg)
  "Scroll text of selected window upward ARG lines; or `dmd-small-scrolling' if no ARG."
  (interactive "^P")
  (let ((fun-scroll-up (if (fboundp 'scroll-up-command)
                           'scroll-up-command
                         'scroll-up)))
    (if arg
        (funcall fun-scroll-up arg)
      (funcall fun-scroll-up dmd-small-scrolling))))

(defun dmd-small-scroll-down-command (&optional arg)
  "Scroll text of selected window downward ARG lines; or `dmd-small-scrolling' if no ARG."
  (interactive "^P")
  (let ((fun-scroll-down (if (fboundp 'scroll-down-command)
                             'scroll-down-command
                           'scroll-down)))
    (if arg
        (funcall fun-scroll-down arg)
      (funcall fun-scroll-down dmd-small-scrolling))))

(global-set-key (kbd "C-v") 'dmd-small-scroll-up-command)
(global-set-key (kbd "M-v") 'dmd-small-scroll-down-command)

;; Remove annoying keybindings
(global-set-key (kbd "C-x C-z") nil)
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "C-x C-c") nil)

(require 'graze-url-autoloads)
(global-set-key (kbd "C-c y") 'gu-copy-url-at-point)
(global-set-key (kbd "C-c b") 'gu-browse-url)
(global-set-key (kbd "C-c w s") 'gu-search)

;; iy-go-to-char configuration
(with-eval-after-load 'iy-go-to-char
  (global-set-key (kbd "C-c f") 'riy-go-to-char)
  (global-set-key (kbd "C-c F") 'iy-go-to-char-backward)
  (global-set-key (kbd "C-c ;") 'iy-go-to-char-continue)
  (global-set-key (kbd "C-c ,") 'iy-go-to-char-continue-backward))

;; Quiet!
(with-eval-after-load 'config-quiet
  (global-set-key (kbd "C-c q") 'quiet-mode))

;; Winner configuration
(with-eval-after-load 'winner
  (global-set-key (kbd "C-c u") 'winner-undo)
  (global-set-key (kbd "C-c r") 'winner-redo)
  (winner-mode 1))

(global-set-key (kbd "<f11>") 'toggle-frame-fullscreen)

;;;; Terminal Emulator
;; C-x 4 t is for multi-term in another window, so a terminal in
;; another frame should be an xterm or similar.
(defcustom terminal-emulator "xterm"
  "A terminal emulator to use."
  :group 'external)

(defcustom terminal-emulator-parameters nil
  "The parameters to send to the terminal emulator."
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

(global-set-key (kbd "C-x 5 t") 'dmd-terminal-emulator)

(global-set-key (kbd "M-Q") 'unfill-paragraph)

(with-eval-after-load 'compile-cache
  (require 'compile)
  (global-set-key (kbd "<f5>") 'compile-cache)
  (global-set-key (kbd "<f6>") 'recompile))

(global-set-key (kbd "M-\\") 'execute-extended-command)

;;;; Helm
(with-eval-after-load 'helm-config
  (require 'helm-command)
  (require 'helm-files)
  (require 'helm-buffers)
  (require 'helm-ag)
  (require 'helm-bibtex)
  (require 'helm-pages)
  (require 'swiper)
  (require 'swiper-helm)

  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-c m") 'helm-M-x)
  (global-set-key (kbd "C-c C-m") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-x b") 'helm-buffers-list)
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-set-key (kbd "C-c j") 'helm-pages)
  (global-set-key (kbd "C-s") 'swiper-helm)
  (global-set-key (kbd "C-r") 'swiper-helm)
  (global-set-key (kbd "C-;") 'newline-and-indent))
(require 'helm-config)
(helm-mode 1)

(with-eval-after-load 'comint
  (defcustom comint-buffer-minimum-size 0
    "The minimum size for comint buffer when truncating."
    :type 'integer
    :group 'comint)
  (defun dmd-comint-truncate-buffer (&optional n)
    "Does what comint-truncate-buffer should do. That is, truncate
the buffer to keep N lines.

If N is not set, use `comint-buffer-minimum-size'."
    (interactive "P")
    (let ((comint-buffer-maximum-size
           (or n
               (- (line-number-at-pos (point-max)) (line-number-at-pos)))))
      (comint-truncate-buffer)))
  (define-key comint-mode-map (kbd "C-c C-l") 'dmd-comint-truncate-buffer))

;;; Hideshow
(add-hook 'prog-mode-hook (lambda () (hs-minor-mode 1)))
(with-eval-after-load 'hideshow
  (define-key hs-minor-mode-map (kbd "C-c -") 'hs-hide-block)
  (define-key hs-minor-mode-map (kbd "C-c _") 'hs-hide-all)
  (define-key hs-minor-mode-map (kbd "C-c =") 'hs-show-block)
  (define-key hs-minor-mode-map (kbd "C-c +") 'hs-show-all))

;;; Company
(global-set-key (kbd "C-SPC") 'company-complete)
(with-eval-after-load 'company
  (global-company-mode))

;;; Magit
(require 'magit-autoloads)
(global-set-key (kbd "C-c g") 'magit-status)
(with-eval-after-load 'magit
  (require 'git-commit)
  (require 'magit-svn)
  (require 'orgit)
  
  (add-to-list 'Info-directory-list
               (expand-file-name "magit/Documentation" modules-dir))

  (add-hook 'magit-mode-hook 'magit-svn-mode))

(global-set-key (kbd "C-h j") 'javadoc-lookup)
(with-eval-after-load 'javadoc-lookup
  (javadoc-add-roots "/usr/share/doc/openjdk-7-jdk/api"))

(with-eval-after-load 'eclim
  (defvar eclim-mode-map)
  (define-key eclim-mode-map (kbd "C-c C-e p r") 'eclim-run-class)
  (add-hook 'eclim-mode-hook 'company-emacs-eclim-setup)
  (add-hook 'java-mode-hook 'eclim-mode))

(global-set-key (kbd "M-o s") 'occur-by-moccur)
(global-set-key (kbd "M-o m") 'moccur)
(global-set-key (kbd "M-o d") 'dmoccur)
(with-eval-after-load 'color-moccur
  (require 'moccur-edit)
  (defalias 'mgrep 'moccur-grep)
  (defalias 'mrgrep 'moccur-grep-find))

(require 'org)
(with-eval-after-load 'org
  (require 'subr-x)
  (require 'org-contacts)
  (require 'org-clock)
  (require 'org-habit)
  (require 'org-agenda)
  (require 'org-id)
  (require 'org-attach)
  (require 'org-bullets)
  (require 'org-mime)
  (require 'org-drill (expand-file-name "org-drill/org-drill.el" modules-dir))
  (require 'org-ref)
  (require 'org-agenda)
  (require 'dmd-org-mode)
  (require 'diary-lib)
  (require 'ox-beamer)

  (dmd--update-org-agenda-files)

  (define-key org-beamer-mode-map (kbd "C-c C-b") nil)

  (global-set-key (kbd "C-c o l") 'org-store-link)
  (global-set-key (kbd "C-c o a") 'org-agenda)
  (global-set-key (kbd "C-c o g") 'org-clock-goto)
  (global-set-key (kbd "C-c o c") 'org-capture)
  (global-set-key (kbd "C-c o n") 'org-annotate-file)

  (add-to-list 'org-export-filter-headline-functions
               'org-latex-ignore-heading-filter-headline)

  (setq org-export-async-init-file (expand-file-name "init-org-async.el" user-emacs-directory))

  (setq org-element-use-cache nil)

  (setq org-capture-templates
        `(("n" "Note" entry
		   (function dmd--org-capture-elfeed)
		   "* %a
:PROPERTIES:
:CREATED: %U
:END:
" :prepend t :empty-lines 1)
		  ("T" "Task in current buffer" entry
           (function ,(dmd--org-capture-headline "Task"))
           "* TODO %?
:PROPERTIES:
:CREATED: %U
:END:
%a
" :prepend t :empty-lines 1)
          ("t" "Task" entry
          (file+headline "~/org/capture.org" "Task")
          "* TODO %?
:PROPERTIES:
:CREATED: %U
:END:
%a
" :prepend t :empty-lines 1)
         ("m" "Mail" entry
          (file+headline "~/org/capture.org" "Task")
          "* NEXT Mail from %:from on %:subject :mail:
SCHEDULED: %t
:PROPERTIES:
:CREATED: %U
:END:
%a" :prepend t :immediate-finish t :empty-lines 1)
         ("J" "New journal entry in current buffer" entry

		  (function dmd--org-capture-datetree)
          "* %?" :immediate-finish t :jump-to-captured t :empty-lines 1 :unnarrowed t)
         ("j" "New journal entry" entry
          (file+datetree "~/org/journals.org")
          "* %?" :immediate-finish t :jump-to-captured t :empty-lines 1 :unnarrowed t)
         ("e" "Meeting" entry
          (file+headline "~/org/capture.org" "Task")
          "* MEETING with %?
:PROPERTIES:
:CREATED: %U
:END:" :prepend t :empty-lines 1 :clock-in t :clock-resume t))
        org-capture-templates-contexts
        '(("m" ((in-mode . "mu4e-view-mode")))
          ("T" ((in-mode . "org-mode")))
          ("J" ((in-mode . "org-mode")))
		  ("n" ((in-mode . "elfeed")))))

  (add-to-list 'org-babel-default-header-args '(padline . no))

  (diary-list-entries (calendar-current-date) nil 'list-only)
  (mapc (lambda (file)
          (bury-buffer (find-file-noselect file)))
        diary-included-files)
  (define-key org-mode-map (kbd "C-c )") 'helm-bibtex)
  (define-key org-mode-map (kbd "C-c j")
	'(lambda (&optional prefix)
	   (interactive "P")
	   (if prefix
		   (helm-org-agenda-files-headings)
		 (helm-org-in-buffer-headings))))
  (define-key org-mode-map (kbd "C-c >") 'org-time-stamp-inactive)

  (add-to-list 'Info-directory-list
               (expand-file-name "org-mode/doc" modules-dir))

  ;; Prompt for a date for CREATED properties
  (add-to-list 'org-property-set-functions-alist
               (cons "CREATED" '(lambda (prompt collection
                                                &optional predicate require-match initial-input
                                                hist def inherit-input-method)
                                  (format-time-string "[%Y-%m-%d %a %H:%M]" (org-read-date nil 'totime nil prompt nil def nil)))))

  (advice-add #'org-attach-open :override #'helm-org-attach-open)

  (advice-add #'org-toggle-latex-fragment :around (lambda (oldfun &optional arg)
													"Temp fix for org-latex-preview in non-file buffers (skip the buffer instead of throwing an error)"
													(if (buffer-file-name (buffer-base-buffer))
														(funcall oldfun arg)
													  (message "Can't preview LaTeX fragment in a non-file buffer"))))

  ;;; Don't scatter LaTeX images
  (make-directory org-latex-preview-ltxpng-directory t)

  (add-hook 'org-clock-in-prepare-hook 'dmd-org-set-effort)

  (org-babel-lob-ingest (expand-file-name "org-mode/doc/library-of-babel.org" modules-dir))
  (org-babel-lob-ingest (expand-file-name "lob.org" user-emacs-directory))

  ;; message-mode
  (add-hook 'message-mode-hook 'turn-on-orgstruct)
  (add-hook 'message-mode-hook 'turn-on-orgstruct++)
  (add-hook 'message-mode-hook 'turn-on-orgtbl)

  (add-hook 'org-ref-open-notes-functions 'dmd-org-ref-open-bibtex-notes)

  ;; Always clocking ! Always !
  (defvar dmd-always-clocking-timer (run-at-time 't 30 'dmd-always-clocking-check))
  
  ;; Make windmove work in org-mode:
  (add-hook 'org-shiftup-final-hook 'windmove-up)
  (add-hook 'org-shiftleft-final-hook 'windmove-left)
  (add-hook 'org-shiftdown-final-hook 'windmove-down)
  (add-hook 'org-shiftright-final-hook 'windmove-right)

  (add-hook 'org-after-refile-insert-hook 'basic-save-buffer)

  (add-hook 'org-mode-hook 'dmd-set-ispell-dictionary-from-org)
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (add-hook 'org-mode-hook 'dmd-org-mode-reftex-setup)
  (add-hook 'org-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'dmd-org-add-ids-to-headlines nil 'local)
              (add-hook 'before-save-hook 'dmd-org-add-CREATED-to-headlines nil 'local)
              (add-hook 'before-save-hook 'org-update-parent-todo-statistics nil 'local))))

(require' pyvenv)
(with-eval-after-load 'pyvenv
  (pyvenv-tracking-mode 1)
  (pyvenv-mode 1)
  (add-hook 'pyvenv-post-activate-hooks
			(lambda ()
			  (setq-default python-shell-buffer-name python-shell-buffer-name))))

(require 'elpy)
(with-eval-after-load 'elpy
  (elpy-enable))

(with-eval-after-load 'message
  (define-key message-mode-map (kbd "C-c C-c") nil))

(with-eval-after-load 'bibtex
  (define-key bibtex-mode-map (kbd "C-c C-o") 'dmd-bibtex-open)
  (add-hook 'bibtex-mode-hook 'bibtex-set-dialect))

(global-set-key (kbd "C-x #") 'delete-frame)

(require 'yasnippet)
(require 'company-yasnippet)
(define-key yas-minor-mode-map (kbd "C-c & C-s") 'company-yasnippet)
(global-set-key (kbd "M-S-c") 'company-yasnippet)
(yas-global-mode 1)

(require 'flycheck)
(require 'flycheck-pos-tip)
(global-flycheck-mode 1)

(with-eval-after-load 'mu4e
  (add-to-list 'Info-directory-list
               (expand-file-name (expand-file-name
                                  "mu/mu4e"
                                  modules-dir)))
  (setq mu4e-mu-binary (or (executable-find "mu")
                           (expand-file-name "mu/mu/mu"
                                             modules-dir)))
  (require 'org-mu4e)
  (add-hook 'mu4e-view-mode-hook
            (lambda()
              ;; try to emulate some of the eww key-bindings
              (local-set-key (kbd "<tab>") 'w3m-next-anchor)
              (local-set-key (kbd "<backtab>") 'w3m-previous-anchor)
			  (local-set-key (kbd "*") (lambda ()
										 (interactive)
										 (org-capture nil "m")
										 (mu4e-view-mark-for-flag))))))

(with-eval-after-load 'org-mu4e
  (setq org-mu4e-link-query-in-headers-mode nil)
  ;; Redefine org-mu4e-open to fit my needs
  (defun org-mu4e-open (path)
	"Open the mu4e message (for paths starting with 'msgid:') or run
the query (for paths starting with 'query:')."
	(require 'mu4e)
	(cond
	 ((string-match "^msgid:\\(.+\\)" path)
	  (let ((msgid (match-string 1 path)))
		(mu4e-headers-search (format "msgid:%s" msgid) current-prefix-arg)
		(mu4e~headers-redraw-get-view-window)
		(other-window 1)
		(mu4e-view-message-with-msgid msgid)))
	 ((string-match "^query:\\(.+\\)" path)
	  (let ((mu4e-headers-include-related t)
			(mu4e-headers-show-threads t))
		(mu4e-headers-search (match-string 1 path) current-prefix-arg)))
	 (t (mu4e-error "mu4e: unrecognized link type '%s'" path)))))

(require 'mu4e)

(require 'which-key)
(which-key-mode 1)

(mapc (lambda (module)
        (message "Loading %s" module)
        ;; (require module)
        (unless (ignore-errors (require module))
          (warn "Failed to load module `%s'" module)))
	  dmd-config-modules)


(require 'env-helper)
(file-notify-add-watch "/etc/environment"
					   '(change)
					   #'dmd--environment-watcher)

;; Automagically tail log files
(add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-tail-mode))
(with-eval-after-load 'autorevert
  (defun dmd--etc-log-tail-handler ()
    (goto-char (point-max))
    (make-local-variable 'auto-revert-interval)
    (setq auto-revert-interval 1)
    (auto-revert-set-timer)
    (setq auto-revert-verbose nil)
    (read-only-mode 1)
    (font-lock-mode -1)
    (visual-line-mode -1)
    (toggle-truncate-lines 1)
    (when (fboundp 'show-smartparens-mode)
      (show-smartparens-mode 0)))
  (add-hook 'auto-revert-tail-mode-hook 'dmd--etc-log-tail-handler))

;;; Octave
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

;;; Slime
(load (expand-file-name "~/quicklisp/slime-helper.el") t)
(load (expand-file-name "~/quicklisp/clhs-use-local.el") t)
(with-eval-after-load 'slime
  (defalias 'srepl 'slime-repl)
  (require 'slime-company)
  (slime-setup '(slime-repl
                 inferior-slime
                 slime-asdf
                 slime-banner
                 slime-autodoc
                 slime-editing-commands
                 slime-fancy-inspector
                 slime-fancy
                 slime-snapshot
                 slime-fontifying-fu
                 slime-fuzzy
                 slime-indentation
                 slime-package-fu
                 slime-references
                 slime-scratch
                 slime-xref-browser
                 slime-presentations
                 slime-company))

  (slime-autodoc-mode)

  (setq inferior-lisp-program "~/bin/sbcl"
        slime-complete-symbol-function 'slime-fuzzy-complete-symbol
        slime-startup-animation t
        slime-complete-symbol*-fancy t
        slime-net-coding-system 'utf-8-unix)

  (add-hook 'lisp-mode-hook (lambda () (slime-mode 1)))
  (add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode 1)))

  (defun common-lisp-indentation ()
    (set (make-local-variable lisp-indent-function)
         'common-lisp-indent-function))

  (add-hook 'lisp-mode-hook 'common-lisp-indentation)

  (define-key slime-repl-mode-map (kbd "C-c C-v C-l") 'slime-pretty-print-presentation-at-point)
  (define-key slime-repl-mode-map (kbd "C-c C-v l") 'slime-pretty-print-presentation-at-point)
  (define-key slime-repl-mode-map (kbd "C-x M-e") 'slime-pprint-eval-last-expression)
  (define-key slime-mode-map (kbd "C-x M-e") 'slime-pprint-eval-last-expression)

  (defun slime-dc ()
    "Close the current connection and the repl-buffer"
    (interactive)
    (save-window-excursion
      (slime-switch-to-output-buffer)
      (kill-buffer)
      (slime-net-close (slime-connection))))

  ;; Store fasls here
  (make-directory "/tmp/slime-fasls/" t) ;; be sure the directory exists
  (setq slime-compile-file-options '(:fasl-directory "/tmp/slime-fasls/"))

  ;; Add a directory to asdf:*central-registry*
  (defslime-repl-shortcut slime-repl-add-to-central-registry
    ("add-to-central-registry" "+a" "add")
    (:handler (lambda (directory)
                (interactive
                 (list (expand-file-name (file-name-as-directory
                                          (read-directory-name
                                           "Add directory: "
                                           (slime-eval '(swank:default-directory))
                                           nil nil "")))))
                (insert "(cl:pushnew (cl:truename #P\"" directory "\") asdf:*central-registry* :test #'equal)")
                (slime-repl-send-input t)))
    (:one-liner "Add a directory to asdf:*central-registry*"))

;;; Quickload a system
  (defslime-repl-shortcut slime-repl-quickload
    ("quickload" "+ql" "ql")
    (:handler (lambda ()
                (interactive)
                (let* ((system-names
                        (slime-eval '(cl:nunion
                                      (swank:list-asdf-systems)
                                      (cl:nunion
                                       (cl:mapcar 'ql-dist:name
                                                  (ql:system-list))
                                       (ql:list-local-systems)
                                       :test 'cl:string=)
                                      :test 'cl:string=)))
                       (default-value (slime-find-asd-file
                                       (or default-directory
                                           (buffer-file-name))
                                       system-names))
                       (prompt (concat "System "
                                       (if default-value
                                           (format " (default `%s'): " default-value)
                                         ": ")))
                       (system (completing-read prompt
                                                system-names
                                                nil nil nil
                                                'slime-system-history
                                                default-value)))
                  (insert "(ql:quickload :" system ")")
                  (slime-repl-send-input t))))
    (:one-liner "Quickload a system"))

;;; Awesome hacks available!
  (setq slime-enable-evaluate-in-emacs t)

;;; CLHS
  (quicklisp-clhs-setup-hyperspec-root)
  (define-key slime-mode-map (kbd "C-c C-d f") 'common-lisp-hyperspec)
  (define-key slime-repl-mode-map (kbd "C-c C-d f") 'common-lisp-hyperspec)

;;; dpans
  (defun dmd-ansicl-lookup (major-mode)
    (info-lookup-add-help
     :mode major-mode
     :regexp "[^][()'\" \t\n]+"
     :ignore-case t
     :doc-spec '(("(ansicl)Symbol Index" nil nil nil))))
  (mapc 'dmd-ansicl-lookup
        '(lisp-mode
          slime-repl-mode))

;;; bind C-c / to slime-selector
  (define-key slime-mode-map (kbd "C-c /") 'slime-selector)
  (define-key slime-repl-mode-map (kbd "C-c /") 'slime-selector)

  (defun dmd-dump-slime ()
    "Dump current SLIME instance to PWD/slime.img"
    (interactive)
    (save-excursion
      (switch-to-buffer-other-window "*inferior-lisp*")
      (goto-char (point-min))
      (insert (format "(trivial-dump-core::sbcl-dump-image-slime %S)" (expand-file-name "slime.img")))
      (inferior-slime-return)))

  (defun dmd-load-slime ()
    "Load a previously saved SLIME image (see `dmd-dump-slime') named PWD/slime.img."
    (interactive)
    (slime-start :program "~/bin/sbcl"
                 :program-args '("--core" "slime.img")
                 :directory default-directory)))

(with-eval-after-load 'copyright
  ;;; redefined skeleton (original in copyright.el)
  (define-skeleton copyright
    "Insert a copyright by $ORGANIZATION notice at cursor."
    nil
    comment-start
    " Copyright (C) " `(format-time-string "%Y") " by "
    (or (getenv "ORGANIZATION")
        user-full-name)
    comment-end \n
    comment-start
    " See the file LICENSE for copying permission."
    comment-end \n))

(with-eval-after-load 'mule
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (prefer-coding-system 'utf-8))

(require 'firestarter)
(defvar firestarter nil)
(firestarter-mode 1)

(with-eval-after-load 'ansi-color
  (ansi-color-for-comint-mode-on))

(require 'saveplace)

(add-to-list 'auto-mode-alist '("wscript" . python-mode))

(require 'undo-tree)
(global-undo-tree-mode 1)

(require 'projectile)
(with-eval-after-load 'projectile
  ;; Remove inexistant projects
  (dolist (proj projectile-known-projects)
	(unless (file-exists-p proj)
	  (setq projectile-known-projects (delete proj projectile-known-projects))))
  (projectile-save-known-projects))
(projectile-global-mode 1)
(require 'helm-projectile)
(helm-projectile-on)

(with-eval-after-load 'elisp-slime-nav
  (add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)
  (add-hook 'ielm-mode-hook 'elisp-slime-nav-mode))

(with-eval-after-load 'eldoc
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode))

(require 'paredit)
(with-eval-after-load 'paredit
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'ielm-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'slime-repl-mode-hook 'enable-paredit-mode))

(with-eval-after-load 'redshank-loader
  (redshank-setup '(lisp-mode-hook slime-repl-mode-hook) t))

(with-eval-after-load 'org-game
  :load-path (lambda () (expand-file-name "org-game" src-dir))
  (org-game-start))

(require 'elfeed-autoloads)
(require 'elfeed-org-autoloads)
(with-eval-after-load 'elfeed
  (add-hook 'kill-emacs-hook 'elfeed-db-compact)
  (elfeed-org)
  (defun dmd-elfeed-search-tag-mustread ()
    (interactive)
    (elfeed-search-tag-all 'mustread))
  (defun dmd-elfeed-show-tag-mustread ()
    (interactive)
    (elfeed-show-tag 'mustread))
  (define-key elfeed-search-mode-map (kbd "e") 'dmd-elfeed-search-tag-mustread)
  (define-key elfeed-show-mode-map (kbd "e") 'dmd-elfeed-show-tag-mustread))

;; Save a list of recent files visited.
(recentf-mode 1)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; Text-mode Hook
(add-hook 'text-mode-hook 'dmd-text-mode-setup)

;; Message-mode hook
(add-hook 'message-mode-hook 'dmd-text-mode-setup)

;; Prog-mode hook
(add-hook 'prog-mode-hook 'hs-minor-mode)
(add-hook 'prog-mode-hook 'subword-mode)
(add-hook 'prog-mode-hook 'glasses-mode)


;;;; Alias
(defalias 'renb 'dmd-rename-buffer)
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'isp 'ispell-change-dictionary)

;;; Debbugs
(require 'debbugs-autoloads)
(with-eval-after-load 'debbugs
  (add-to-list 'Info-directory-list
               (expand-file-name
				"debbugs"
				elpa-dir)))

;;; Beacon
(require 'beacon)
(beacon-mode 1)

;;;; Theme

;; tab and indentation configuration
(setq indent-tabs-mode nil)
(setq tab-width 4)

(global-font-lock-mode 1)
(setq font-lock-maximum-decoration t
      font-lock-verbose nil)

;; Set the default font
(defvar *fonts-list* '("-unknown-Inconsolata-normal-normal-normal-*-15-*-*-*-m-0-iso10646-1"
                       "-unknown-DejaVu Sans Mono-normal-normal-normal-*-13-*-*-*-m-0-iso10646-1"))

(add-to-list 'default-frame-alist `(font . ,(find-if #'font-info *fonts-list*)))
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq initial-frame-alist (append initial-frame-alist
                                  (copy-alist default-frame-alist)))

;;; Switch all buffers to fundamental hooks cuz sometimes something
;;; bad has happened but we don't care if we're killing emacs.
(add-hook 'kill-emacs-hook (lambda ()
							 (dolist (buffer (buffer-list))
							   (with-current-buffer buffer
								 (fundamental-mode)))))

;;; init.el ends here
