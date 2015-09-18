;;; init.el --- Initialization

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
(defvar elpa-dir (expand-file-name "elpa/" user-emacs-directory))
(defvar config-dir (expand-file-name "config/" user-emacs-directory))

(defcustom src-dir (expand-file-name "~/src/elisp/")
  "The source directory where third-part modules are located."
  :group 'dmd-config)

(add-to-list 'load-path config-dir)

;; custom-file configuration
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(eval-and-compile
  (require 'bytecomp)
  (byte-compile-disable-warning 'cl-functions)
  (require 'cl))

(defvar dmd-modules
  (cl-loop for config-file in (directory-files config-dir nil "^config-.*.el$")
           collect (intern (file-name-base config-file)))
  "List of available configuration modules.")

(add-to-list 'load-path (expand-file-name "use-package/" src-dir))

(defvar use-package-verbose t)
(require 'use-package)
(require 'bind-key)

(defvar dmd/required
  '(saveplace
    ffap
    uniquify
    ansi-color
    recentf
    verbiste
    info
    helm-config
    ebib
    key-chord
    graze-url
    compile-cache
    dired
    comint
    doc-view
    hideshow
    gtk-lookup
    semantic
    semantic/ia
    semantic/bovine/gcc
    semantic/imenu
    cedet-global
    company
    company-yasnippet
    http-post-simple
    thingatpt
    imenu
    bytecomp
    el-dispatcher
    doctags
    ediff
    emms-setup
    emms-player-mpv-quiet
    magit
    magit-svn
    magit-blame
    tidy-autoloads
    rcirc
    shoes-off-log
    shoes-off
    cc-mode
    javadoc-lookup
    maven-fetch
    eclim
    eclimd
    company-emacs-eclim
    js2-mode
    moz
    xlicense
    skeleton
    elisp-slime-nav
    redshank-loader
    markdown-mode
    workgroups
    scratch
    undo-tree
    linum
    rw-hunspell
    rw-ispell
    rw-language-and-country-codes
    fic-ext-mode
    markit
    yaml-mode
    dired-x
    rainbow-mode
    chm-view
    cursor-chg
    woman
    browse-kill-ring
    manual-tagging
    w3m-wget
    smart-tab
    smerge-mode
    edebug
    compilation-font
    parallel
    ess
    ess-site
    gnuplot
    sendmail
    message
    mm-decode
    mail-utils
    nnir
    epg
    epa-mail
    adaptive-wrap
    multiple-cursors
    multi-term
    tuareg
    ocamldebug
    god-mode
    reftex
    bibtex
    org
    org-ref
    org-loaddefs
    ob-stan
    ob-haskell
    ob-io
    ob-java
    ob-js
    ob-keys
    ob-latex
    ob-ledger
    ob-lilypond
    ob-lisp
    ob-makefile
    ob-matlab
    ob-maxima
    ob-mscgen
    ob-octave
    ob-org
    ob-perl
    ob-picolisp
    ob-plantuml
    ob-ruby
    ob-sass
    ob-scala
    ob-scheme
    ob-screen
    ob-shell
    ob-shen
    ob-sql
    ob-sqlite
    ob-table
    ob-tangle
    ob-awk
    ob-calc
    ob-C
    ob-clojure
    ob-comint
    ob-coq
    ob-core
    ob-css
    ob-ditaa
    ob-dot
    ob-emacs-lisp
    ob-eval
    ob-exp
    ob-fortran
    ob-gnuplot
    ob-groovy
    ob-sed
    ob-forth
    ob-lob
    ob-ocaml
    ob-processing
    ob-python
    ob-ref
    ob-abc
    ob-asymptote
    ob-ebnf
    ob-J
    ob-R
    doi-utils
    jmax-bibtex
    pubmed
    arxiv
    sci-id
    isbn
    org-agenda
    org-clock
    org-game
    org-archive
    org-plot
    ox
    ox-latex
    ox-beamer
    ox-md
    ox-reveal
    org-list
    org-drill
    calendar
    diary-lib
    org-annotate-file
    org-contacts
    org-magit
    org-man
    org-feed
    org-habit
    plantuml-mode
    projectile
    helm
    helm-projectile
    helm-org
    helm-bibtex
    ag
    helm-ag
    prolog
    ediprolog
    view
    rust-mode
    scala-mode-auto
    ensime
    color-moccur
    moccur-edit
    slime-autoloads
    slime
    info-look
    smartparens
    smartparens-config
    w3m-load
    w3m
    w3m-search
    browse-url
    pandoc-mode
    xmlgen
    sgml-mode
    xwidget
    parallel-xwidget
    ace-window
    ispell
    marmalade-upload
    yasnippet
    pomodoro
    page-break-lines
    helm-pages
    flycheck
    flycheck-pos-tip
    mu4e
    org-mu4e
    epg-config
    firestarter
    calfw
    calfw-org
    cal-fw-cal
    request)
  "List of required modules.")

(load (expand-file-name "~/quicklisp/slime-helper.el") t)
(load (expand-file-name "~/quicklisp/clhs-use-local.el") t)

(use-package pdf-tools
  :load-path (lambda () (expand-file-name src-dir "pdf-tools/pdf-tools-0.50/"))
  :config
  (pdf-tools-install))

(use-package diary-lib
  :config
  (diary-list-entries (calendar-current-date) nil 'list-only)
  (mapc (lambda (file)
          (bury-buffer (find-file-noselect file)))
        (append diary-included-files
                (org-agenda-files))))

;; Enabled/Disabled commands
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)

(use-package dabbrev
  :bind (("M-/" . dabbrev-expand)))

(use-package align
  :bind (("C-x \\" . align-regexp)))

(use-package isearch
  :bind (("C-s" . isearch-forward-regexp)
         ("C-r" . isearch-backward-regexp)))

(use-package windmove
  :bind* (("S-<up>" . windmove-up)
          ("S-<down>" . windmove-down)
          ("S-<right>" . windmove-right)
          ("S-<left>" . windmove-left)))

;; Window switching. (C-x o goes to the next window)
(bind-key "C-x C-o" (lambda () (interactive) (other-window -1)))

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

(bind-key "M-z" 'zap-to-char-)

;; Move in window
(defun move-to-window-line-top ()
  "Goto the first visible line."
  (interactive)
  (move-to-window-line 0))

(defun move-to-window-line-bottom ()
  "Goto to the last visible line."
  (interactive)
  (move-to-window-line -1))

(bind-keys ("C-c <" . move-to-window-line-top)
           ("C-c >" . move-to-window-line-bottom))


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

(bind-keys ("C-`" . push-mark-no-activate)
           ("M-`" . jump-to-mark))

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

(bind-keys ("C-v" . dmd-small-scroll-up-command)
           ("M-v" . dmd-small-scroll-up-command))

;; Remove annoying keybindings
(unbind-key "C-x C-z")
(unbind-key "C-x C-c")

(use-package graze-url
  :bind (("C-c y" . gu-copy-url-at-point)
         ("C-c b" . gu-browse-url)
         ("C-c w s" . gu-search)))

;; iy-go-to-char configuration
(use-package iy-go-to-char
  :load-path (lambda () (expand-file-name "iy-go-to-char" src-dir))
  :bind (("C-c f" . iy-go-to-char)
         ("C-c F" . iy-go-to-char-backward)
         ("C-c ;" . iy-go-to-char-continue)
         ("C-c ," . iy-go-to-char-continue-backward)))

;; Quiet!
(use-package config-quiet
  :bind (("C-c q" . quiet-mode)))

;; Winner configuration
(use-package winner
  :bind (("C-c u" . winner-undo)
         ("C-c r" . winner-redo)))

(bind-key "<f11>" 'toggle-fullscreen)

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

(bind-key "C-x 5 t" 'dmd-terminal-emulator)

(bind-key "M-Q" 'unfill-paragraph)

(use-package compile
  :bind (("<f5>" . compile-cache)
         ("<f6>" . recompile)))

(global-set-key (kbd "M-\\") 'execute-extended-command)

;;;; Helm
(use-package helm
  :bind (("M-x" . helm-M-x)
         ("C-c m" . helm-M-x)
         ("C-c C-m" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-buffers-list)))

(global-set-key (kbd "C-;") 'newline-and-indent)

(use-package comint
  :config
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
  (bind-key "C-c C-l" 'dmd-comint-truncate-buffer comint-mode-map))

(use-package doc-view
  :config
  (defun dmd-doc-view-info ()
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

  (defun dmd-doc-view-external ()
    "Open the current document using an external program."
    (interactive)
    (start-process "doc-view external" (generate-new-buffer " *DocView External Viewer*")
                   "/usr/bin/evince" buffer-file-name))
  
  (bind-keys :map doc-view-mode-map
             ("C-c C-i" . dmd-doc-view-info)
             ("C-c C-v" . dmd-doc-view-external)))

(use-package hideshow
  :config
  (bind-keys :map hs-minor-mode-map
             ("C-c -" . hs-hide-block)
             ("C-c _" . hs-hide-all)
             ("C-c =" . hs-show-block)
             ("C-c +" . hs-show-all)))

(use-package company
  :load-path (lambda () (expand-file-name "company-mode" src-dir))
  :bind (("C-SPC" . company-complete)))

(use-package company-yasnippet
  :load-path (lambda () (expand-file-name "company-mode" src-dir))
  :bind (("M-C" . company-yasnippet)))

(use-package magit
  :load-path (lambda () (expand-file-name "magit" src-dir))
  :bind (("C-c g" . magit-status)))

(use-package javadoc-lookup
  :bind (("C-h j" . javadoc-lookup)))

(define-key eclim-mode-map (kbd "C-c C-e p r") 'eclim-run-class)

(define-prefix-command 'moccur-map)
(define-key global-map (kbd "M-o") 'moccur-map)

(define-key moccur-map (kbd "s") 'occur-by-moccur)
(define-key moccur-map (kbd "m") 'moccur)
(define-key moccur-map (kbd "d") 'dmoccur)

(define-key dired-mode-map (kbd "M-o") 'moccur-map)

(global-set-key (kbd "s-f") 'god-mode-all)
(global-set-key (kbd "s-c") 'god-local-mode)

(define-key org-mode-map (kbd "C-c )") 'helm-bibtex)

(define-key message-mode-map (kbd "C-c C-c") nil)

(define-key org-agenda-mode-map (kbd "x") nil)

(define-key bibtex-mode-map (kbd "C-c C-o") 'dmd-bibtex-open)

(define-key gnus-summary-mode-map (kbd "i") (kbd "L S"))
(define-key gnus-summary-mode-map (kbd "y") (kbd "I S"))

(global-set-key (kbd "C-x #") 'delete-frame)

(define-key org-beamer-mode-map (kbd "C-c C-b") nil)

(define-key org-mode-map (kbd "C-c j") (lambda (&optional prefix)
                                           (interactive "P")
                                           (if prefix
                                               (helm-org-agenda-files-headings)
                                             (helm-org-in-buffer-headings))))

(global-set-key (kbd "<f9>") 'org-agenda)

(define-key yas-minor-mode-map (kbd "C-c & C-s") 'company-yasnippet)

(global-set-key (kbd "C-c j") 'helm-pages)

(define-key mu4e-view-mode-map (kbd "<tab>") 'shr-next-link)
(define-key mu4e-view-mode-map (kbd "<backtab>") 'shr-previous-link)

(define-key org-mode-map (kbd "C-c >") 'org-time-stamp-inactive)

(mapc (lambda (module)
        (message "Loading %s" module)
        ;; (require module)
        (unless (ignore-errors (require module))
          (warn "Failed to load module `%s'" module)))
      (append dmd/required dmd/modules))

(provide 'init)

;;; init.el ends here

;; Local Variables:
;; firestarter: (byte-recompile-file (buffer-file-name))
;; End:
