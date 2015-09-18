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

(defcustom src-dir (expand-file-name "src/elisp/" (getenv "HOME"))
  "The source directory where third-part modules are located."
  :group 'dmd/config)

(add-to-list 'load-path config-dir)

;; custom-file configuration
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(eval-and-compile
  (require 'bytecomp)
  (byte-compile-disable-warning 'cl-functions)
  (require 'cl))

;; Load my configuration
(defvar dmd/modules
  (cl-loop for config-file in (directory-files config-dir nil "^config-.*.el$")
           collect (intern (subseq config-file 0 (- (length config-file) 3))))
  "List of available configuration modules.")

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
    gnus
    gnus-msg
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


(add-hook 'after-init-hook
          (lambda ()
            ;; This needs to be done in `after-init-hook' to override
            ;; packages provided by ELPA.
            (fni/add-to-load-path src-dir t t)

            (load (expand-file-name "~/quicklisp/slime-helper.el") t)
            (load (expand-file-name "~/quicklisp/clhs-use-local.el") t)
            (pdf-tools-install)
            (elpy-enable)

            (mapc (lambda (module)
                    (message "Loading %s" module)
                    ;; (require module)
                    (unless (ignore-errors (require module))
                      (warn "Failed to load module `%s'" module)))
                  (append dmd/required dmd/modules))
            (diary-list-entries (calendar-current-date) nil 'list-only)
            (mapc (lambda (file)
                    (bury-buffer (find-file-noselect file))) (append diary-included-files
                                                                     (org-agenda-files)))))


;; enabled/disabled commands
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)

;; Init stuff
(setf inhibit-startup-screen t)

;; Local Variables:
;; firestarter: (byte-recompile-file (buffer-file-name))
;; End:

(provide 'init)

;;; init.el ends here
