;; init.el
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

(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))


;; Load path
(defun fni/add-to-load-path (this-directory &optional with-subdirs recursive)
  "Add THIS-DIRECTORY at the beginning of the load-path, if it exists.
Add all its subdirectories not starting with a '.' if the
optional argument WITH-SUBDIRS is not nil.
Do it recursively if the third argument is not nil."
  (when (and this-directory
             (file-directory-p this-directory))
    (let* ((this-directory (expand-file-name this-directory))
           (files (directory-files this-directory t "^[^\\.]")))

      ;; completely canonicalize the directory name (*may not* begin with `~')
      (while (not (string= this-directory (expand-file-name this-directory)))
             (setq this-directory (expand-file-name this-directory)))

      (message "Adding `%s' to load-path..." this-directory)
      (add-to-list 'load-path this-directory)

      (when with-subdirs
        (while files
               (let ((dir-or-file (car files)))
                 (when (file-directory-p dir-or-file)
                   (if recursive
                       (fni/add-to-load-path dir-or-file 'with-subdirs 'recursive)
                       (fni/add-to-load-path dir-or-file))))
               (setq files (cdr files)))))))

(defvar dotfiles-dir (file-name-directory
                      (or (buffer-file-name) load-file-name))
  ".emacs.d location.")
(defvar elisp-dir (concat dotfiles-dir "elisp/"))
(defvar elpa-dir (concat dotfiles-dir "elpa/"))
(defvar config-dir (concat dotfiles-dir "config/"))

(defcustom src-dir (concat (getenv "HOME") "/src/elisp/")
  "The source directory where third-part modules are located."
  :group 'dmd/config)

(fni/add-to-load-path elisp-dir t)
(fni/add-to-load-path config-dir)


;; custom-file configuration
(setq custom-file (concat dotfiles-dir "custom.el"))
(load custom-file 'noerror)


;; Misc configuration
(require 'bytecomp)
(byte-compile-disable-warning 'cl-functions)
(require 'cl)

;; Allow SIGUSR2 to "unfreeze" emacs when it blocks in jit-lock
;; (advice-add 'jit-lock--debug-fontify :around
;;             (lambda (fun &rest args)
;;               (with-local-quit (apply fun args))))




;;; Package.el
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; ELPA configuration
(setq package-archives
      '(("ELPA" . "https://elpa.gnu.org/packages/")
        ("marmalade" . "https://marmalade-repo.org/packages/")))

;; Load my configuration
(defvar dmd/modules
  (loop for config-file in (directory-files config-dir nil "^config-.*.el$")
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
    org-ebib
    ob-asymptote
    ob-awk
    ob-calc
    ob-C
    ob-clojure
    ob-css
    ob-ditaa
    ob-dot
    ob-emacs-lisp
    ob-gnuplot
    ob-haskell
    ob-java
    ob-js
    ob-latex
    ob-ledger
    ob-lisp
    ob-lilypond
    ob-matlab
    ob-mscgen
    ob-ocaml
    ob-octave
    ob-org
    ob-perl
    ob-python
    ob-R
    ob-ruby
    ob-sass
    ob-scheme
    ob-screen
    ob-sh
    ob-sql
    ob-sqlite
    calendar
    diary-lib
    org-annotate-file
    os
    os-bb
    os-github
    os-rmine
    org-contacts
    org-magit
    org-man
    org-feed
    org-habit
    plantuml-mode
    ob-plantuml
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
    request)
  "List of required modules")


(add-hook 'after-init-hook
          (lambda ()
            ;; This needs to be done in `after-init-hook' to override
            ;; packages provided by ELPA.
            (fni/add-to-load-path src-dir t t)

            (load (expand-file-name "~/quicklisp/slime-helper.el") t)
            (load (expand-file-name "~/quicklisp/clhs-use-local.el") t)
            (pdf-tools-install)

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
