;; config-org.el
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

(defvar org-dir (concat src-dir "org-mode/"))
(fni/add-to-load-path org-dir t t)
(add-to-list 'Info-directory-list
             (expand-file-name (concat org-dir
                                       "doc/")))

(require 'org)
(require 'org-latex)
(require 'org-beamer)
(require 'org-list)
(require 'org-drill)
;;; babel requirements
(require 'ob-asymptote)
(require 'ob-awk)
(require 'ob-calc)
(require 'ob-C)
(require 'ob-clojure)
(require 'ob-css)
(require 'ob-ditaa)
(require 'ob-dot)
(require 'ob-emacs-lisp)
(require 'ob-gnuplot)
(require 'ob-haskell)
(require 'ob-java)
(require 'ob-js)
(require 'ob-latex)
(require 'ob-ledger)
(require 'ob-lisp)
(require 'ob-lilypond)
(require 'ob-matlab)
(require 'ob-mscgen)
(require 'ob-ocaml)
(require 'ob-octave)
(require 'ob-org)
(require 'ob-perl)
(require 'ob-python)
(require 'ob-R)
(require 'ob-ruby)
(require 'ob-sass)
(require 'ob-scheme)
(require 'ob-screen)
(require 'ob-sh)
(require 'ob-sql)
(require 'ob-sqlite)

;; Subcommands for org global keymap
(define-prefix-command 'mode-specific-org-map)
(define-key mode-specific-map (kbd "o") 'mode-specific-org-map)

;; define where org file should be located by default
(setq org-directory "~/org/")

;; The following lines are always needed. Choose your own keys.
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(define-key mode-specific-org-map (kbd "l") 'org-store-link)
(define-key mode-specific-org-map (kbd "a") 'org-agenda)

;; capture
(define-key mode-specific-org-map (kbd "c") 'org-capture)

;; define where notes should be stored (for capture with C-c c)
(setq-default org-default-notes-file (concat org-directory "capture.org"))

;; start the overview on the current day
(setq-default org-agenda-start-on-weekday nil)

;; Append the new note at the beginning
(setq-default org-reverse-note-order t)

;; enable silent org-mode within mail
(add-hook 'mail-mode-hook 'turn-on-orgstruct)
(add-hook 'mail-mode-hook 'turn-on-orgstruct++)

;; customize TODO keywords
(setq org-todo-keywords
      '((sequence "TODO(t@)" "STARTED(s!/@)" "WAIT(w@/!)" "|" "DONE(d@/!)" "CANCELLED(c@/!)" "DEFERRED(e@/!)")
        (sequence "REPORT(r@)" "BUG(b!/@)" "KNOWNCAUSE(k!/@)" "|" "FIXED(f@/!)" "CANCELLED(c@/!)")
        (sequence "TOREAD(o@)" "|" "READ(a!/@)" "CANCELLED(c@/!)")
        (sequence "MEMO(m@)")))

;; fontify src code
(setq org-src-fontify-natively t)

;;; Calendar/Diary
(setq org-agenda-include-diary t)
(require 'calendar)
(require 'diary-lib)

(setq diary-file "~/.diary"
      org-agenda-diary-file "~/org/diary.org")
(add-hook 'diary-list-entries-hook 'diary-include-other-diary-files)
(add-hook 'diary-mark-entries-hook 'diary-mark-included-diary-files)

;;; export Latex
(add-to-list 'org-export-latex-default-packages-alist '("" "listings" t))

;;; config org-annotate (contrib)
(require 'org-annotate-file)
(setq org-annotate-file-storage-file (concat org-directory "annotated.org"))
(define-key mode-specific-org-map (kbd "n") 'org-annotate-file)

;;; Org Sync
(fni/add-to-load-path (concat src-dir "org-sync/"))
(require 'os)
(require 'os-bb)
(require 'os-github)
(require 'os-rmine)

;;; Org Contacts
(require 'org-contacts)

;;; Org Magit
(fni/add-to-load-path (concat src-dir "org-magit/"))
(require 'org-magit)

(provide 'config-org)
