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

(setq org-reveal-root "file:///home/daimrod/packages/reveal/")

;; Subcommands for org global keymap
(define-prefix-command 'mode-specific-org-map)
(define-key mode-specific-map (kbd "o") 'mode-specific-org-map)

;; define where org file should be located by default
(setq org-directory "~/org/")

;; The following lines are always needed. Choose your own keys.
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(define-key mode-specific-org-map (kbd "l") 'org-store-link)
(define-key mode-specific-org-map (kbd "a") 'org-agenda)
(define-key mode-specific-org-map (kbd "g") 'org-clock-goto)
(define-key mode-specific-org-map (kbd "c") 'org-capture)

;; define where notes should be stored (for capture with C-c c)
(setq-default org-default-notes-file (concat org-directory "capture.org"))

;; start the overview on the current day
(setq-default org-agenda-start-on-weekday nil)

;; Append the new note at the beginning
(setq-default org-reverse-note-order t)

;; enable silent org-mode within mail
(add-hook 'message-mode-hook 'turn-on-orgstruct)
(add-hook 'message-mode-hook 'turn-on-orgstruct++)
(add-hook 'message-mode-hook 'turn-on-orgtbl)

;; fontify src code
(setq org-src-fontify-natively t)

;;; Calendar/Diary
(setq org-agenda-include-diary t)

(setq diary-file "~/.diary"
      org-agenda-diary-file "~/org/diary.org")
(add-hook 'diary-list-entries-hook 'diary-include-other-diary-files)
(add-hook 'diary-mark-entries-hook 'diary-mark-included-diary-files)

;;; export Latex
(setq org-latex-default-packages-alist nil
      org-latex-packages-alist
      '(("table" "xcolor" nil)
        ("AUTO" "inputenc" t)
        ("" "listings" t)
        ("" "minted" t)
        ("" "tikz" t)
        ("T1" "fontenc" t)
        ("" "fixltx2e" nil)
        ("" "graphicx" t)
        ("" "longtable" nil)
        ("" "float" nil)
        ("" "wrapfig" nil)
        ("normalem" "ulem" t)
        ("" "textcomp" t)
        ("" "marvosym" t)
        ("" "wasysym" t)
        ("" "latexsym" t)
        ("" "amssymb" t)
        ("" "amstext" nil)
        ("" "hyperref" nil)
        "\\tolerance=1000"))
(setq org-latex-create-formula-image-program 'imagemagick)
(setq org-latex-listings 'minted)

(add-to-list 'org-latex-minted-langs '(R "r"))

;;; config org-annotate (contrib)
(setq org-annotate-file-storage-file (concat org-directory "annotated.org"))
(define-key mode-specific-org-map (kbd "n") 'org-annotate-file)

;;; see (info "(org) Speed keys")
(setq org-use-speed-commands t)

;;; Don't scatter LaTeX images
(setq org-latex-preview-ltxpng-directory "/tmp/ltxpng/")
(make-directory org-latex-preview-ltxpng-directory t)

;; Make windmove work in org-mode:
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

(setq-default org-element-use-cache nil)

;; Org File Apps
(advice-add 'org-open-at-point :filter-args
              ;; Filter out any parameters
              (lambda (&optional arg reference-buffer)
                nil)
              '((name . org-open-at-point-del-args)))

(setf org-file-apps
      '(("\\.x?html?\\'" . (let ((dispatcher-prefix-arg current-prefix-arg)) (gu-browse-url file)))
        ("\\.pdf\\'" . (let ((dispatcher-prefix-arg current-prefix-arg)) (dmd/open-pdf file)))
        (t . emacs)))

;;; Nicolas Goaziou, http://article.gmane.org/gmane.emacs.orgmode/67692
(defun org-latex-ignore-heading-filter-headline (headline backend info)
  "Strip headline from HEADLINE. Ignore BACKEND and INFO."
  (when (and (org-export-derived-backend-p backend 'latex)
             (string-match "\\`.*ignoreheading.*\n" headline))
    (replace-match "" nil nil headline)))

(add-to-list 'org-export-filter-headline-functions
             'org-latex-ignore-heading-filter-headline)

(setq org-export-async-init-file (expand-file-name "init-org-async.el" dotfiles-dir))

(add-hook 'org-mode-hook 'dmd/org-mode-reftext-setup)

;; Prompt for a date for CAPTURE_TIME properties
(add-to-list 'org-property-set-functions-alist
             (cons "CAPTURE_TIME" '(lambda (prompt collection
                                                   &optional predicate require-match initial-input
                                                   hist def inherit-input-method)
                                     (format-time-string "[%Y-%m-%d %a]" (org-read-date nil 'totime nil prompt nil def nil)))))

(provide 'config-org)
