;;; config-org.el --- Org Mode's configuration

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


;; Subcommands for org global keymap
(define-prefix-command 'mode-specific-org-map)
(define-key mode-specific-map (kbd "o") 'mode-specific-org-map)

;; The following lines are always needed. Choose your own keys.
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(define-key mode-specific-org-map (kbd "l") 'org-store-link)
(define-key mode-specific-org-map (kbd "a") 'org-agenda)
(define-key mode-specific-org-map (kbd "g") 'org-clock-goto)
(define-key mode-specific-org-map (kbd "c") 'org-capture)

;; enable silent org-mode within mail
(add-hook 'message-mode-hook 'turn-on-orgstruct)
(add-hook 'message-mode-hook 'turn-on-orgstruct++)
(add-hook 'message-mode-hook 'turn-on-orgtbl)

;;; config org-annotate (contrib)
(define-key mode-specific-org-map (kbd "n") 'org-annotate-file)

;;; Don't scatter LaTeX images
(make-directory org-latex-preview-ltxpng-directory t)

;; Make windmove work in org-mode:
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

(setq-default org-element-use-cache nil)

;;; Nicolas Goaziou, http://article.gmane.org/gmane.emacs.orgmode/67692
(defun org-latex-ignore-heading-filter-headline (headline backend info)
  "Strip headline from HEADLINE.  Ignore BACKEND and INFO."
  (when (and (org-export-derived-backend-p backend 'latex)
             (string-match "\\`.*ignoreheading.*\n" headline))
    (replace-match "" nil nil headline)))

(add-to-list 'org-export-filter-headline-functions
             'org-latex-ignore-heading-filter-headline)

(setq org-export-async-init-file (expand-file-name "init-org-async.el" user-emacs-directory))

(add-hook 'org-mode-hook 'dmd-org-mode-reftex-setup)

;; Prompt for a date for CREATED properties
(add-to-list 'org-property-set-functions-alist
             (cons "CREATED" '(lambda (prompt collection
                                                   &optional predicate require-match initial-input
                                                   hist def inherit-input-method)
                                     (format-time-string "[%Y-%m-%d %a %H:%M]" (org-read-date nil 'totime nil prompt nil def nil)))))

(org-add-link-type "http" 'gu-browse-url)

(add-hook 'org-store-link-functions 'dmd--org-link-to-named-block)

(add-hook 'org-after-refile-insert-hook 'basic-save-buffer)

(add-hook 'org-mode-hook (lambda ()
                           (add-hook 'before-save-hook 'dmd-org-add-ids-to-headlines nil 'local)
                           (add-hook 'before-save-hook 'dmd-org-add-CREATED-to-headlines nil 'local)
                           (add-hook 'before-save-hook 'org-update-parent-todo-statistics nil 'local)))

(add-hook 'org-clock-in-prepare-hook 'dmd-org-set-effort)

(advice-add #'org-attach-open :override #'helm-org-attach-open)

(org-babel-lob-ingest (expand-file-name "lob.org" user-emacs-directory))


(provide 'config-org)

;;; config-org.el ends here
