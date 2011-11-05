;; config-org.el
;; Copyright (C) 2011 Grégoire Jadi

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

(require 'org-install)

;; Subcommands for org global keymap
(define-prefix-command 'mode-specific-org-map)
(define-key mode-specific-map (kbd "o") 'mode-specific-org-map)

;; define where org file should be located by default
(setq org-directory "~/org/")

;; The following lines are always needed. Choose your own keys.
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(define-key mode-specific-org-map (kbd "l") 'org-store-link)
(define-key mode-specific-org-map (kbd "a") 'org-agenda)
(define-key mode-specific-org-map (kbd "b") 'org-iswitchb)

;; capture
(define-key mode-specific-org-map (kbd "c") 'org-capture)

;; define where notes should be stored (for capture with C-c c)
(setq-default org-default-notes-file (concat org-directory "notes.org"))

;; start the overview on the current day
(setq-default org-agenda-start-on-weekday nil)

;; Append the new note at the beginning
(setq-default org-reverse-note-order t)

;; enable silent org-mode within mail
(add-hook 'mail-mode-hook 'turn-on-orgstruct)
(add-hook 'mail-mode-hook 'turn-on-orgstruct++)

;; customize TODO keywords
(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s@/!)" "WAIT(w@/!)" "|" "DONE(d@/!)" "CANCELED(c@/!)" "DEFERRED(d@/!)")
        (sequence "REPORT(r@/!)" "BUG(b@/!)" "KNOWNCAUSE(k@/!)" "|" "FIXED(f@/!)" "CANCELED(c@/!)")))

(provide 'config-org)
