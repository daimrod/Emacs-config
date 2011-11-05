;;;; config-org.el

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
