(require 'org)
(require 'subr-x)
(require 'org-contacts)
(require 'org-clock)
(require 'org-habit)
(require 'org-agenda)
(require 'org-id)
(require 'org-attach)
(require 'org-agenda)
(require 'dmd-org-mode)
(require 'diary-lib)
(require 'ox-beamer)
(require 'helm-org)

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

(setq org-element-use-cache t)

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
        ("p" "New planning entry" entry
         (file+weektree "~/org/planning.org")
         "* TODO %?%a" :empty-lines 1)
        ("J" "New journal entry in current buffer" entry
         (function dmd--org-capture-weektree)
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
(define-key org-mode-map (kbd "C-c C-S-o") 'org-mark-ring-goto)

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

;; (org-babel-lob-ingest (expand-file-name "org-mode/doc/library-of-babel.org" modules-dir))
(org-babel-lob-ingest (expand-file-name "lob.org" user-emacs-directory))

;; message-mode
(add-hook 'message-mode-hook 'turn-on-orgstruct)
(add-hook 'message-mode-hook 'turn-on-orgstruct++)
(add-hook 'message-mode-hook 'turn-on-orgtbl)

;; Always clocking ! Always !
;; (defvar dmd-always-clocking-timer (run-at-time 't 30 'dmd-always-clocking-check))

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
            (add-hook 'before-save-hook 'org-update-parent-todo-statistics nil 'local)))

(setq helm-bibtex-bibliography bibtex-files)
(setq reftex-default-bibliography bibtex-files)


(advice-add #'org-check-agenda-file  :override #'dmd-org-check-agenda-file)
