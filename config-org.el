;;;; config-org.el

(require 'org-install)

;; define where org file should be located by default
(setq org-directory "~/org/")

;; The following lines are always needed. Choose your own keys.
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)

;; enable silent org-mode within mail
(add-hook 'mail-mode-hook 'turn-on-orgstruct)
(add-hook 'mail-mode-hook 'turn-on-orgstruct++)

(provide 'config-org)
