(defvar eclim-mode-map)
(define-key eclim-mode-map (kbd "C-c C-e p r") 'eclim-run-class)
(add-hook 'eclim-mode-hook 'company-emacs-eclim-setup)
(add-hook 'java-mode-hook 'eclim-mode)

