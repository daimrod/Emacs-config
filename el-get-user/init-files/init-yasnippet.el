(yas-global-mode 1)
(require 'company-yasnippet)
(define-key yas-minor-mode-map (kbd "C-c & C-s") 'company-yasnippet)
(global-set-key (kbd "M-S-c") 'company-yasnippet)
