(require 'orgalist)

(add-hook 'message-mode-hook (lambda () (orgalist-mode 1)))
