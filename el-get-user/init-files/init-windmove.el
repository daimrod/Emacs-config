(require 'windmove)

(with-eval-after-load 'windmove
  (global-set-key (kbd "S-<up>") 'windmove-up)
  (global-set-key (kbd "S-<down>") 'windmove-down)
  (global-set-key (kbd "S-<right>") 'windmove-right)
  (global-set-key (kbd "S-<left>") 'windmove-left))

