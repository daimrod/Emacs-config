;; Winner configuration
(with-eval-after-load 'winner
  (global-set-key (kbd "C-c u") 'winner-undo)
  (global-set-key (kbd "C-c r") 'winner-redo)
  (winner-mode 1))

