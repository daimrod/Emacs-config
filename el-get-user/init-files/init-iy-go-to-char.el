;; iy-go-to-char configuration
(with-eval-after-load 'iy-go-to-char
  (global-set-key (kbd "C-c f") 'riy-go-to-char)
  (global-set-key (kbd "C-c F") 'iy-go-to-char-backward)
  (global-set-key (kbd "C-c ;") 'iy-go-to-char-continue)
  (global-set-key (kbd "C-c ,") 'iy-go-to-char-continue-backward))

