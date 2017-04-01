(require 'god-mode)

(global-set-key (kbd "<escape>") 'god-local-mode)

(setq god-exempt-major-modes nil)
(setq god-exempt-predicates nil)

(setq s-mode-line-fg "#073642"
      s-mode-line-bg "#839496"
      s-mode-line-god-mode-fg "#002b36"
      s-mode-line-god-mode-bg "#eee8d5"
      s-mode-line-inactive-fg "#586e75"
      s-mode-line-inactive-bg "#002b36")

(defun dmd/god-mode-update-cursor ()
  (let ((limited-colors-p (> 257 (length (defined-colors)))))
    (cond (god-local-mode
           (set-face-foreground 'mode-line s-mode-line-god-mode-fg)
           (set-face-background 'mode-line s-mode-line-god-mode-bg)
           (set-face-foreground 'mode-line-inactive s-mode-line-inactive-fg)
           (set-face-background 'mode-line-inactive s-mode-line-inactive-bg))
          (t
           (set-face-foreground 'mode-line s-mode-line-fg)
           (set-face-background 'mode-line s-mode-line-bg)
           (set-face-foreground 'mode-line-inactive s-mode-line-inactive-fg)
           (set-face-background 'mode-line-inactive s-mode-line-inactive-bg))))))

(add-hook 'god-mode-enabled-hook 'dmd/god-mode-update-cursor)
(add-hook 'god-mode-disabled-hook 'dmd/god-mode-update-cursor)
(add-hook 'window-configuration-change-hook 'dmd/god-mode-update-cursor)
