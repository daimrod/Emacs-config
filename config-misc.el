;;;; config-misc.el --- Misc configuration

(require 'linum)

;; line number everywhere
(global-linum-mode 1)

;; answer by y and n instead of yes and no
(defalias 'yes-or-no-p 'y-or-n-p)

;; every backup files'll store in ~/backup directory
(setq backup-directory-alist
      '(("." . "~/backup/")))
(setq backup-by-copying t)

;; use x-clipboard
(setq x-select-enable-clipboard t)

;; utf-8 roxx
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(ansi-color-for-comint-mode-on)

;; some configuration (C-h v)
(setq require-final-newline t
      uniquify-buffer-name-style 'forward
      fringe-mode (cons 4 0)
      save-place-file (concat dotfiles-dir "places"))

;; Save a list of recent files visited.
(recentf-mode 1)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; enable ido-mode
(ido-mode t)
(setq ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-max-prospects 10)

(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; workgroups configuration
(workgroups-mode)
(setq wg-morph-on nil)

(setq browse-url-browser-function 'w3m-browse-url)
(global-set-key "\C-xm" 'browse-url-at-point)

;; markdown configuration
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(setq auto-mode-alist
   (cons '("\\.md" . markdown-mode) auto-mode-alist))

(provide 'config-misc)
