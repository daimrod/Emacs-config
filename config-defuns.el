;;;; config-defuns.el

(require 'http-post-simple)
(require 'thingatpt)
(require 'imenu)

(defun ido-imenu ()
  "Update the imenu index and then use ido to select a symbol to navigate to.
Symbols matching the text at point are put first in the completion list."
  (interactive)
  (imenu--make-index-alist)
  (let ((name-and-pos '())
        (symbol-names '()))
    (flet ((addsymbols (symbol-list)
             (when (listp symbol-list)
               (dolist (symbol symbol-list)
                 (let ((name nil) (position nil))
                   (cond
                     ((and (listp symbol) (imenu--subalist-p symbol))
                      (addsymbols symbol))

                     ((listp symbol)
                      (setq name (car symbol))
                      (setq position (cdr symbol)))

                     ((stringp symbol)
                      (setq name symbol)
                      (setq position (get-text-property 1 'org-imenu-marker symbol))))

                   (unless (or (null position) (null name))
                     (add-to-list 'symbol-names name)
                     (add-to-list 'name-and-pos (cons name position))))))))
      (addsymbols imenu--index-alist))
    ;; If there are matching symbols at point, put them at the beginning of `symbol-names'.
    (let ((symbol-at-point (thing-at-point 'symbol)))
      (when symbol-at-point
        (let* ((regexp (concat (regexp-quote symbol-at-point) "$"))
               (matching-symbols (delq nil (mapcar (lambda (symbol)
                                                     (if (string-match regexp symbol) symbol))
                                                   symbol-names))))
          (when matching-symbols
            (sort matching-symbols (lambda (a b) (> (length a) (length b))))
            (mapc (lambda (symbol) (setq symbol-names (cons symbol (delete symbol symbol-names))))
                  matching-symbols)))))
    (let* ((selected-symbol (ido-completing-read "Symbol? " symbol-names))
           (position (cdr (assoc selected-symbol name-and-pos))))
      (goto-char position))))

(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(defun sudo-edit (&optional arg)
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo::" (ido-read-file-name "File: ")))
      (find-alternate-file (concat "/sudo::" buffer-file-name))))

(defun sbrk-paste ()
  "paste a chunk of code to pastebin.sbrk.org"
  (interactive)
  (let* ((region-active-p (region-active-p))
         (start (if region-active-p (region-beginning) (buffer-end -1)))
         (end (if region-active-p (region-end) (buffer-end 1))))
    (http-post-simple "http://pastebin.sbrk.org/add"
                      `((author ,(getenv "USER"))
                        (code ,(buffer-substring start end))))))

(defun zap-to-char- (arg char)
  "Just like zap-to-char- but do not chop the last CHAR."
  (interactive "p\ncZap to char-: ")
  ;; Avoid "obsolete warnings for translation-table-for-input.
  (with-no-warnings
      (if (char-table-p translation-table-for-input)
          (setq char (or (aref translation-table-for-input char) char))))
  (save-excursion
   (kill-region (point)
                (progn
                  (search-forward (char-to-string char) nil nil arg)
                  (if (>= arg 0)
                      (- (point) 1)
                      (+ (point) 1))))))

(defun recompile-config ()
  (interactive)
  (map nil #'byte-compile-file
       '("~/.gnus.el"
         "~/.emacs-w3m.el"))
  (byte-recompile-directory dotfiles-dir 0 t))

(defun move-to-window-line-top ()
  (interactive)
  (move-to-window-line 0))

(defun move-to-window-line-bottom ()
  (interactive)
  (move-to-window-line -1))

(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))

(defun small-scroll-up-command (&optional arg)
  (interactive "^P")
  (if arg
      (scroll-up-command arg)
      (scroll-up-command 5)))

(defun small-scroll-down-command (&optional arg)
  (interactive "^P")
  (if arg
      (scroll-down-command arg)
      (scroll-down-command 5)))

(defun show-big-text (text &optional size font)
  (interactive "sText to show: ")
  (let ((size (number-to-string
               (if (null size)
                   (window-width)
                   size)))
        (font (if (null font)
                  "doh"
                  font)))
   (shell-command (format "figlet -w %s -f %s %s"
                          size font text))))

(provide 'config-defuns)
