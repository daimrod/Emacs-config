;; config-multi-term.el
;; Copyright (C) 2011 Grégoire Jadi

;; Author: Grégoire Jadi <gregoire.jadi@gmail.com>

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(autoload 'multi-term "multi-term" nil t)
(autoload 'multi-term-next "multi-term" nil t)

(eval-after-load "multi-term"
                 '(setq multi-term-program "/bin/bash"
                   term-unbind-key-list '("C-x" "C-h")))

(eval-after-load "multi-term"
                 '(setq multi-term-program "/bin/bash"
                   term-unbind-key-list '("C-x" "C-h")
                   term-bind-key-alist '(("C-p" . previous-line)
                                         ("C-n" . next-line)
                                         ("C-a" . move-beginning-of-line)
                                         ("C-c C-a" . term-bol)
                                         ("C-e" . move-end-of-line)
                                         ("C-SPC" . set-mark-command)
                                         ("C-w" . kill-region)
                                         ("M-w" . kill-ring-save)
                                         ("C-y" . yank)
                                         ("M-y" . yank-pop)
                                         ("C-s" . isearch-forward)
                                         ("C-r" . isearch-backward)
                                         ("M-f" . term-send-forward-word)
                                         ("M-b" . term-send-backward-word)
                                         ("M-p" . term-send-up)
                                         ("M-n" . term-send-down)
                                         ("M-r" . term-send-reverse-search-history)
                                         ("C-c C-c" . term-interrupt-subjob)
                                         ("C-c C-z" . term-stop-subjob))))

(custom-set-variables '(term-prompt-regexp "^.*\\$ *"))

(defadvice term-bol (around update-process-mark)
  (let ((old-pos (point)))
    ad-do-it
    (let ((new-pos (point)))
      (dotimes (i (- old-pos new-pos))
        (term-send-raw-string "")))))

(ad-activate 'term-bol)

(global-set-key (kbd "C-c t") 'multi-term-next)
(global-set-key (kbd "C-c T") 'multi-term)

(provide 'config-multi-term)
