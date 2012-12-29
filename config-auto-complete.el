;; config-auto-complete.el
;; Copyright (C) 2011, 2012 Grégoire Jadi

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

;;; Use the git version because the current version in ELPA is broken
;;; with Yasnippet
(fni/add-to-load-path (concat src-dir "auto-complete/") t)
(require 'auto-complete)
(require 'auto-complete-config)

(ac-config-default)
(add-to-list 'ac-dictionary-directories (concat dotfiles-dir "ac-dict/"))

(setq ac-use-quick-help t
      ac-quick-help-delay 1.5
      ac-auto-show-menu 1)

(define-key ac-mode-map (kbd "M-TAB") 'auto-complete)

(setq-default ac-sources
              '(ac-source-filename
                ac-source-files-in-current-dir
                ac-source-words-in-buffer
                ac-source-yasnippet
                ac-source-dictionary))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setf ac-sources
                  (append ac-sources
                          '(ac-source-features
                            ac-source-functions
                            ac-source-symbols
                            ac-source-variables)))))

(add-hook 'c-mode-hook (lambda ()
                         (add-to-list 'ac-sources
                                      'ac-source-semantic)))

(add-hook 'c++-mode-hook (lambda ()
                           (setf ac-sources
                                 (append ac-sources
                                         '(
                                           ;; Tooo slow
                                           ;; ac-source-semantic
                                           ac-source-words-in-same-mode-buffers
                                           ac-source-gtags)))))


(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))

(provide 'config-auto-complete)
