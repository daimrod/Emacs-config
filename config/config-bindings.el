;; config-bindings.el
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
(require 'key-chord)

;; Align your code in a pretty way.
(global-set-key (kbd "C-x \\") 'align-regexp)

;; Completion that uses many different methods to find options.
(global-set-key (kbd "M-/") 'dabbrev-expand)

;; Perform general cleanup.
(global-set-key (kbd "C-c n") 'cleanup-buffer)

;; Use regex searches by default.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Imenu
(global-set-key (kbd "C-x C-i") 'imenu)

;; File finding
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-x C-M-f") 'find-file-in-project)
(global-set-key (kbd "C-x f") 'recentf-ido-find-file)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Window switching. (C-x o goes to the next window)
(windmove-default-keybindings) ;; Shift+direction
(global-set-key (kbd "C-x C-o") (lambda () (interactive) (other-window -1))) ;; back one

;; use another version of zap-to-char (don't chopd the last char)
(global-set-key (kbd "M-z") 'zap-to-char-)
(global-set-key (kbd "M-Z") 'zap-to-char)

;; Move in window
(global-set-key (kbd "C-c <") 'move-to-window-line-top)
(global-set-key (kbd "C-c >") 'move-to-window-line-bottom)

;; quickmove between mark without disturbing transient-mark-mode
(global-set-key (kbd "C-`") 'push-mark-no-activate)
(global-set-key (kbd "M-`") 'jump-to-mark)

;; scroll up and down slowly by default (one line at time)
(global-set-key (kbd "C-v") 'dmd/small-scroll-up-command)
(global-set-key (kbd "M-v") 'dmd/small-scroll-down-command)
(global-set-key (kbd "C-S-v") 'scroll-up-command)
(global-set-key (kbd "M-V") 'scroll-down-command)

;; remove C-x C-z
(global-unset-key (kbd "C-x C-z"))

;; never quit emacs daemon
(global-set-key (kbd "C-x C-c") 'dmd/quit-or-hide)

;; manage url at point
(require 'graze-url nil t)
(eval-after-load "graze-url"
  '(progn
     (global-set-key (kbd "C-c y") 'gu-copy-url-at-point)
     (global-set-key (kbd "C-c b") 'gu-browse-url)
     (global-set-key (kbd "C-c w s") 'gu-search)))

;; pretty print last s-exp
(global-set-key (kbd "C-x M-e") 'pp-eval-last-sexp)

;; iy-go-to-char configuration
(global-set-key (kbd "C-c f") 'iy-go-to-char)
(global-set-key (kbd "C-c F") 'iy-go-to-char-backward)
(global-set-key (kbd "C-c ;") 'iy-go-to-char-continue)
(global-set-key (kbd "C-c ,") 'iy-go-to-char-continue-backward)

(eval-after-load "config-quiet"
  '(global-set-key (kbd "C-c q") 'quiet-mode))

(global-set-key (kbd "C-c u") 'winner-undo)
(global-set-key (kbd "C-c r") 'winner-redo)

(global-set-key [f11] 'toggle-fullscreen)

;;; C-x 4 t is for multi-term in another window, so a terminal in
;;; another frame should be an xterm or similar.
(global-set-key (kbd "C-x 5 t") 'dmd/terminal-emulator)

(key-chord-define-global "QP" 'ace-jump-word-mode)
(key-chord-define-global "QO" 'ace-jump-char-mode)
(key-chord-define-global "QL" 'ace-jump-line-mode)
(key-chord-define-global "QD" 'ace-jump-mode-pop-mark)

(global-set-key (kbd "M-Q") 'unfill-paragraph)

(require 'compile-cache)
(global-set-key (kbd "<f5>") 'compile-cache)
(global-set-key (kbd "<f6>") 'recompile)

(global-set-key (kbd "M-\\") 'execute-extended-command)

(global-set-key (kbd "C-x k") 'dmd/ido-kill-buffer)

;;; EMMS
(global-set-key (kbd "M-<f10>") 'emms-previous)
(global-set-key (kbd "M-<f11>")
                '(lambda (&optional prefix)
                   "Pause current track if PREFIX is nil,
otherwise stop it."
                   (interactive "P")
                   (if prefix
                       (progn (emms-stop)
                              (message "Stopped"))
                     (emms-pause)
                     (message "Paused"))))
(global-set-key (kbd "M-<f12>")
                '(lambda () (interactive)
                   (if emms-random-playlist
                       (emms-random)
                     (emms-next))))

(global-set-key (kbd "C-;") 'newline-and-indent)

;; M-x
(global-set-key (kbd "C-c m") 'execute-extended-command)
(global-set-key (kbd "C-c C-m") 'execute-extended-command)

(require 'dired)
(define-key dired-mode-map (kbd "e") 'dmd/dired-exec-lisp)

(require 'comint)
(define-key comint-mode-map (kbd "C-c C-l") 'dmd/comint-truncate-buffer)

(require 'doc-view)
(define-key doc-view-mode-map (kbd "C-c C-i") 'dmd/doc-view-info)
(define-key doc-view-mode-map (kbd "C-c C-v") 'dmd/doc-view-external)

(provide 'config-bindings)