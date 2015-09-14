;;; config-bindings.el --- My bindings
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

;;; Commentary:

;;; Code:

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

;; remove C-x C-c
(global-unset-key (kbd "C-x C-c"))

;; manage url at point
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
(global-set-key (kbd "C-x 5 t") 'dmd-terminal-emulator)

(define-key global-map (kbd "M-s") 'ace-map)

(defvar ace-map)
(define-prefix-command 'ace-map)
(define-key ace-map (kbd "s") 'ace-jump-word-mode)
(define-key ace-map (kbd "l") 'ace-jump-line-mode)
(define-key ace-map (kbd "c") 'ace-jump-char-mode)
(define-key ace-map (kbd "p") 'ace-jump-mode-pop-mark)
(define-key ace-map (kbd "w") 'ace-window)

(global-set-key (kbd "M-Q") 'unfill-paragraph)

(global-set-key (kbd "<f5>") 'compile-cache)
(global-set-key (kbd "<f6>") 'recompile)

(global-set-key (kbd "M-\\") 'execute-extended-command)

;; M-x
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-c m") 'helm-M-x)
(global-set-key (kbd "C-c C-m") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-buffers-list)

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

(define-key dired-mode-map (kbd "e") 'dmd/dired-exec-lisp)

(define-key comint-mode-map (kbd "C-c C-l") 'dmd/comint-truncate-buffer)

(define-key doc-view-mode-map (kbd "C-c C-i") 'dmd/doc-view-info)
(define-key doc-view-mode-map (kbd "C-c C-v") 'dmd/doc-view-external)

(define-key hs-minor-mode-map (kbd "C-c -") 'hs-hide-block)
(define-key hs-minor-mode-map (kbd "C-c _") 'hs-hide-all)
(define-key hs-minor-mode-map (kbd "C-c =") 'hs-show-block)
(define-key hs-minor-mode-map (kbd "C-c +") 'hs-show-all)

(define-key mode-specific-map (kbd "e") 'ebib)

(global-set-key (kbd "C-SPC") 'company-complete)
(global-set-key (kbd "M-C") 'company-yasnippet)

(define-key mode-specific-map (kbd "g") 'magit-status)

(define-key java-mode-map (kbd "C-h j") 'javadoc-lookup)
(define-key eclim-mode-map (kbd "C-c C-e p r") 'eclim-run-class)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c C->") 'mc/mark-all-in-region)

(global-set-key (kbd "C-c t") 'multi-term-next)
(global-set-key (kbd "C-c T") 'multi-term)
(global-set-key (kbd "C-x 4 t") 'multi-term-dedicated-toggle)

(define-key projectile-mode-map (kbd "C-c p a") 'projectile-ag)

(global-set-key [f10] 'ediprolog-dwim)

(define-prefix-command 'moccur-map)
(define-key global-map (kbd "M-o") 'moccur-map)

(define-key moccur-map (kbd "s") 'occur-by-moccur)
(define-key moccur-map (kbd "m") 'moccur)
(define-key moccur-map (kbd "d") 'dmoccur)

(define-key dired-mode-map (kbd "M-o") 'moccur-map)

(global-set-key (kbd "s-f") 'god-mode-all)
(global-set-key (kbd "s-c") 'god-local-mode)

(define-key org-mode-map (kbd "C-c )") 'helm-bibtex)

(define-key message-mode-map (kbd "C-c C-c") nil)

(define-key org-agenda-mode-map (kbd "x") nil)

(define-key bibtex-mode-map (kbd "C-c C-o") 'dmd-bibtex-open)

(define-key gnus-summary-mode-map (kbd "i") (kbd "L S"))
(define-key gnus-summary-mode-map (kbd "y") (kbd "I S"))

(global-set-key (kbd "C-x #") 'delete-frame)

(define-key org-beamer-mode-map (kbd "C-c C-b") nil)

(define-key org-mode-map (kbd "C-c j") (lambda (&optional prefix)
                                           (interactive "P")
                                           (if prefix
                                               (helm-org-agenda-files-headings)
                                             (helm-org-in-buffer-headings))))

(global-set-key (kbd "<f9>") 'org-agenda)

(define-key yas-minor-mode-map (kbd "C-c & C-s") 'company-yasnippet)

(global-set-key (kbd "C-c j") 'helm-pages)

(define-key mu4e-view-mode-map (kbd "<tab>") 'shr-next-link)
(define-key mu4e-view-mode-map (kbd "<backtab>") 'shr-previous-link)

(define-key org-mode-map (kbd "C-c >") 'org-time-stamp-inactive)

(provide 'config-bindings)

;;; config-bindings.el ends here
