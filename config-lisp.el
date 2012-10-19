;; config-lisp.el
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

(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code."
  t)
(add-hook 'lisp-mode-hook 'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)

(require 'elisp-slime-nav)
(add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)
(add-hook 'inferior-emacs-lisp-mode-hook 'elisp-slime-nav-mode)

;;; higlight-sexp configuration
(fni/add-to-load-path (concat src-dir "highlight-sexp/"))
(require 'highlight-sexp)
(add-hook 'lisp-mode-hook 'highlight-sexp-mode)
(add-hook 'emacs-lisp-mode-hook 'highlight-sexp-mode)

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;;; redshank configuration
(fni/add-to-load-path (concat src-dir "redshank/"))
(require 'redshank-loader)
(eval-after-load "redshank-loader"
  (redshank-setup '(lisp-mode-hook
                    slime-repl-mode-hook) t))

(setq lisp-lambda-list-keyword-alignment t
      lisp-lambda-list-keyword-parameter-alignment t)

;;; auto-compile elisp files
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (add-hook 'after-save-hook 'dmd/autocompile nil t)))

(provide 'config-lisp)
