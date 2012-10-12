;; config-multi-term.el
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

(require 'multi-term)

(eval-after-load "multi-term"
                 '(setq multi-term-program "/bin/bash"
                   term-unbind-key-list '("C-x"
                                          "C-h"
                                          "M-x"
                                          "C-z")
                   term-term-name "xterm-256color"))

(custom-set-variables '(term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

(flet ((set-color (pair)
                  (multiple-value-bind (face color)
                      pair
                    (set-face-attribute face
                                        nil
                                        :foreground color
                                        :background color))))
  (mapc 'set-color
        '((term-color-black "#2e3434")
          (term-color-red "tomato")
          (term-color-green "#6ac214")
          (term-color-yellow "#edd400")
          (term-color-blue "light sky blue")
          (term-color-magenta "magenta")
          (term-color-cyan "cyan")
          (term-color-white "#eeeeec"))))

(setq-default ansi-term-color-vector
              [term-face
               term-color-black
               term-color-red
               term-color-green
               term-color-yellow
               term-color-blue
               term-color-magenta
               term-color-cyan
               term-color-white
               ])

(global-set-key (kbd "C-c t") 'multi-term-next)
(global-set-key (kbd "C-c T") 'multi-term)
(global-set-key (kbd "C-x 4 t") 'multi-term-dedicated-toggle)

;;; autopair doesn't play well with term-mode
(add-hook 'term-mode-hook (lambda ()
                            (setq autopair-dont-activate t)
                            (autopair-mode -1)
                            (compilation-shell-minor-mode t)))

(provide 'config-multi-term)
