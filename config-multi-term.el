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
                   term-unbind-key-list '("C-x"
                                          "C-h"
                                          "M-x"
                                          "C-z")
                   term-term-name "xterm-256color"))

(custom-set-variables '(term-prompt-regexp "^.*\\$ *"))

(setq-default ansi-color-names-vector
              [unspecified
               "#2e3434"        ; black
               "tomato"         ; red
               "#6ac214"        ; green
               "#edd400"        ; yellow
               "light sky blue" ; blue
               "magenta"        ; magenta
               "cyan"           ; cyan
               "#eeeeec"        ; white
               ])

(global-set-key (kbd "C-c t") 'multi-term-next)
(global-set-key (kbd "C-c T") 'multi-term)

(provide 'config-multi-term)
