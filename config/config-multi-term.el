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

(eval-after-load "multi-term"
                 '(setq multi-term-program "/bin/bash"
                   term-unbind-key-list '("C-x"
                                          "C-h"
                                          "M-x"
                                          "C-z")
                   ;; required to configure properly the environment
                   ;; with AnSiT? variables in .bashrc
                   ;; TERM is restored to xterm-256-color after that.
                   term-term-name "xterm-256color"))

(add-hook 'term-mode-hook
          (lambda ()
            ;; term should use my own face
            (copy-face 'default 'term-face)

            ;; Commentary section in term.el
            (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
            (make-local-variable 'mouse-yank-at-point)
            (make-local-variable 'transient-mark-mode)
            (setq mouse-yank-at-point t
                  transient-mark-mode nil)
            (auto-fill-mode -1)

            ;; awesome bindings available!
            (compilation-shell-minor-mode t)))

(cl-flet ((set-color (pair)
                  (multiple-value-bind (face color)
                      pair
                    (set-face-attribute face
                                        nil
                                        :foreground color
                                        :background nil))))
  (mapc #'set-color
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

(defadvice term-send-input (after update-current-directory)
  "Update the current directory."
  (let* ((pid (process-id (get-buffer-process (current-buffer))))
         (cwd (file-truename (format "/proc/%d/cwd" pid))))
    (cd cwd)))

(ad-activate 'term-send-input)

(defadvice term-send-raw (after update-current-directory)
  "Update the current directory."
  (let* ((pid (process-id (get-buffer-process (current-buffer))))
         (cwd (file-truename (format "/proc/%d/cwd" pid))))
    (cd cwd)))

(ad-activate 'term-send-raw)


(provide 'config-multi-term)
