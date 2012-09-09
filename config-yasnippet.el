;; config-yasnippet.el
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

(fni/add-to-load-path (concat src-dir "yasnippet/"))

(require 'yasnippet)
(setq yas-snippet-dirs
      (list
       (concat dotfiles-dir "snippets/")
       (concat src-dir "yasnippet/snippets/")))

;; Load the snippets
(setq yas-trigger-key "M-TAB"
      yas-fallback-behavior 'call-other-command
      yas-prompt-functions '(yas-ido-prompt))

(yas-global-mode 1)

(provide 'config-yasnippet)
