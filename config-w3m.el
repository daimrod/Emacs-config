;;; config-w3m.el ---

;; Copyright (C) 2012 Grégoire Jadi

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

(fni/add-to-load-path (concat src-dir "emacs-w3m/") t t)
(require 'w3m-load)
(require 'w3m)
(require 'w3m-search)
(require 'browse-url)

(setq w3m-init-file (concat dotfiles-dir "config-w3m"))

(flet ((w3m-add-search-engine-with-quickshort
        (search-engine)
        (let ((name (elt search-engine 0))
              (url (elt search-engine 1)))
          (add-to-list 'w3m-search-engine-alist
                       (list name url))
          (add-to-list 'w3m-uri-replace-alist
                       (list (concat "\\`" name ":")
                             'w3m-search-uri-replace
                             name)))))
  (mapc #'w3m-add-search-engine-with-quickshort
        '(("enfr" "http://www.wordreference.com/enfr/%s")
          ("fren" "http://www.wordreference.com/fren/%s")
          ("seeks" "http://s.s/search?q=%s"))))

(setf w3m-command-arguments '("-o" "http_proxy=http://localhost:8118/"))

(let ((map (make-keymap)))
  (suppress-keymap map)
  (mapc
   (lambda (pair)
     (destructuring-bind (key &rest def)
         pair
       (define-key map (read-kbd-macro key) def)))
   `(("B"               . w3m-view-previous-page)
     ("F"               . w3m-view-next-page)
     ("RET"             . w3m-view-this-url)
     ("<S-return>"      . w3m-view-this-url-new-session)
     ("C-c C-w"         . w3m-delete-buffer)
     ("s"               . w3m-search)
     ("S"               . w3m-search-new-session)
     ("h"               . w3m-history)
     ("H"               . w3m-session-select)
     ("a"               . w3m-bookmark-add-current-url)
     ("g"               . dmd/w3m-browse-url)
     ("G"               . dmd/w3m-browse-url-new-session)
     ("C-c i s"           . w3m-save-image)
     ("d"               . w3m-download-this-url)
     ("t"               . w3m-toggle-inline-image)
     ("T"               . w3m-toggle-inline-images)
     ("v"               . w3m-bookmark-view)
     ("V"               . w3m-bookmark-view-new-session)
     ("C-c C-f"         . w3m-shift-left)
     ("C-c C-b"         . w3m-shift-right)
     ("C-c l"           . w3m-horizontal-recenter)
     ("C-c C-c"         . w3m-submit-form)
     ("C-c C-l"         . w3m-go-to-linknum)
     ("C-c C-k"         . w3m-process-stop)
     ("C-c C-n"         . w3m-next-buffer)
     ("C-c C-p"         . w3m-previous-buffer)
     ("C-c N"           . w3m-tab-move-right)
     ("C-c P"           . w3m-tab-move-left)
     ("C-c C-t"         . w3m-copy-buffer)
     ("Q"               . w3m-quit)
     ("SPC"             . w3m-scroll-up-or-next-url)
     ("DEL"             . w3m-scroll-down-or-previous-url)
     ("TAB"             . w3m-next-anchor)
     ("<backtab>"       . w3m-previous-anchor)
     ("E"               . w3m-bookmark-edit)
     ("r"               . w3m-reload-this-page)
     ("R"               . w3m-reload-all-pages)
     ("z i"             . w3m-zoom-in-image)
     ("z o"             . w3m-zoom-out-image)))
  (setf w3m-mode-map map))

(defun dmd/w3m-browse-url (url prefix)
  "Ask emacs-w3m to browse URL."
  (interactive
   (progn
     (browse-url-interactive-arg "URL: ")))
  (when (stringp url)
    (w3m-goto-url (w3m-canonicalize-url url))))

(defun dmd/w3m-browse-url-new-session (url prefix)
  "Ask emacs-w3m to browse URL."
  (interactive
   (progn
     (browse-url-interactive-arg "URL: ")))
  (when (stringp url)
    (w3m-goto-url-new-session (w3m-canonicalize-url url))))

(defun dmd/switch-color-frame ()
  "Switch the color between the foreground and the background of
he current frame."
  (interactive)
  (let ((org-bg (face-attribute 'default :background))
        (org-fg (face-attribute 'default :foreground)))
    (set-face-background 'default org-fg (selected-frame))
    (set-face-foreground 'default org-bg (selected-frame))))

(provide 'config-w3m)

;;; config-w3m.el ends here
