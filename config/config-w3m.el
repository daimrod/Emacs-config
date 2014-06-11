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

(setf w3m-init-file (concat dotfiles-dir "config-w3m"))

(cl-flet ((w3m-add-search-engine-with-quickshort
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

(setf w3m-command-arguments (list "-o" (concat "http_proxy=" (getenv "http_proxy"))))

(let ((map w3m-mode-map))
  (mapc
   (lambda (pair)
     (destructuring-bind (key &rest def)
         pair
       (define-key map (read-kbd-macro key) def)))
   `(("g" . dmd/w3m-browse-url)
     ("q" . w3m-delete-buffer)
     ("n" . w3m-next-buffer)
     ("p" . w3m-previous-buffer)
     ("G" . dmd/w3m-browse-url-new-session)
     ("F" . w3m-view-next-page)))
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
