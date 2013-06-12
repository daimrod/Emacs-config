;; config-cc-mode.el
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

(setq-default c-basic-offset 4
              tab-width 4 ; or any other preferred value
              cua-auto-tabify-rectangles nil
              compilation-window-height 10)

(setq c-default-style
      '((java-mode . "java")
        (awk-mode . "awk")
        (c-mode . "k&r")
        (other . "linux")))

(require 'gtk-look)
;; Fix gtk-look-init-cache
(defun gtk-lookup-cache-init ()
  "Initialize `gtk-lookup-cache', if not already done.
The return is the `gtk-lookup-cache' list."
  (when (eq gtk-lookup-cache 'uninitialized)
    ;; build in `result' and only after that set gtk-lookup-cache, so as not
    ;; to leave a half built cache if killed (C-g) part-way through
    (let ((result nil)
          (case-fold-search nil)
          (filelist
           ;; `file-truename' here and `remove' below will eliminate
           ;; any duplicate filenames arising from symlinks or repeat
           ;; matches of wildcards in gtk-lookup-devhelp-indices
           (sort (mapcar 'file-truename
                         (apply 'append
                                (mapcar 'file-expand-wildcards
                                        gtk-lookup-devhelp-indices)))
                 'string<)))

      (unless filelist
        (if (eval-when-compile (fboundp 'display-warning)) ;; not in emacs21
            (display-warning 'gtk-look "No devhelp files found")
          (message "No devhelp files found")))

      ;; if there's a .devhelp2 then don't look at the old .devhelp
      (dolist (filename filelist)
        (when (string-match "\\.devhelp2\\(\\.gz\\)?\\'" filename)
          (let ((base (substring filename 0 (match-beginning 0))))
            (setq filelist (delete (concat base ".devhelp")
                                   filelist))
            (setq filelist (delete (concat base ".devhelp.gz")
                                   filelist)))))

      (gtk-lookup--with-auto-compression
       (with-temp-buffer
         (while filelist
           (let ((filename (pop filelist)))
             (message "Processing %s" filename)

             ;; skip duplicates turned up by the wildcard expansions
             (setq filelist (delete filename filelist))

             (let ((dir (file-name-directory filename))
                   tail-list)
               ;; In Emacs 21.3 jka-compr doesn't erase the buffer
               ;; properly under the "replace" argument to
               ;; insert-file-contents, so use erase-buffer instead.
               ;; (Fixed in Emacs 22.)
               (erase-buffer)
               (insert-file-contents filename)

               ;; "<function ...>" is devhelp 1 format
               ;;
               ;; "<keyword type=...>" is devhelp 2 format
               ;; the name field can be
               ;;     "enum foo"
               ;;     "foo()"
               ;;     "foo ()"
               ;;
               ;; The field values are xml entity-encoded like "&rquot;"
               ;; etc, but that doesn't (or shouldn't) occur in C symbols.
               ;;
               (while (re-search-forward "\
<\\(function\\|keyword type=\"\\([^\"]*\\)\"\\)\
 name=\"\\([^\"]*\\)\"\
 link=\"\\([^\"]+\\)\"\
" nil t)
                 (let ((type (match-string 2))
                       (name (match-string 3))
                       (link (match-string 4))
                       tail)

                   ;; lose trailing "()" or " ()" on name of functions, and
                   ;; set `type' for functions in devhelp 1 format
                   (when (string-match " ?()\\'" name)
                     (setq name (substring name 0 (match-beginning 0)))
                     (setq type "function"))

                   ;; lose leading "enum" or "union" from name, and set
                   ;; `type' for devhelp 1 format
                   (when (string-match "\\`\\(enum\\|struct\\|union\\) "
                                       name)
                     (setq type (match-string 1 name))
                     (setq name (substring name (match-end 0))))

                   ;; devhelp 1 textual index entries usually have a space,
                   ;; exclude them
                   (if (and (not type)
                            (string-match " " name))
                       (setq type ""))

                   ;; devhelp 1 notice "struct" type from link
                   (if (and (not type)
                            (string-match "-struct\\'" link))
                       (setq type "struct"))

                   ;; devhelp 1 seen with some dubious empty name="",
                   ;; exclude them
                   (when (equal "" name)
                     (setq type ""))

                   ;; devhelp 2 exclude "property" and "signal" entries
                   ;; Could think about having them under qualified
                   ;; "GtkWidget::foo" or something.
                   (when (member type '("property" "signal"))
                     (setq type ""))

                   ;; devhelp 2 textual index entries have type="", exclude
                   ;; them
                   (when (not (equal type ""))

                     (if type (setq type (intern type)))
                     (setq tail (list dir type))
                     ;; share cons cells
                     (setq tail (or (car (assoc tail tail-list))
                                    (prog1 tail
                                      (push tail tail-list))))
                     (push (cons name (cons link tail))
                           result)))))))))

      (setq gtk-lookup-cache result)))
  gtk-lookup-cache)

(provide 'config-cc-mode)
