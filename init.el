;;;; init.el

(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Load path etc.

;; load-path enhancement
(defun fni/add-to-load-path (this-directory &optional with-subdirs recursive)
  "Add THIS-DIRECTORY at the beginning of the load-path, if it exists.
Add all its subdirectories not starting with a '.' if the
optional argument WITH-SUBDIRS is not nil.
Do it recursively if the third argument is not nil."
  (when (and this-directory
             (file-directory-p this-directory))
    (let* ((this-directory (expand-file-name this-directory))
           (files (directory-files this-directory t "^[^\\.]")))

      ;; completely canonicalize the directory name (*may not* begin with `~')
      (while (not (string= this-directory (expand-file-name this-directory)))
             (setq this-directory (expand-file-name this-directory)))

      (message "Adding `%s' to load-path..." this-directory)
      (add-to-list 'load-path this-directory)

      (when with-subdirs
        (while files
               (let ((dir-or-file (car files)))
                 (when (file-directory-p dir-or-file)
                   (if recursive
                       (fni/add-to-load-path dir-or-file 'with-subdirs 'recursive)
                       (fni/add-to-load-path dir-or-file))))
               (setq files (cdr files)))))))

(setq home-dir "/home/daimrod/")
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(setq elisp-dir (concat dotfiles-dir "elisp/"))
(setq elpa-dir (concat dotfiles-dir "elpa/"))

(fni/add-to-load-path dotfiles-dir)
(fni/add-to-load-path elisp-dir t)
(fni/add-to-load-path elpa-dir t)

(setq custom-file (concat dotfiles-dir "custom.el"))

;; These should be loaded on startup rahter than autoloaded on demande
;; since they are likely to be used in every session

(require 'cl)
(require 'saveplace)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)
(require 'w3m-load)
(require 'mime-w3m)
(require 'workgroups)
(require 'scratch)
(require 'verbiste)
(require 'undo-tree)
(require 'woman)
(require 'alarm)
(require 'pos-tip)

;; Load my configuration
(require 'config-defuns)
(require 'config-bindings)
(require 'config-theme)
(require 'config-lisp)
(require 'config-cc-mode)
(require 'config-ediff)
(require 'config-slime)
(require 'config-yasnippet)
(require 'config-misc)
(require 'config-bbdb)
(require 'config-git)
(require 'config-alias)
(require 'config-org)
(require 'config-auto-complete)

(load custom-file 'noerror)

;; enabled/disabled commands
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
