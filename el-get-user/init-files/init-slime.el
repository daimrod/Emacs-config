;;; Slime
(load (expand-file-name "~/quicklisp/slime-helper.el") t)
(load (expand-file-name "~/quicklisp/clhs-use-local.el") t)
(defalias 'srepl 'slime-repl)
(require 'slime-company)
(slime-setup '(slime-repl
               inferior-slime
               slime-asdf
               slime-banner
               slime-autodoc
               slime-editing-commands
               slime-fancy-inspector
               slime-fancy
               slime-snapshot
               slime-fontifying-fu
               slime-fuzzy
               slime-indentation
               slime-package-fu
               slime-references
               slime-scratch
               slime-xref-browser
               slime-presentations
               slime-company))

(slime-autodoc-mode)

(setq inferior-lisp-program "~/bin/sbcl"
      slime-complete-symbol-function 'slime-fuzzy-complete-symbol
      slime-startup-animation t
      slime-complete-symbol*-fancy t
      slime-net-coding-system 'utf-8-unix)

(add-hook 'lisp-mode-hook (lambda () (slime-mode 1)))
(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode 1)))

(defun common-lisp-indentation ()
  (set (make-local-variable lisp-indent-function)
       'common-lisp-indent-function))

(add-hook 'lisp-mode-hook 'common-lisp-indentation)

(define-key slime-repl-mode-map (kbd "C-c C-v C-l") 'slime-pretty-print-presentation-at-point)
(define-key slime-repl-mode-map (kbd "C-c C-v l") 'slime-pretty-print-presentation-at-point)
(define-key slime-repl-mode-map (kbd "C-x M-e") 'slime-pprint-eval-last-expression)
(define-key slime-mode-map (kbd "C-x M-e") 'slime-pprint-eval-last-expression)

(defun slime-dc ()
  "Close the current connection and the repl-buffer"
  (interactive)
  (save-window-excursion
    (slime-switch-to-output-buffer)
    (kill-buffer)
    (slime-net-close (slime-connection))))

;; Store fasls here
(make-directory "/tmp/slime-fasls/" t) ;; be sure the directory exists
(setq slime-compile-file-options '(:fasl-directory "/tmp/slime-fasls/"))

;; Add a directory to asdf:*central-registry*
(defslime-repl-shortcut slime-repl-add-to-central-registry
  ("add-to-central-registry" "+a" "add")
  (:handler (lambda (directory)
              (interactive
               (list (expand-file-name (file-name-as-directory
                                        (read-directory-name
                                         "Add directory: "
                                         (slime-eval '(swank:default-directory))
                                         nil nil "")))))
              (insert "(cl:pushnew (cl:truename #P\"" directory "\") asdf:*central-registry* :test #'equal)")
              (slime-repl-send-input t)))
  (:one-liner "Add a directory to asdf:*central-registry*"))

;;; Quickload a system
(defslime-repl-shortcut slime-repl-quickload
  ("quickload" "+ql" "ql")
  (:handler (lambda ()
              (interactive)
              (let* ((system-names
                      (slime-eval '(cl:nunion
                                    (swank:list-asdf-systems)
                                    (cl:nunion
                                     (cl:mapcar 'ql-dist:name
                                                (ql:system-list))
                                     (ql:list-local-systems)
                                     :test 'cl:string=)
                                    :test 'cl:string=)))
                     (default-value (slime-find-asd-file
                                     (or default-directory
                                         (buffer-file-name))
                                     system-names))
                     (prompt (concat "System "
                                     (if default-value
                                         (format " (default `%s'): " default-value)
                                       ": ")))
                     (system (completing-read prompt
                                              system-names
                                              nil nil nil
                                              'slime-system-history
                                              default-value)))
                (insert "(ql:quickload :" system ")")
                (slime-repl-send-input t))))
  (:one-liner "Quickload a system"))

;;; Awesome hacks available!
(setq slime-enable-evaluate-in-emacs t)

;;; CLHS
(quicklisp-clhs-setup-hyperspec-root)
(define-key slime-mode-map (kbd "C-c C-d f") 'common-lisp-hyperspec)
(define-key slime-repl-mode-map (kbd "C-c C-d f") 'common-lisp-hyperspec)

;;; dpans
(defun dmd-ansicl-lookup (major-mode)
  (info-lookup-add-help
   :mode major-mode
   :regexp "[^][()'\" \t\n]+"
   :ignore-case t
   :doc-spec '(("(ansicl)Symbol Index" nil nil nil))))
(mapc 'dmd-ansicl-lookup
      '(lisp-mode
        slime-repl-mode))

;;; bind C-c / to slime-selector
(define-key slime-mode-map (kbd "C-c /") 'slime-selector)
(define-key slime-repl-mode-map (kbd "C-c /") 'slime-selector)

(defun dmd-dump-slime ()
  "Dump current SLIME instance to PWD/slime.img"
  (interactive)
  (save-excursion
    (switch-to-buffer-other-window "*inferior-lisp*")
    (goto-char (point-min))
    (insert (format "(trivial-dump-core::sbcl-dump-image-slime %S)" (expand-file-name "slime.img")))
    (inferior-slime-return)))

(defun dmd-load-slime ()
  "Load a previously saved SLIME image (see `dmd-dump-slime') named PWD/slime.img."
  (interactive)
  (slime-start :program "~/bin/sbcl"
               :program-args '("--core" "slime.img")
               :directory default-directory))

