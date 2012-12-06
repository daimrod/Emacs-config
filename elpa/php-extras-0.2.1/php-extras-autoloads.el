;;; php-extras-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (php-extras-eldoc-documentation-function php-extras-insert-previous-variable
;;;;;;  php-extras-insert-previous-variable-key) "php-extras" "php-extras.el"
;;;;;;  (20672 39741))
;;; Generated autoloads from php-extras.el

(defvar php-extras-insert-previous-variable-key [(control c) (control $)] "\
Key sequence for `php-extras-insert-previous-variable'.")

(custom-autoload 'php-extras-insert-previous-variable-key "php-extras" nil)

(autoload 'php-extras-insert-previous-variable "php-extras" "\
Insert previously used variable from buffer.
With prefix argument search that number of times backwards for
variable. If prefix argument is negative search forward.

\(fn ARG)" t nil)

(autoload 'php-extras-eldoc-documentation-function "php-extras" "\
Get function arguments for core PHP function at point.

\(fn)" nil nil)

(add-hook 'php-mode-hook #'(lambda nil (unless eldoc-documentation-function (set (make-local-variable 'eldoc-documentation-function) #'php-extras-eldoc-documentation-function))))

(eval-after-load 'php-mode `(let ((map php-mode-map) (key php-extras-insert-previous-variable-key)) (define-key map key 'php-extras-insert-previous-variable)))

;;;***

;;;### (autoloads (php-extras-generate-eldoc) "php-extras-gen-eldoc"
;;;;;;  "php-extras-gen-eldoc.el" (20672 39741))
;;; Generated autoloads from php-extras-gen-eldoc.el

(autoload 'php-extras-generate-eldoc "php-extras-gen-eldoc" "\
Regenerate PHP function argument hash table from php.net. This is slow!

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("php-extras-eldoc-functions.el" "php-extras-pkg.el")
;;;;;;  (20672 39942 929717))

;;;***

(provide 'php-extras-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; php-extras-autoloads.el ends here
