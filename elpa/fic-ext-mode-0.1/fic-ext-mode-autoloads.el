;;; fic-ext-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "fic-ext-mode" "fic-ext-mode.el" (21342 1846
;;;;;;  81855 86000))
;;; Generated autoloads from fic-ext-mode.el

(autoload 'fic-ext-mode "fic-ext-mode" "\
Fic mode -- minor mode for highlighting FIXME/TODO in comments

\(fn &optional ARG)" t nil)
(add-hook 'c-mode-common-hook 'fic-ext-mode)
(add-hook 'python-mode-hook 'fic-ext-mode)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; fic-ext-mode-autoloads.el ends here
