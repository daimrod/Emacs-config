;;; elfeed-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "../modules/elfeed-org/elfeed-org" "../modules/elfeed-org/elfeed-org.el"
;;;;;;  (22062 28185 373320 449000))
;;; Generated autoloads from ../modules/elfeed-org/elfeed-org.el

(autoload 'elfeed-org "../modules/elfeed-org/elfeed-org" "\
Hook up rmh-elfeed-org to read the `org-mode' configuration when elfeed is run.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "../modules/elfeed/elfeed" "../modules/elfeed/elfeed.el"
;;;;;;  (22061 60019 210276 565000))
;;; Generated autoloads from ../modules/elfeed/elfeed.el

(autoload 'elfeed-update "../modules/elfeed/elfeed" "\
Update all the feeds in `elfeed-feeds'.

\(fn)" t nil)

(autoload 'elfeed "../modules/elfeed/elfeed" "\
Enter elfeed.

\(fn)" t nil)

(autoload 'elfeed-load-opml "../modules/elfeed/elfeed" "\
Load feeds from an OPML file into `elfeed-feeds'.
When called interactively, the changes to `elfeed-feeds' are
saved to your customization file.

\(fn FILE)" t nil)

(autoload 'elfeed-export-opml "../modules/elfeed/elfeed" "\
Export the current feed listing to OPML-formatted FILE.

\(fn FILE)" t nil)

;;;***

;;;### (autoloads nil nil ("../modules/elfeed/elfeed-db.el" "../modules/elfeed/elfeed-lib.el"
;;;;;;  "../modules/elfeed/elfeed-pkg.el" "../modules/elfeed/elfeed-search.el"
;;;;;;  "../modules/elfeed/elfeed-show.el" "../modules/elfeed/xml-query.el")
;;;;;;  (22062 22809 867075 547000))

;;;***

(provide 'elfeed-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; elfeed-autoloads.el ends here
