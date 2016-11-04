(setq mu4e-mu-binary (or (executable-find "mu")
                         (expand-file-name "mu/mu/mu"
                                           modules-dir)))

(add-hook 'mu4e-view-mode-hook
          (lambda()
            (require 'org-mu4e)
            ;; try to emulate some of the eww key-bindings
            (local-set-key (kbd "<tab>") 'w3m-next-anchor)
            (local-set-key (kbd "<backtab>") 'w3m-previous-anchor)
            (local-set-key (kbd "*") (lambda ()
                                       (interactive)
                                       (org-capture nil "m")
                                       (mu4e-view-mark-for-flag)))))

(defun dmd-mu4e-view-add-subject-to-buffer-name ()
  "Add Subject to buffer name."
  (let* ((subject (plist-get mu4e~view-msg :subject))
         (buffer-name (buffer-name))
         (new-name (concat buffer-name " " subject)))
    (rename-buffer new-name)))

(add-hook 'mu4e-view-mode-hook #'dmd-mu4e-view-add-subject-to-buffer-name)
