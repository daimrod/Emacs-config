(require 'emacs-w3m-autoloads)
(with-eval-after-load 'w3m
  (require 'w3m-util)
  (defun dmd--w3m-go-to-title-in-page ()
    (interactive)
    (let ((title (w3m-buffer-title (current-buffer)))
          (max-cut 10))
      (when title
        (goto-char (point-min))
        (while (and (not (search-forward-regexp title nil t))
                    (> (length title) max-cut)
                    (setq title (subseq title 0 (1- (length title))))))))))

