(require' pyvenv)
(pyvenv-tracking-mode -1)
(pyvenv-mode -1)
(add-hook 'pyvenv-post-activate-hooks
          (lambda ()
            (setq-default python-shell-buffer-name python-shell-buffer-name)))

