;; Remove inexistant projects
(require 'projectile)
(dolist (proj projectile-known-projects)
  (unless (file-exists-p proj)
    (setq projectile-known-projects (delete proj projectile-known-projects))))
(projectile-save-known-projects)
(projectile-global-mode 1)
