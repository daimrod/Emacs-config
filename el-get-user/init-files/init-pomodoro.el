(require 'pomodoro)

(defvar pomodoro-minute 0)
(defvar pomodoro-set 0)

(add-hook 'window-configuration-change-hook 'pomodoro-update-modeline)
(add-hook 'focus-in-hook 'pomodoro-update-modeline)
