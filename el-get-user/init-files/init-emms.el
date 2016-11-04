(require 'emms-setup)
(emms-all)
(emms-default-players)

;; (add-hook 'org-clock-in-hook (lambda ()
;;                                (when (and emms-player-playing-p
;;                                           emms-player-paused-p)
;;                                  (emms-pause))))
;; (add-hook 'org-clock-out-hook (lambda ()
;;                                 (when emms-player-playing-p
;;                                   (emms-pause))))

