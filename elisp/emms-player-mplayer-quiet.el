;;; emms-player-mplayer-quiet.el --- mplayer support for EMMS

(require 'emms-compat)
(require 'emms-player-simple)
(require 'emms-player-mplayer)

(define-emms-simple-player mplayer-quiet '(file url)
  (concat "\\`\\(http\\|mms\\)://\\|"
          (emms-player-simple-regexp
           "ogg" "mp3" "wav" "mpg" "mpeg" "wmv" "wma"
           "mov" "avi" "divx" "ogm" "ogv" "asf" "mkv"
           "rm" "rmvb" "mp4" "flac" "vob" "m4a" "ape"
           "flv" "webm"))
  "mplayer" "-vo" "no" "-slave" "-quiet" "-really-quiet")

(define-emms-simple-player mplayer-quiet-playlist '(streamlist)
  "\\`http://"
  "mplayer" "-vo" "no" "-slave" "-quiet" "-really-quiet" "-playlist")

(emms-player-set emms-player-mplayer-quiet
		 'pause
		 'emms-player-mplayer-pause)

;;; Pause is also resume for mplayer
(emms-player-set emms-player-mplayer-quiet
                 'resume
                 nil)

(emms-player-set emms-player-mplayer-quiet
		 'seek
		 'emms-player-mplayer-seek)

(emms-player-set emms-player-mplayer-quiet
		 'seek-to
		 'emms-player-mplayer-seek-to)

(provide 'emms-player-mplayer-quiet)
;;; emms-player-mplayer-quiet.el ends here
