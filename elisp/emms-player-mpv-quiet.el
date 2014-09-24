;;; emms-player-mpv-quiet.el --- mpv support for EMMS

(require 'emms-compat)
(require 'emms-player-simple)
(require 'emms-player-mplayer)

(define-emms-simple-player mpv-quiet '(file url)
  (concat "\\`\\(http\\|mms\\)://\\|"
          (emms-player-simple-regexp
           "ogg" "mp3" "wav" "mpg" "mpeg" "wmv" "wma"
           "mov" "avi" "divx" "ogm" "ogv" "asf" "mkv"
           "rm" "rmvb" "mp4" "flac" "vob" "m4a" "ape"
           "flv" "webm"))
  "mpv" "-vo" "no" "--slave-broken" "--quiet" "--really-quiet")

(define-emms-simple-player mpv-quiet-playlist '(streamlist)
  "\\`http://"
  "mpv" "-vo" "no" "--slave-broken" "--quiet" "--really-quiet" "--playlist")

(emms-player-set emms-player-mpv-quiet
		 'pause
		 'emms-player-mpv-pause)

;;; Pause is also resume for mpv
(emms-player-set emms-player-mpv-quiet
                 'resume
                 nil)

(emms-player-set emms-player-mpv-quiet
		 'seek
		 'emms-player-mpv-seek)

(emms-player-set emms-player-mpv-quiet
		 'seek-to
		 'emms-player-mpv-seek-to)

(provide 'emms-player-mpv-quiet)
;;; emms-player-mpv-quiet.el ends here
