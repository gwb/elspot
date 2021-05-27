
(require 'async)
(require 'hydra)
(require 's)
(require 'dash)

;; A few string utilities 
(defun s-surround (by-what str)
  (s-concat by-what str by-what))

(defun s-join+ (separator &rest strings)
  (s-join separator strings))


;; High-level macros for interfacing with spotify via osascript
(defmacro spotify--tell (&rest strings)
  `(s-join+ " -e "
            "osascript"
            "'tell application \"Spotify\"'"
            ,@(-map (-partial #'s-surround "'") strings)
            "'end tell'"))


(defmacro spotify--cmd (&rest strings)
  `(s-chomp (shell-command-to-string (spotify--tell ,@strings))))


;; Interactive commands for spotify
(defun spotify--format-str-duration (str-duration &optional normalize)
  (format-seconds "%m : %s"
                  (/ (string-to-number str-duration) (or normalize 1))))


(defun spotify--status ()
  (list :artist    (spotify--cmd "artist of current track")
        :album     (spotify--cmd "album of current track")
        :track     (spotify--cmd "name of current track")
        :duration  (spotify--format-str-duration
                    (spotify--cmd "duration of current track")
                    1000)
        :position  (spotify--format-str-duration
                    (spotify--cmd "player position"))))


;; (defun spotify-refresh-info ()
;;   (interactive)
;;   (let ((status (spotify--status)))
;;     (setq spotify--track (plist-get status :track))
;;     (setq spotify--artist (plist-get status :artist))
;;     (setq spotify--album (plist-get status :album))))

(defun spotify--t/f-to-boolean (str)
  (equal str "true"))

(defun spotify--playback-status ()
  (let ((shuffling (spotify--t/f-to-boolean (spotify--cmd "shuffling")))
        (repeating (spotify--t/f-to-boolean (spotify--cmd "repeating"))))
    (--> ""
      (if shuffling (s-concat it "[shuffling] ") it)
      (if repeating (s-concat it "[repeating] ") it))))

(defun spotify-refresh-info ()
  (interactive)
  (setq spotify--track (spotify--cmd "name of current track"))
  (setq spotify--artist (spotify--cmd "artist of current track"))
  (setq spotify--album (spotify--cmd "album of current track"))
  (setq spotify--playback (spotify--playback-status)))

(defun spotify-playpause ()
  (interactive)
  (let ((cur-status (spotify--cmd "player state as string")))
    (if (s-equals? cur-status "playing")
        (message "Spotify now paused.")
      (message "Spotify now playing."))
    (spotify--cmd "playpause")
    (spotify-refresh-info)))

(defun spotify-next ()
  (interactive)
  (spotify--cmd "next track")
  (spotify-refresh-info))

(defun spotify-previous ()
  (interactive)
  (spotify--cmd "set player position to 0" "previous track")
  (spotify-refresh-info))



(defhydra hydra-spotify
  (:color pink
          :hint nil
          :pre (progn
                 (setq spotify--track "-")
                 (setq spotify--artist "-")
                 (setq spotify--album "-")
                 (setq spotify--playback "")))
  "
Controls                       Info
--------------------------------------
_SPC_: play / pause         Track : %s(identity spotify--track)
_n_  : next                 Artist: %s(identity spotify--artist)
_p_  : previous             Album : %s(identity spotify--album)
_i_  : fetch info              %s(identity spotify--playback)
"
  ("SPC" spotify-playpause)
  ("n" spotify-next)
  ("p" spotify-previous)
  ("i" spotify-refresh-info)
  ("q" nil "quit" :color blue))



(provide 'elspot)
;;; elspot.el ends here
