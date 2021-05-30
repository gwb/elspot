 ;;; -*- lexical-binding: t -*-

(require 'cl)
(require 'request)
(require 'async)
(require 'hydra)
(require 's)
(require 'dash)

(defvar SPOTIFY-CLIENT-FILEPATH "/Users/gwb/.shpotify.cfg") ;; user-specific
(defconst SPOTIFY_TOKEN_URI "https://accounts.spotify.com/api/token")
(defconst SPOTIFY_SEARCH_API "https://api.spotify.com/v1/search")
(defvar spotify-access-token "")


;; A few string utilities 
(defun s-surround (by-what str)
  (s-concat by-what str by-what))

(defun s-join+ (separator &rest strings)
  (s-join separator strings))

(defun s-join-prepend (separator strings)
  (s-prepend separator
             (s-join separator strings)))

(defun s-quote (str)
  (s-surround "\"" str))

(defun s-fit (str len)
  (if (> (length str) len)
      (s-truncate len str)
      (s-pad-right len " " str)))

(defun s-chop (fix s)
  (s-chop-prefix fix
                 (s-chop-suffix fix s)))

;; Workhorse macros for interacting with spotify via Applescript
(defmacro spotify--tell (&rest strings)
  (let ((part1 (s-join+ " -e "
                        "osascript"
                        "'tell application \"Spotify\"'"))
        (part3 " -e 'end tell'")
        (strings-lst (make-symbol "strings-lst")))
    `(let ((,strings-lst (-map (-partial #'s-surround "'") (list ,@strings))))
       (s-join " "
               (append '(,part1)
                       (list (s-join-prepend " -e " ,strings-lst))
                       '(,part3))))))

(defmacro spotify--cmd (&rest strings)
  `(s-chomp (shell-command-to-string (spotify--tell ,@strings))))


;; Interactive commands for spotify
(defun spotify--format-str-duration (str-duration &optional normalize)
  (format-seconds "%m : %s"
                  (/ (string-to-number str-duration) (or normalize 1))))

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


;; Dealing with the search API

(defun spotify--read-client-info ()
  (let* ((client-info-raw (with-temp-buffer
                            (insert-file-contents SPOTIFY-CLIENT-FILEPATH)
                            (buffer-string)))
         (client-info-alist (--map (s-split "=" it)
                                   (s-split "\n" client-info-raw t))))
    (list :client-id (s-chop "\"" (cadr (assoc "CLIENT_ID" client-info-alist)))
          :client-secret (s-chop "\"" (cadr (assoc "CLIENT_SECRET" client-info-alist))))))

(defun spotify--get-credentials ()
  (let ((client-info (spotify--read-client-info)))
    (->> (s-concat (plist-get client-info :client-id)
                   ":"
                   (plist-get client-info :client-secret))
      (base64-encode-string)
      (s-replace "\n" "")
      (s-replace "\r" ""))))


(defun spotify--build-access-token-query ()
  (s-join+ " "
           "curl"
           SPOTIFY_TOKEN_URI
           "--silent"
           "-X POST"
           "-H" (s-quote (s-concat "Authorization: Basic "
                                   (spotify--get-credentials))) 
           "-d" "grant_type=client_credentials"))


(defun spotify--fetch-access-token ()
  (let* ((query-result-raw (shell-command-to-string (spotify--build-access-token-query)))
         (access_token (gethash "access_token" (json-parse-string query-result-raw))))
    (setq spotify-access-token access_token)
    access_token))


(defun spotify--get-access-token (&optional force-refresh-token)
  (if force-refresh-token
      (spotify--fetch-access-token)
    spotify-access-token))



(defun spotify--build-search-query (args type limit offset access-token)
  (s-join+ " "
           "curl"
           "-s" "-G"
           SPOTIFY_SEARCH_API
           "--data-urlencode"
           (s-quote (s-concat "q=" args))
           "-d"
           (s-quote (s-concat "type=" type
                              "&limit=" (int-to-string limit)
                              "&offset=" (int-to-string offset)))
           "-H" "Accept: application/json"
           "-H" (s-quote (s-join+ " " "Authorization: Bearer" access-token))))


(defun spotify--parse-search-query-result (query-result-string)
  (cl-flet ((get-track (entry) (gethash "name" entry))
            (get-uri (entry) (gethash "uri" entry))
            (get-album (entry) (gethash "name" (gethash "album" entry)))
            (get-artists (entry) (--map (gethash "name" it)
                                        (gethash "artists" entry)))) 
           (let ((entries (gethash "items"
                                   (gethash "tracks"
                                            (json-parse-string query-result-string)))))
             (--map (list
                     :name (get-track it)
                     :artists (get-artists it)
                     :album (get-album it)
                     :uri (get-uri it))
                    entries))))

(defun spotify--token-error? (query-result)
  (gethash "error" query-result))

(defun spotify--search-api (search type limit offset force-refresh-token)
  (let* ((access-token (spotify--get-access-token force-refresh-token))
        (query-result-raw (shell-command-to-string
                           (spotify--build-search-query
                            search
                            type limit
                            offset
                            access-token))))
    (if (spotify--token-error? (json-parse-string query-result-raw))
        (if force-refresh-token
            (error "Error could not authenticate, even with fresh token")
          (message "Access token may have expired.. trying to fetch a new one")
          (spotify--search-api search type limit offset t))
      (spotify--parse-search-query-result query-result-raw))))


;; User interface
(defun spotify-hydra-search-api (search)
  (setq spotify--search-result (spotify--search-api search "track" 4 0 nil))
  (hydra-spotify-list/body))


(defun spotify-prompt-search ()
  (interactive)
  (ivy-read "Query: "
            nil
            :action #'spotify-hydra-search-api))

(defun sr--track (idx)
  (s-fit (plist-get (nth idx spotify--search-result) :name) 30))

(defun sr--artist (idx)
  (s-fit (s-join " + " (plist-get (nth idx spotify--search-result) :artists)) 25))

(defun sr--album (idx)
  (s-fit (plist-get (nth idx spotify--search-result) :album) 25))


(defun spotify--play-uri (uri)
  (let ((cmd (format "play track \"%s\"" uri)))
    (spotify--cmd cmd)))


(defmacro spotify--setup-play-fns ()
  (let ((fn-name-lst (--map (intern (format "spotify--play-%s" it))
                            (-iota 5 1))))
    `(progn
       ,@(--map `(defun ,(nth (1- it) fn-name-lst) ()
                   (interactive)
                   (spotify--play-uri (plist-get (nth ,it spotify--search-result) :uri))
                   (hydra-spotify/body))
                (-iota 5 1)))))


(spotify--setup-play-fns)

;; (defun spotify--play-1 ()
;;   (interactive)
;;   (spotify--play-uri (plist-get (nth 0 spotify--search-result) :uri))
;;   (hydra-spotify/body))

;; (defun spotify--play-2 ()
;;   (interactive)
;;   (spotify--play-uri (plist-get (nth 1 spotify--search-result) :uri))
;;   (hydra-spotify/body))

;; (defun spotify--play-3 ()
;;   (interactive)
;;   (spotify--play-uri (plist-get (nth 2 spotify--search-result) :uri))
;;   (hydra-spotify/body))

;; (defun spotify--play-4 ()
;;   (interactive)
;;   (spotify--play-uri (plist-get (nth 3 spotify--search-result) :uri))
;;   (hydra-spotify/body))

;; (defun spotify--play-5 ()
;;   (interactive)
;;   (spotify--play-uri (plist-get (nth 3 spotify--search-result) :uri))
;;   (hydra-spotify/body))


;; Defining the hydra (1)
(defhydra hydra-spotify-list
  (:color blue
          :hint nil
          :pre 1)
  "
id         Track                           Artist                        Album
--------------------------------------------------------------------------------------
_1_ : %s(sr--track 0)^^|  %s(sr--artist 0)^^|  %s(sr--album 0)
_2_ : %s(sr--track 1)^^|  %s(sr--artist 1)^^|  %s(sr--album 1)
_3_ : %s(sr--track 2)^^|  %s(sr--artist 2)^^|  %s(sr--album 2)
_4_ : %s(sr--track 3)^^|  %s(sr--artist 3)^^|  %s(sr--album 3)
_5_ : %s(sr--track 4)^^|  %s(sr--artist 4)^^|  %s(sr--album 4)

"
  ("1" spotify--play-1)
  ("2" spotify--play-2)
  ("3" spotify--play-3)
  ("4" spotify--play-4)
  ("5" spotify--play-5)
  ("b" hydra-spotify/body "back")
  ("q" nil "quit"))


;; Defining the hydra (2)

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
  ("s" spotify-prompt-search "search" :color blue)
  ("q" nil "quit" :color blue))








(provide 'elspot)
;;; elspot.el ends here
