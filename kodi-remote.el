(require 'request)
(require 'json)

(defun kodi-remote-play-pause (url)
  (interactive "p")
   (request
    "http://localhost:9090/jsonrpc"
    :type "POST"
    :data (json-encode '(("id" . 1)
			 ("jsonrpc" . "2.0")
			 ("method" . "Player.PlayPause")
			 ("params" . (
				      ("playerid" . 1)
				      )
			  )))
    :headers '(("Content-Type" . "application/json"))
    :parser 'json-read))


(defun kodi-remote-music (url)
  (interactive "p")
   (request
    "http://localhost:9090/jsonrpc"
    :type "POST"
    :data (json-encode '(("id" . 1)
			 ("jsonrpc" . "2.0")
			 ("method" . "Player.Open")
			 ("params" . (
				      (
				       "item" .
				       (
					("partymode" . "music")
					)
				       )
				      )
			  )))
    :headers '(("Content-Type" . "application/json"))
    :parser 'json-read)
)


(defun kodi-remote-play-url (url)
  (interactive "surl: ")
  (setq json (json-encode '(("id" . 1)("jsonrpc" . "2.0")("method" . "Player.Open")("params" . (("item" .  (("file" . "url"))))))))
  (setq json-with-url (replace-regexp-in-string "url" url json))
  (request
   "http://localhost:9090/jsonrpc"
   :type "POST"
   :data (json-encode '(("id" . 1)("jsonrpc" . "2.0")("method" . "Playlist.Clear")("params" . (("playlistid" . 1)))))
   :headers '(("Content-Type" . "application/json"))
   :parser 'json-read)
  (request
   "http://localhost:9090/jsonrpc"
   :type "POST"
   :data json-with-url
   :headers '(("Content-Type" . "application/json"))
   :parser 'json-read)
  (request
   "http://localhost:9090/jsonrpc"
   :type "POST"
   :data json-with-url
   :headers '(("Content-Type" . "application/json"))
   :parser 'json-read)
  )

(defun kodi-remote-play-youtube-id (id)
  (interactive "sid: ")
  (setq pre-url "plugin:\/\/plugin.video.youtube\/?path=\/root\/search&action=play_video&videoid=")
  (setq url (concat pre-url id))
  (kodi-remote-play-url url)
  )

(defun kodi-remote-play-youtube-url (url)
  (interactive "surl: ")
  (when (string-match "v=[^&]*" url)
    (kodi-remote-play-youtube-id (substring (match-string 0 url) 2 ))))


(provide 'kodi-remote)
