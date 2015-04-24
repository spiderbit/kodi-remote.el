;;; kodi-remote.el --- functions to remote control a kodi instance

;; Copyright (C) 2015 Stefan Huchler

;; Author: Stefan Huchler <stefan.huchler@mail.de>

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(require 'request)
(require 'json)


(defvar kodi-host-name "localhost:9090")


(defun kodi-json-url ()
  (concat "http://" kodi-host-name "/jsonrpc")
  )


(defun kodi-remote-play-pause (url)
  (interactive "p")
   (request
    (kodi-json-url)
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
    (kodi-json-url)
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
    (kodi-json-url)
   :type "POST"
   :data (json-encode '(("id" . 1)("jsonrpc" . "2.0")("method" . "Playlist.Clear")("params" . (("playlistid" . 1)))))
   :headers '(("Content-Type" . "application/json"))
   :parser 'json-read)
  (request
    (kodi-json-url)
   :type "POST"
   :data json-with-url
   :headers '(("Content-Type" . "application/json"))
   :parser 'json-read)
  (request
    (kodi-json-url)
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
