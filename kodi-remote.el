;;; kodi-remote.el --- functions to remote control a kodi instance

;; Copyright (C) 2015 Stefan Huchler

;; Author: Stefan Huchler <stefan.huchler@mail.de>
;; URL: http://github.com/spiderbit/kodi-remote.el
;; Package-Requires: ((request "0.2.0"))

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

;;; Commentary:
;;
;; Emacs Remote Control functions for Kodi
;; including a function to play directly videos from youtube and
;; other sites see youtube-dl for supported sites.
;;

;;; Code:

(require 'request)
(require 'json)


(defvar kodi-host-name "localhost:9090")
(defvar kodi-active-player -1)
(defvar kodi-active-window nil)
(defvar kodi-fullscreen nil)
(defvar kodi-volume nil)

(defun kodi-json-url ()
  "Function to create the full json-url of the kodi-instance."
  (concat "http://" kodi-host-name "/jsonrpc")
  )

;;;###autoload
(defun kodi-remote-play-pause ()
  "Toggle play/pause of active player."
  (interactive)
  (kodi-remote-get-active-player-id)
  (sit-for 0.01)
  (request
   (kodi-json-url)
   :type "POST"
   :data (json-encode `(("id" . 1)
			("jsonrpc" . "2.0")
			("method" . "Player.PlayPause")
			("params" . (
				     ("playerid" . ,kodi-active-player
				      )
				     )
			 )))
   :headers '(("Content-Type" . "application/json"))
   :parser 'json-read))



;;;###autoload
(defun kodi-remote-stop ()
  "Stopps active player."
  (interactive)
  (kodi-remote-get-active-player-id)
  (sit-for 0.01)
  (request
   (kodi-json-url)
   :type "POST"
   :data (json-encode `(("id" . 1)
			("jsonrpc" . "2.0")
			("method" . "Player.Stop")
			("params" . (
				     ("playerid" . ,kodi-active-player
				      )
				     )
			 )))
   :headers '(("Content-Type" . "application/json"))
   :parser 'json-read))


(defun kodi-remote-player-seek (direction)
  "Seeks active player."
  (interactive)
  (sit-for 0.01)
  (request
   (kodi-json-url)
   :type "POST"
   :data (json-encode `(("id" . 1)
			("jsonrpc" . "2.0")
			("method" . "Player.Seek")
			("params" . (
				     ("playerid" . ,kodi-active-player
				      )
				     ("value" . ,direction)
				     )
			 )))
   :headers '(("Content-Type" . "application/json"))
   :parser 'json-read))


(defun kodi-remote-input (input)
  (request
   (kodi-json-url)
   :type "POST"
   :data (json-encode `(("id" . 1)
			("jsonrpc" . "2.0")
			("method" . ,input)
			))
   :headers '(("Content-Type" . "application/json"))
   :parser 'json-read))


(defun kodi-remote-input-left ()
  "Move left in menu"
  (interactive)
  (kodi-remote-get-active-window)
  (sit-for 0.1)
  (if (string-equal kodi-active-window "Fullscreen video")
      (kodi-remote-player-seek "smallbackward")
    (kodi-remote-input "Input.Left")))


(defun kodi-remote-input-right ()
  "Move left in menu"
  (interactive)
  (kodi-remote-get-active-window)
  (sit-for 0.1)
  (if (string-equal kodi-active-window "Fullscreen video")
      (kodi-remote-player-seek "smallforward")
    (kodi-remote-input "Input.Right")))

(defun kodi-remote-input-up ()
  "Move up in menu"
  (interactive)
  (kodi-remote-get-active-window)
  (sit-for 0.1)
  (if (string-equal kodi-active-window "Fullscreen video")
      (kodi-remote-player-seek "bigforward")
    (kodi-remote-input "Input.Up")))

(defun kodi-remote-input-down ()
  "Move down in menu"
  (interactive)
  (kodi-remote-get-active-window)
  (sit-for 0.1)
  (if (string-equal kodi-active-window "Fullscreen video")
      (kodi-remote-player-seek "bigbackward")
    (kodi-remote-input "Input.Down")))

(defun kodi-remote-input-back ()
  "Moves back menu"
  (interactive)
  (kodi-remote-input "Input.Back"))

(defun kodi-remote-input-context-menu ()
  "Activates context menu"
  (interactive)
  (kodi-remote-input "Input.ContextMenu"))

(defun kodi-remote-input-home ()
  "Switches to the home screen"
  (interactive)
  (kodi-remote-input "Input.Home"))

;; (defun kodi-remote-input-info ()
;;   "Kodi move left in menu"
;;   (interactive)
;;   (kodi-remote-input "Input.Info"))

;; (defun kodi-remote-input-show-codec ()
;;   "Kodi move left in menu"
;;   (interactive)
;;   (kodi-remote-input "Input.ShowCodec"))

;; (defun kodi-remote-input-show-osd ()
;;   "Kodi move left in menu"
;;   (interactive)
;;   (kodi-remote-input "Input.ShowOSD"))

(defun kodi-remote-select ()
  "Select active item"
  (interactive)
  (kodi-remote-input "Input.Select"))


;; (defun kodi-remote-input-send-text ()
;;   "Kodi move right in menu"
;;   (interactive)
;;   (request
;;    (kodi-json-url)
;;    :type "POST"
;;    :data (json-encode '(("id" . 1)
;; 			("jsonrpc" . "2.0")
;; 			("method" . "Input.SendText()")
;; 			("params" . (
;; 				     ("text" . "test")
;; 				     )
;; 			)))
;;    :headers '(("Content-Type" . "application/json"))
;;    :parser 'json-read)
;;   )


;;;###autoload
(defun kodi-remote-music ()
  "Start musik playing in kodi in party mode."
  (interactive)
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


(defun kodi-remote-playlist-previous ()
  "Previous song in kodi music player."
  (interactive)
  (kodi-remote-playlist-goto "previous"))

(defun kodi-remote-playlist-next ()
  "Next song in kodi music player."
  (interactive)
  (kodi-remote-playlist-goto "next"))


(defun kodi-remote-get-active-player-id ()
  (interactive)
  (request
   (kodi-json-url)
   :data (json-encode '(("id" . 0)
			("jsonrpc" . "2.0")
			("method" . "Player.GetActivePlayers")
			))
   :headers '(("Content-Type" . "application/json"))
   :success
   (function* (lambda (&key data &allow-other-keys)
   		(when data
		  (setq kodi-active-player (cdr (elt  (cdr (elt (cdr (car (json-read-from-string data))) 0 )) 0 )))
		  ;; (print kodi-active-player)
		  )))
   :error
   (function* (lambda (&key error-thrown &allow-other-keys&rest _)
   		(message "Got error: %S" error-thrown)))
   ;; :complete (lambda (&rest _) (message "Finished!"))
   :parser 'buffer-string))



(defun kodi-remote-get-active-window ()
  (interactive)
  (request
   (kodi-json-url)
   :data (json-encode '(("id" . 0)
			("jsonrpc" . "2.0")
			("method" . "Gui.GetProperties")
			("params" . (
				     ("properties" . ("currentwindow"))
				     )
			 )
			))
   :headers '(("Content-Type" . "application/json"))
   :success
   (function* (lambda (&key data &allow-other-keys)
   		(when data
		  (setq kodi-active-window (let-alist (json-read-from-string data) .result.currentwindow.label))
		  ;; (print kodi-active-window)
		  )))
   :error
   (function* (lambda (&key error-thrown &allow-other-keys&rest _)
   		(message "Got error: %S" error-thrown)))
   ;; :complete (lambda (&rest _) (message "Finished!"))
   :parser 'buffer-string))

(defun kodi-remote-get-volume ()
  (interactive)
  (request
   (kodi-json-url)
   :data (json-encode '(("id" . 0)
			("jsonrpc" . "2.0")
			("method" . "Application.GetProperties")
			("params" . (
				     ("properties" . ("volume"))
				     )
			 )
			))
   :headers '(("Content-Type" . "application/json"))
   :success
   (function* (lambda (&key data &allow-other-keys)
   		(when data
		  (setq kodi-volume (let-alist (json-read-from-string data) .result.volume))
		  ;; (print kodi-volume)
		  )))
   :error
   (function* (lambda (&key error-thrown &allow-other-keys&rest _)
   		(message "Got error: %S" error-thrown)))
   ;; :complete (lambda (&rest _) (message "Finished!"))
   :parser 'buffer-string))



(defun kodi-remote-toggle-fullscreen ()
  (interactive)
  (request
   (kodi-json-url)
   :type "POST"
   :data (json-encode '(("id" . 0)
			("jsonrpc" . "2.0")
			("method" . "Gui.SetFullScreen")
			("params" . (
				     ("fullscreen" . "toggle")
				     )
			 )
			))
   :headers '(("Content-Type" . "application/json"))
   :parser 'json-parse))


(defun kodi-remote-set-volume (offset)
  ;; (interactive)
  (kodi-remote-get-volume)
  (sit-for 0.1)
  (let* ((vol (+ kodi-volume offset)))
    (request
     (kodi-json-url)
     :type "POST"
     :data (json-encode `(("id" . 0)
			  ("jsonrpc" . "2.0")
			  ("method" . "Application.SetVolume")
			  ("params" . (
				       ("volume" . ,vol)
				       )
			   )
			  ))
     :headers '(("Content-Type" . "application/json"))
     :parser 'json-parse)))


(defun kodi-remote-volume-decrease ()
  "Decrease the volume of kodi"
  (interactive)
  (kodi-remote-set-volume -10))

(defun kodi-remote-volume-increase ()
  "Increase the volume of kodi"
  (interactive)
  (kodi-remote-set-volume 10))


(defun kodi-remote-is-fullscreen ()
  (interactive)
  (request
   (kodi-json-url)
   :data (json-encode '(("id" . 0)
			("jsonrpc" . "2.0")
			("method" . "Gui.GetProperties")
			("params" . (
				     ("properties" . ("fullscreen"))
				     )
			 )
			))
   :headers '(("Content-Type" . "application/json"))
   :success
   (function* (lambda (&key data &allow-other-keys)
   		(when data
		  (setq kodi-fullscreen (cdr (elt (cdr (car (json-read-from-string data))) 0 )))
		  ;; (print kodi-fullscreen)
		  )))
   :error
   (function* (lambda (&key error-thrown &allow-other-keys&rest _)
   		(message "Got error: %S" error-thrown)))
   ;; :complete (lambda (&rest _) (message "Finished!"))
   :parser 'buffer-string))


(defun kodi-remote-playlist-goto (pos)
  "Function to set the POS of kodi musik player."
  (request
   (kodi-json-url)
   :type "POST"
   :data (json-encode `(("id" . 1)("jsonrpc" . "2.0")
			("method" . "Player.GoTo")
			("params" . (("playerid" . 0)
				     ("to" . ,pos)))))
   :headers '(("Content-Type" . "application/json"))
   :parser 'json-read)
  )


(defun kodi-remote-play-url (url)
  "Plays either direct links to video files or plugin play command URLs."
  (interactive "surl: ")
  (let* ((json (json-encode `(("id" . 1)("jsonrpc" . "2.0")("method" . "Player.Open")("params" . (("item" .  (("file" . ,url)))))))))
    (request
     (kodi-json-url)
     :type "POST"
     :data (json-encode '(("id" . 1)("jsonrpc" . "2.0")("method" . "Playlist.Clear")("params" . (("playlistid" . 1)))))
     :headers '(("Content-Type" . "application/json"))
     :parser 'json-read)
    (request
     (kodi-json-url)
     :type "POST"
     :data json
     :headers '(("Content-Type" . "application/json"))
     :parser 'json-read)))


;;;###autoload
(defun kodi-remote-play-video-url (video-url)
  "Sends urls from videos like youtube to kodi.
it depends on having youtube-dl installed because that was the only way
I got it to run.  Using quvi to get the url or dircectly sending a play
command to the plugin did both not work.
could be used for other sites, too.  whatever youtube-dl supports.
Argument VIDEO-URL A Url from a youtube video."
  (interactive "surl: ")
  (let ((url
	 (substring
	  (shell-command-to-string
	   (concat "youtube-dl -f best -g " video-url)) 0 -1)))
    (kodi-remote-play-url url)))



;; Create the keymap for this mode
(defvar kodi-remote-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "m") 'kodi-remote-music)
    (define-key map (kbd "SPC") 'kodi-remote-play-pause)
    (define-key map (kbd "<left>") 'kodi-remote-input-left)
    (define-key map (kbd "<right>") 'kodi-remote-input-right)
    (define-key map (kbd "<up>") 'kodi-remote-input-up)
    (define-key map (kbd "<down>") 'kodi-remote-input-down)
    (define-key map (kbd "<backspace>") 'kodi-remote-input-back)
    (define-key map (kbd "<return>") 'kodi-remote-select)
    (define-key map (kbd "x") 'kodi-remote-stop)
    ;; (define-key map (kbd "<delete>") 'kodi-remote-file-delete)
    (define-key map (kbd "h") 'kodi-remote-input-left)
    (define-key map (kbd "n") 'kodi-remote-input-right)
    (define-key map (kbd "c") 'kodi-remote-input-up)
    (define-key map (kbd "t") 'kodi-remote-input-down)
    (define-key map (kbd "=") 'kodi-remote-volume-increase)
    (define-key map (kbd "+") 'kodi-remote-volume-increase)
    (define-key map (kbd "-") 'kodi-remote-volume-decrease)
    (define-key map (kbd "<tab>") 'kodi-remote-toggle-fullscreen)
    map)
  "Keymap for kodi-remote-mode.")



(define-derived-mode kodi-remote-mode special-mode "kodi-remote"
  "Major mode for remote controlling kodi instance
Key bindings:
\\{kodi-remote-mode-map}
")

;;;###autoload
(defun kodi-remote ()
  "Open a `kodi-remote-mode' buffer."
  (interactive)
  (let* ((name "*kodi-remote*")
         (buffer (or (get-buffer name)
                     (generate-new-buffer name))))
    (unless (eq buffer (current-buffer))
      (with-current-buffer buffer
        (unless (eq major-mode 'kodi-remote-mode)
          (condition-case e
              (progn
		;; (setq buffer-read-only nil)
                (kodi-remote-mode)
		;; (insert "Kodi Remote")
		;; (newline 3)
		;; (insert "kodi-remote-mode-map")
		;; (read-only-mode)
                ;; (KODI-remote-draw)()
                ;; (goto-char (point-min))
		)
            (error
             (kill-buffer buffer)
             (signal (car e) (cdr e))))))
      (switch-to-buffer-other-window buffer))))


;;; Some code where I tried to get appending working:

;; (defun kodi-remote-append-url (url)
;;   "appends video urls to the video queue, either pure urls to video files
;; or plugin command urls"
;;   (interactive "surl: ")
;;   (setq json (json-encode '(("id" . 1)("jsonrpc" . "2.0")("method" . "Playlist.Add")("params" . (("playlistid" . 1)("item" .  (("file" . "url"))))))))
;;   (setq json-with-url (replace-regexp-in-string "url" url json))
;;   (request
;;    (kodi-json-url)
;;    :type "POST"
;;    :data json-with-url
;;    :headers '(("Content-Type" . "application/json"))
;;    :parser 'json-read)
;;   )



;; (defun kodi-remote-append-video-url (video-url)
;;   "sends urls from videos like youtube to kodi.
;; it depends on having youtube-dl installed because that was the only way
;; I got it to run. Using quvi to get the url or dircectly sending a play
;; command to the plugin did both not work.
;; could be used for other sites, too. whatever youtube-dl supports."
;;   (interactive "surl: ")
;;   (let ((url
;; 	 (substring
;; 	  (shell-command-to-string
;; 	   (concat "youtube-dl -f best -g " video-url)) 0 -1)))
;;     (kodi-remote-append-url url)))


;;; Code for appending ends here


(provide 'kodi-remote)
;;; kodi-remote.el ends here
