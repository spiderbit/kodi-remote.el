# kodi-remote

Emacs Remote Control for Kodi
![kodi-remote main menu](example.png)

## Requirements
to use kodi-remote-play-video-url you must install youtube-dl
* see youtube-dl for [supported sites](https://github.com/rg3/youtube-dl/blob/master/docs/supportedsites.md)

to use kodi-remote-play-stream-url you need to have the youtube 
plugin installed in kodi

## Installation
Available as the kodi-remote package on MELPA http://melpa.org/

Or:
	M-x package-install-file RET /path/to/kodi-remote.el RET

## Setup

### set the host name of the htpc instance:
    (setq kodi-host-name "my-htpc:8080")

### OPTIONAL:
	To use alternative playlist system (titles of youtube movies):
	(setq kodi-network-interface "enp0s25")
	For functionality that need direct write access to the filesystem:
	(setq kodi-dangerous-options t)
	(setq kodi-access-host "my-htpc")
	if you don't use ssh to access your kodi server / nas:
	(setq kodi-access-method "smb/ftp/adb...")

## Using

### open kodi remote:
    M-x kodi-remote

### starts playback of a youtube video
    M-x kodi-remote-play-video-url RET <paste youtube url> RET

### starts playback of a youtube video over the kodi-youtube plugin
    M-x kodi-remote-play-stream-url RET <paste youtube url> RET
	(advantage is that it works also with streamed videos)

### opens the kodi playlist
    M-x kodi-remote-playlist RET

## Contributing

Yes, please do! See [CONTRIBUTING][] for guidelines.

## License

See [COPYING][]. Copyright (c) 2015 Stefan Huchler.


[CONTRIBUTING]: ./CONTRIBUTING.md
[COPYING]: ./COPYING
