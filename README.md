# kodi-remote

## Summary

Emacs Remote Control for Kodi
Including a function to send youtube and other urls to kodi
* see youtube-dl for [supported sites](https://github.com/rg3/youtube-dl/blob/master/docs/supportedsites.md)

## Requirements

to use the video website play feature you must install youtube-dl on your machine.

## Installing

### clone kodi remote
    cd ~/.emacs.d/
    git clone <repo-url>

### add the path of kodi-remote and to your emacs init file:
    (add-to-list 'load-path "~/.emacs.d/kodi-remote")
    (require 'kodi-remote)
    (setq kodi-host-name "my-htpc:8080")

### set the network interface you connect over to kodi (optional only testet under linux)
	(setq kodi-network-interface "enp0s25")

### OPTIONAL: setup settings for deleting files (over tramp)
	(setq kodi-dangerous-options t)
	(setq kodi-access-host "my-htpc")
	if you don't use ssh to access your kodi server / nas:
	(setq kodi-access-method "smb/ftp/adb...")

you need a running kodi instance of course!

## Using

### open kodi remote:
    M-x kodi-remote

### starts playback of the youtube video
    M-x kodi-remote-play-video-url RET <paste youtube url> RET

### starts playback of the youtube video over the kodi-youtube plugin
    M-x kodi-remote-play-stream-url RET <paste youtube url> RET
	(advantage is that it works also with streamed videos)

### starts music playback in party mode
    M-x kodi-remote-play-music RET

### opens the kodi video playlist
    M-x kodi-remote-playlist RET

## Contributing

Yes, please do! See [CONTRIBUTING][] for guidelines.

## License

See [COPYING][]. Copyright (c) 2015 Stefan Huchler.


[CONTRIBUTING]: ./CONTRIBUTING.md
[COPYING]: ./COPYING
