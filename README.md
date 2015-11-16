# kodi-remote

## Summary

Emacs Remote Control functions for Kodi including a function to play directly videos from youtube and other sites see youtube-dl for supported sites. 

## Requirements

	to use the video website play feature you must install youtube-dl on your machine.

## Installing

### clone kodi remote
    cd ~/.emacs/
    git clone <repo-url>

### add the path of kodi-remote and to your emacs init file:
    (add-to-list 'load-path "~/.emacs/kodi-remote")
    (require 'kodi-remote)
    (setq kodi-host-name "my-htpc:9090")

you need a running kodi instance of course!

## Using

### starts playback of the youtube video
    M-x kodi-remote-play-video-url RET <paste youtube url> RET

### starts music playback in party mode
    M-x kodi-remote-play-music RET

### toggles play/pause
    M-x kodi-remote-play-pause RET

## Contributing

Yes, please do! See [CONTRIBUTING][] for guidelines.

## License

See [COPYING][]. Copyright (c) 2015 Stefan Huchler.


[CONTRIBUTING]: ./CONTRIBUTING.md
[COPYING]: ./COPYING
