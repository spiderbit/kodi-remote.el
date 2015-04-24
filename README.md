# kodi-remote

## Summary

Emacs Remote Control functions for Emacs

## Installing

### clone kodi remote
    cd ~/.emacs/
    git clone <repo-url>

### add the path of kodi-remote and to your emacs init file:
    (add-to-list 'load-path "~/.emacs/kodi-remote")
    (require 'kodi-remote)

you need a running kodi instance, at the moment hostname and port is hardcoded to localhost:9090 but you can change the url strings in the elisp file easily.

## Using

### starts playback of the youtube video
    M-x kodi-remote-play-youtube-url RET <paste youtube url> RET
or

    M-x kodi-remote-play-youtube-id RET <paste youtube video id> RET

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
