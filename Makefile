all: test

test: prepare
	HOME=$(PWD)/tests/HOME kodi &
	emacs -batch -l ert -l tests/kodi-remote-tests.el -f ert-run-tests-batch-and-exit
	rm -rf tests/HOME/.kodi

# interactive-test:
# 	emacs -l ert -l tests/kodi-remote-tests.el -f ert
# 	# rm -rf tests/HOME/.kodi

prepare:
	cp -r tests/mocks/.kodi tests/HOME
	sed -i -e 's|$$GITROOT|'$(PWD)'|g' tests/HOME/.kodi/userdata/sources.xml

open: prepare
	HOME=$(PWD)/tests/HOME kodi

# http://freemusicarchive.org/music/Ash_Turner/Beautiful_Open_Spaces_Trio_Tracks
# would need a parser to get the changing download url
BeautifulOpenSpacesTrioTracks.zip:
	wget https://freemusicarchive.org/music/zip/d5c9d6e896755ae7ff0fec0211f61bedca4a16bf -O BeautifulOpenSpacesTrioTracks.zip

extract_mock: BeautifulOpenSpacesTrioTracks.zip
	unzip -o BeautifulOpenSpacesTrioTracks.zip -d tests/mocks/.kodi/media/music/

tests/mocks/.kodi/media/music/%.mp3: extract_mock
	ffmpeg -t 2 -i $@ -map 0:a -codec:a copy -map_metadata -1 -acodec libmp3lame -ac 2 -ab 65k -ar 44100 $@ -y -v 0

generate_mock: tests/mocks/.kodi/media/music/*.mp3
	ls

# /nix/store/vr17xkggj63x4yw7gnd044z4ylpdkbmg-kodi-16.1/bin/
