all: test

test: prepare
	emacs -batch -l ert -l tests/kodi-remote-tests.el -f ert-run-tests-batch-and-exit
	rm -rf tests/HOME/.kodi

# interactive-test:
# 	emacs -l ert -l tests/kodi-remote-tests.el -f ert
# 	# rm -rf tests/HOME/.kodi

prepare:
	cp -r tests/mocks/.kodi tests/HOME
	sed -i -e 's|$$GITROOT|'$(PWD)'|g' tests/HOME/.kodi/userdata/sources.xml
	HOME=$(PWD)/tests/HOME kodi &

# /nix/store/vr17xkggj63x4yw7gnd044z4ylpdkbmg-kodi-16.1/bin/
