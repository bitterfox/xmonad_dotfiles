
PACKAGER =
PACKAGER_COMMAND = install
SH = bash
SETUPER = setup.sh
PACKAGES = xmonad xmobar ghc libghc-parsec3-dev libghc-split-dev dmenu gmrun trayer gnome-control-center gnome-settings-daemon network-manager-gnome

all:
	$(PACKAGER) $(PACKAGER_COMMAND) $(PACKAGES)
	$(SH) $(SETUPER)
