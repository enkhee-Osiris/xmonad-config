#!/bin/sh

# Screensaver
if [ -z "$(pgrep xscreensaver)" ] ; then
   xscreensaver -no-splash &
fi
# Xcompozer
if [ -z "$(pgrep clipster)" ] ; then
   clipster -d &
fi

# Background image
if [ -z "$(pgrep xloadimage)" ] ; then
  xloadimage -border black -fullscreen -onroot  ~/.xmonad/wall.jpg
fi
