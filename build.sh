#!/bin/sh
SAVE_FILE=${0:-~/.local/bin/down-flash-video}
ccl -K utf-8 -e "(progn (load (compile-file (load \"down-flash-video.lisp\")))(defparameter *save-file* \"$SAVE_FILE\")(load \"build.lisp\"))"
