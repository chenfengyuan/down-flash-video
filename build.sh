#!/bin/sh
cd `echo $0|sed 's;[^/]\+/\?$;;'`
SAVE_FILE=${1:-~/.local/bin/down-flash-video}
ccl -K utf-8 -e "(progn (load (compile-file (load \"down-flash-video.lisp\")))(defparameter *save-file* \"$SAVE_FILE\")(load \"build.lisp\"))"
