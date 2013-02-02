#!/bin/sh
cd `echo $0|sed 's;[^/]\+/\?$;;'`
SAVE_FILE=${1:-~/.local/bin/down-flash-video}
sbcl --eval "(progn (load \"down-flash-video.lisp\")(defparameter *save-file* \"$SAVE_FILE\"))" --eval "(compile-file \"down-flash-video.lisp\")" --eval "(load \"down-flash-video.lisp\")" --eval "(load \"build.lisp\")"
