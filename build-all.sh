#!/bin/sh

set -e

sbcl --load "build-helper.lisp" --eval "(build-app)" --eval "(exit)"
sbcl --load "build-helper.lisp" --eval "(build-core)" --eval "(exit)"
