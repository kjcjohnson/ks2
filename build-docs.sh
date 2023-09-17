#!/bin/sh

set -e

sbcl --load "build-helper.lisp" --eval '(build-docs "docs/ks2.md")' --eval "(exit)"
