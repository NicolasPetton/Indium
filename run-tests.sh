#!/bin/sh -e
cask exec buttercup -l test/test-helper.el  -L . "$@"
