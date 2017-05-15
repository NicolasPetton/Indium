#!/bin/sh -e
TRAVIS=true cask exec buttercup -l test/test-helper.el  -L . "$@"
