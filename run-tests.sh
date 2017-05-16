#!/bin/sh

set -e

if [ -f /tmp/undercover_coveralls_report  ]; then
    rm /tmp/undercover_coveralls_report
fi

TRAVIS=true cask exec buttercup -l test/test-helper.el  -L . "$@"
