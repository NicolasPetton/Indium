#!/bin/sh -e
cask exec ecukes "$@"
cask exec ert-runner -L . "$@"
