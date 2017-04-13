# Indium [![MELPA](https://melpa.org/packages/indium-badge.svg)](https://melpa.org/#/indium) [![Build Status](https://travis-ci.org/NicolasPetton/Indium.svg?branch=master)](https://travis-ci.org/NicolasPetton/Indium) [![Documentation Status](https://readthedocs.org/projects/indium/badge/?version=latest)](http://indium.readthedocs.io/en/latest/?badge=latest) <img align="right" src="img/indium.png" alt="logo">

A JavaScript development environment for Emacs.

Indium connects to a browser tab or nodejs process and provides many features for
JavaScript development, including:

- a REPL (with auto completion) & object inspection;
- an inspector, with history and navigation;
- a scratch buffer (`M-x indium-scratch`);
- JavaScript evaluation in JS buffers with `indium-interaction-mode`;
- a stepping Debugger, similar to `edebug`, or `cider`.

![REPL](./screenshots/debugger.gif)

**Installation instruction and other documentation can be found on [readthedocs](https://indium.readthedocs.io).**

## Screenshots

The REPL with company-mode

![REPL](./screenshots/repl.png)

The REPL showing clickable output

![REPL](./screenshots/repl2.png)

The inspector

![REPL](./screenshots/inspector.png)

The stepping debugger, and an inspector on locals

![REPL](./screenshots/debugger.png)

## Missing features

Indium is young, here's a list of missing/wanted features

- ~~Code evaluation using `C-x C-e` from project JS buffers~~
- ~~Adding breakpoints (to remove the need for `debugger` statements)~~
- Network inspector (could get inspiration from restclient.el)
- DOM inspector
- Start processes (~~chromium~~, node, grunt, gulp, etc.) from Emacs and attach a
  indium connection to it.

## Backends

There is currently support for Chrom[e|ium] & nodejs.  The firefox backend is
in the TODO list.


