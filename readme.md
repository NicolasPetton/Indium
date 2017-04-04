# Jade [![MELPA](https://melpa.org/packages/jade-badge.svg)](https://melpa.org/#/jade) [![Build Status](https://travis-ci.org/NicolasPetton/jade.svg?branch=master)](https://travis-ci.org/NicolasPetton/jade)

_**J**avaScript **A**wesome **D**evelopment **E**nvironment (in Emacs)_

![REPL](./screenshots/debugger.gif)

Jade connects to a browser tab or nodejs process and provides many features for
JavaScript development, including:

- a REPL (with auto completion) & object inspection;
- an inspector, with history and navigation;
- a scratch buffer (`M-x jade-scratch`);
- JavaScript evaluation in JS buffers with `jade-interaction-mode`;
- a stepping Debugger, similar to `edebug`, or `cider`.

*Installation instruction and other documentation on readthedocs [here](https://jade.readthedocs.io).*

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

Jade is young, here's a list of missing/wanted features

- ~~Code evaluation using `C-x C-e` from project JS buffers~~
- ~~Adding breakpoints (to remove the need for `debugger` statements)~~
- Network inspector (could get inspiration from restclient.el)
- DOM inspector
- Start processes (~~chromium~~, node, grunt, gulp, etc.) from Emacs and attach a
  jade connection to it.

## Backends

There is currently support for Chrom[e|ium], & nodejs.  The firefox backend is
currently missing.


