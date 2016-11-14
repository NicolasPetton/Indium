# Jade [![MELPA](https://melpa.org/packages/jade-badge.svg)](https://melpa.org/#/jade) [![Build Status](https://ci.petton.fr/api/badges/nico/jade/status.svg)](https://ci.petton.fr/nico/jade)

_**J**avaScript **A**wesome **D**evelopment **E**nvironment (in Emacs)_

Jade connects to a browser tab or nodejs process and provides many features for
JavaScript development, including:

- a REPL (with auto completion) & object inspection;
- an inspector, with history and navigation;
- a scratch buffer (`M-x jade-scratch`);
- JavaScript evaluation in JS buffers with `jade-interaction-mode`;
- a stepping Debugger, similar to `edebug`, or `cider`.

## Screenshots

The REPL with company-mode

![REPL](./screenshots/repl.png)

The REPL showing clickable output

![REPL](./screenshots/repl2.png)

The inspector

![REPL](./screenshots/inspector.png)

The stepping debugger, and an inspector on locals

![REPL](./screenshots/debugger.png)

## Getting started

(Emacs 25 is required)

It's available on melpa:

    M-x package-install jade

### Connection to a chrom[e|ium] tab

Chrom[e|ium] >= 54.0 is required for Jade to properly work (debugging protocol
v1.2).

Start chrom[e|ium] with the `--remote-debugging-port` flag:

    chromium --remote-debugging-port=9222 https://gnu.org
    
Evaluate `M-x jade-connect-to-chrome`.

### Connection to a nodejs process

Nodejs >= 6.9.1 is required for Jade to work. 

Start a node process with the `--inspect` flag:

    node --inspect myfile.js

Evaluate `M-x jade-connect-to-nodejs`.

If you wish to break on the first line of the application code, start node using:

    node --inspect --debug-brk myfile.js

Connecting Jade to the node process will open a debugger on the first line of the application code.

### JavaScript evaluation in JS buffers

Add the following to enable evaluation in all JS buffers:

    (add-hook 'js2-mode-hook #'jade-interaction-mode)
    
Then `C-x C-e` will evaluate the node before the point, and `C-c M-i` will
inspect the result.

The entire buffer can also be executed with `jade-eval-buffer`.

## Missing features

Jade is young, here's a list of missing/wanted features

- ~~Code evaluation using `C-x C-e` from project JS buffers~~
- Adding breakpoints (to remove the need for `debugger` statements)
- Network inspector (could get inspiration from restclient.el)
- DOM inspector
- Start processes (~~chromium~~, node, grunt, gulp, etc.) from Emacs and attach a
  jade connection to it.

## Backends

There is currently support for Chrom[e|ium], & nodejs.  The firefox backend is
currently missing.


