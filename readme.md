# Jade - JavaScript IDE in Emacs

Jade connects to a browser tab or nodejs process and provides many features for
JavaScript development, including a REPL (with auto completion) & object
inspection, an inspector, with history and navigation, and even a stepping
Debugger, similar to `edebug`, or `cider`.

## Screenshots

The REPL with company-mode

![REPL](./screenshots/repl.png)

The inspector

![REPL](./screenshots/inspector.png)

The stepping debugger, and an inspector on locals

![REPL](./screenshots/debugger.png)

## Getting started

(Installation instructions missing.)

### Connection to a chrom[e|ium] tab

Start chrom[e|ium] with the `--remote-debugging-port` flag:

    chromium --remote-debugging-port=9222 https://gnu.org
    
Evaluate `M-x jade-connect-to-chrome`.

### Connection to a nodejs process

(Note: as of July 31 2016, this requires node-nightly, which can be installed with `npm install -g node-nightly`).

Start a node process with the `--listen` fla:

    node --listen myfile.js

Evaluate `M-x jade-connect-to-nodejs`.

## Missing features

Jade is young, here's a list of missing/wanted features

- Code evaluation using `C-M-x` from project JS buffers
- Adding breakpoints (to remove the need for `debugger` statements)
- Network inspector (could get inspiration from restclient.el)
- DOM inspector
- Start processes (chromium, node, grunt, gulp, etc.) from Emacs and attach a
  jade connection to it.

## Backends

There is currently support for Chrom[e|ium], & nodejs.  The firefox backend is
currently missing.


