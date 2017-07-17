.. _debugger:

The stepping debugger
=====================

.. _sourcemap:
Using sourcemaps
----------------

Since version 0.7, Indium uses sourcemap files by default when provided.

If you wish to disable sourcemaps when debugging, set ``indium-script-enable-sourcemaps`` to ``nil``.

.. Note:: Only sourcemaps specified in separate files are currently supported,
          inlined sourcemaps in minified files are not yet supported.
