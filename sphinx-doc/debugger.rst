.. _debugger:

The stepping debugger
=====================

.. _sourcemap:
Using sourcemaps
----------------

Since version 0.7, Indium uses sourcemap files by default.

If you wish to disable sourcemaps when debugging, set ``indium-script-enable-sourcemaps`` to ``nil``.

.. Note:: Only sourcemap files specified with ``sourceMappingURL`` are currently
          supported, inlined sourcemaps are not yet supported.
