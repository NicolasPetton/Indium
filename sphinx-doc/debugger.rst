.. _debugger:

The stepping debugger
=====================

.. _sourcemaps:

Using sourcemaps
----------------

Since version 0.7, Indium uses sourcemap files by default.

For sourcemaps to work properly with Chrome/Chromium, make sure that a
workspace is correctly set (see :ref:`local-files`).

If you wish to disable sourcemaps when debugging, set ``indium-script-enable-sourcemaps`` to ``nil``.

.. Note:: Only sourcemap files specified with ``sourceMappingURL`` are currently
          supported, inlined sourcemaps are not yet supported.
