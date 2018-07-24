.. _debugger:

The stepping debugger
=====================

.. _sourcemaps:

Using sourcemaps
----------------

Since version 0.7, Indium uses sourcemap files by default.

For sourcemaps to work properly with Chrome/Chromium, make sure that a
workspace is correctly set (see :ref:`setup`).

If you wish to disable sourcemaps when debugging, set ``indium-script-enable-sourcemaps`` to ``nil``.

.. _webpack:

Overriding sourcemap paths
~~~~~~~~~~~~~~~~~~~~~~~~~~

Some sourcemaps cannot be used as is and need path rewriting to map to locations on disks.

Indium provides the configuration option ``sourceMapPathOverrides`` for
providing custom sourcemap paths.

The default mapping works well for Webpack projects::
  
   {
     "webpack:///./~/": "${root}/node_modules/",
     "webpack:///./":   "${root}/",
     "webpack:///":     "/",
     "webpack:///src/": "${root}/"
   }

Overriding the ``sourceMapPathOverrides`` option will erase the default mapping.

Blackboxing scripts
-------------------

The custom variable ``indium-debugger-blackbox-regexps`` holds a list of regular
expression of script paths to blackbox when debugging.

Blackboxed scripts are skipped when stepping in the debugger.
