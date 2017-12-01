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

.. _webpack:

Sourcemaps and Webpack
----------------------

When using Webpack to bundle JavaScript files, Indium will only be able to use
sourcemaps if Webpack is configured to emit absolute file paths.

Here is an example configuration snippet to be inserted in `webpack.config.json` ::

   ...
   output : {
     ...
     devtoolModuleFilenameTemplate: '[absolute-resource-path]',
     devtoolFallbackModuleFilenameTemplate: '[absolute-resource-path]?[hash]'
   }
