.. _debugger:

The stepping debugger
=====================

.. _sourcemaps:

Using sourcemaps
----------------

Since version 0.7, Indium uses sourcemap files by default.

For sourcemaps to work properly with Chrome/Chromium, make sure that a
workspace is correctly set (see :ref:`setup`).

.. Warning:: If your project uses sourcemaps, we advise you to use ``js-mode``
             with ``js2-minor-mode`` instead of ``js2-mode``.  ``js2-mode`` can
             be extremely slow at parsing large files (like compiled JavaScript
             files) that the debugger might open if a stack frame source is not
             source-mapped.  This can happen for instance when using Webpack.


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

.. TIP:: If sourcemaps do not seem to work, you can see how Indium resolves
          sourcemap paths using ``M-x indium-list-sourcemap-sources``.

.. _remoteroot:

Setting a different remote root (NodeJS)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When running a NodeJS application on a remote machine or inside a Docker
container, your application's root folder path might not correspond to where
their source code is located on your local disk.

In this case, you can tell Indium to replace the ``root`` path with a different
location using the ``remoteRoot`` configuration option.

   {
     ...
     "remoteRoot": "/var/task"
   }

Note that ``remoteRoot`` can be used together with the ``root`` configuration
option.

	  
Blackboxing scripts
-------------------

The custom variable ``indium-debugger-blackbox-regexps`` holds a list of regular
expression of script paths to blackbox when debugging.

Blackboxed scripts are skipped when stepping in the debugger.
