Troublehooting
==============

If you run into issues with Indium, this document might help you.

General advices before reporting issues
---------------------------------------

Issues should be reported on the `GitHub issue tracker
<https://github.com/nicolaspetton/indium/issues>`_.

1. If you encounter errors, you can enable ``debug-on-error`` in Emacs using ``M-x
toggle-debug-on-error`` and report an issue with the backtrace.

2. It is also a good idea to turn on Indium's log mode with ``M-: (setq
indium-client-debug t)``, and attach to the issue report the contents of the
``*indium-debug-log*`` to help resolve the issue.

Attaching the contents of the ``*indium-process*`` buffer can help as well in
case an error happens in the server process.

Breakpoints are not set (not using sourcemaps)
----------------------------------------------

If breakpoints do not work, chances are that the project is not configured
correctly.

.. NOTE:: Indium needs to know how to map script source urls to files on disk.
          It uses the ``root`` (alias ``webRoot``) configuration option as the
          base path, as described in the :ref:`general_configuration` page.

Indium provides a command ``indium-list-scirpt-sources`` to list all scripts
parsed by the backend, displaying their sources mapped to files on disk.  Check
that the file where you're trying to add a breakpoint is listed.

Breakpoints and debugging do not work (using sourcemaps)
--------------------------------------------------------

Correctly mapping sourcemaps to file locations can be tedious (see
:ref:`sourcemaps`).


Indium provides the command ``indium-list-sourcemap-sources`` to help
configuring sourcemaps correctly.  This commands displays a list of all
sourcemap sources in the runtime as file paths on disk.  Check that your files
are listed there.
