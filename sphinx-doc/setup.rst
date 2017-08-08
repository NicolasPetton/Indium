.. _up-and-running:

Getting up and running
======================

.. _nodejs:

NodeJS
------

Nodejs >= ``8.x`` is required for Indium to work. 

Installing a recent version of NodeJS
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If your distribution ships an old version of NodeJS, you can install a more
recent version using `nvm <https://github.com/creationix/nvm>`_: ::

  $ curl -o- https://raw.githubusercontent.com/creationix/nvm/v0.33.2/install.sh | bash

Once ``nvm`` is install, you can easily install and use the version of NodeJS
you want: ::
  
  $ nvm install v8
  $ nvm alias default v8
  $ node --version

If you install NodeJS using ``nvm``, chances are that Emacs won't have it in its
``exec path``. A simple solution is to use the excellent `exec-path-from-shell
<https://github.com/purcell/exec-path-from-shell>`_ package.
  
Executing NodeJS from Emacs
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Indium can start NodeJS processes and connect to them automatically.
This is the preferred way of using Indium with NodeJS.

Start a process with ``M-x indium-run-node``.  Once the process is ready, Indium
will connect to it and open a REPL buffer.

The output from the NodeJS process is appended to the ``*nodejs process*`` buffer.

.. NOTE:: Indium will append the ``--inspect`` and ``--debug-brk`` flags to the
          command-line arguments automatically, so you do not need to provide
          them.

Connecting to a Nodejs process
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To connect to an existing NodeJS process, make sure that process was started
with the ``--inspect`` flag: ::

    node --inspect myfile.js
    
If you wish to break on the first line of the application code, start node using: ::

    node --inspect --debug-brk myfile.js
    
Node will tell you to open an URL in Chrome: ::

    chrome-devtools://inspector.html?...&ws=127.0.0.1:PORT/PATH
    
Evaluate ``M-x indium-connect-to-nodejs RET 127.0.0.1 RET PORT RET PATH``,
``PORT`` and ``PATH`` are the ones from the `ws` parameter of the above URL.

Connecting Indium to the node process will open a debugger on the first line of
the application code if you passed the CLI argument ``--debug-brk``.


.. _chrome:

Chrome/Chromium
---------------

Chrome/Chromium >= ``60.0`` is required for Indium to properly work (debugging
protocol ``v1.2``).

Start Chrome/Chromium with the ``--remote-debugging-port`` flag like the following:
::
  
  chromium --remote-debugging-port=9222 https://localhost:3000

Make sure that no instance of Chrome is already running, otherwise Chrome will
simply open a new tab on the existing Chrome instance, and the
``remote-debugging-port`` will not be set.
  
To connect to a tab, run from Emacs: ::

  M-x indium-connect-to-chrome

.. _local-files:
  
Using local files when debugging
--------------------------------

Indium can use local files when debugging, or to set breakpoints.

.. HINT:: When using ``NodeJS``, or when the connected tab uses the ``file://``
          URL, Indium will by itself use local files from disk.  In this case
          there is nothing to setup.

   
If the Chrome connection uses the ``http://`` or ``https://`` protocol, you will
have to tell Indium where to find the corresponding JavaScript files on disk by
setting up a workspace.

To do that, place an empty ``.indium`` marker file in the root folder where your
**web server serves static files**.

The root folder where the ``.indium`` file should be put is not always the
directory that contains your JavaScript files. It should be the root folder
containing static files. Most of the time, it is at least one level above.

Given the following project structure: ::

   project/ (current directory)
      www/
         index.html
         css/
            style.css
         js/
            app.js
         .indium

Indium will lookup the file ``www/js/app.js`` for the URL
"http://localhost:3000/js/app.js".

.. WARNING:: In order for this setup to work, make sure to call
            ``indium-connect-to-chrome`` from somewhere within the workspace
            directory!
