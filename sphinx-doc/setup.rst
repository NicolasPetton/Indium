
.. _setup:

Getting up and running
======================

.. _configuration_file:

Project configuration
---------------------

Place a ``.indium.json`` file in the root folder of your JavaScript project.
The project file can contain one or many configurations settings for NodeJS (see
:ref:`nodejs_configuration`) and Chrome/Chromium (see
:ref:`chrome_configuration`).

Here is a minimalist ``.indium.json`` file.::
     
    {
      "configurations": [
        {
	  "name": "Web project",
	  "type": "chrome"
	}
      ]
    }

.. _general_configuration:

General configuration
---------------------

The ``.indium.json`` file can contain as many configurations as needed, and mix
any supported configuration types.

The currently supported ``type`` values are ``"chrome"`` and ``"node"``.

The root directory of the source files is by default set to the directory where
this ``.indium.json`` file is placed, but it can be overridden with the ``root``
(or the ``webRoot`` alias) option::

  {
    "configurations": [
      {
        "type": "chrome",
	"root": "src"
      }
    ]
  }

Custom script path overrides can be set with ``scriptPathOverrides``.  See
:ref:`scriptpaths` for more information on script paths and debugging.

Custom sourcemap path overrides can be set with ``sourceMapPathOverrides``.  See
:ref:`sourcemaps` for more information on sourcemaps and debugging.

.. _chrome_configuration:

Chrome/Chromium configuration options
-------------------------------------

:host: Host on which Chrome is running (defaults to ``"localhost"``).
:port: Port on which Chrome is running (defaults to ``9222``).
:url: Url to open when running ``indium-launch``.


Example configuration::
  
    {
      "configurations": [
        {
	  "name": "Web project",
	  "type": "chrome",
	  "host": 192.168.22.1,
	  "url": "http://192.168.22.1/myproject/index.html",
	  "port": 9222
	}
      ]
    }

.. _nodejs_configuration:

NodeJS configuration options
----------------------------

:command:
   Nodejs command to start a new process.  The ``--inspect`` flag will be
   added automatically.
	   
:inspect-brk:
   Whether Indium should break at the first statement (false by
   default).

:host:
   Host on which the Node inspector is listening (defaults to ``"localhost"``).
       
:port:
   Port on which the Node inspector is listening (defaults to 9229).

Here is an example configuration for debugging Gulp tasks::

  {
    "configurations": [
      {
        "name": "Gulp",
        "type": "node",
        "command": "node ./node_modules/gulp/bin/gulp.js",
        "inspect-brk": true
      }
    ]
  }

.. _starting_indium:
     
Starting Indium
---------------

Indium can be started in two modes:

- Connect: ``M-x indium-connect`` Connect indium to a running runtime from one
  of the configurations in the ``.indium.json`` project file.
- Launch: ``M-x indium-launch`` Start a JavaScript process (Chrome or NodeJS) as
  specified from the configurations in the ``.indium.json`` project file.

.. _nodejs_requirements:

NodeJS requirements
-------------------

Nodejs >= ``8.x`` is required for Indium to work. 

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

.. _chrome_requirements:

Chrome/Chromium requirements
----------------------------

Chrome/Chromium >= ``60.0`` is required for Indium to properly work (debugging
protocol ``v1.2``).

When the variable ``indium-chrome-use-temporary-profile`` is non-nil (the
default), ``M-x indium-launch`` will start a new instance of Chrome/Chromium
with the remote debugging port set up.

Otherwise, you can start Chrome/Chromium with the ``--remote-debugging-port``
flag like the following: ::
  
  chromium --remote-debugging-port=9222 https://localhost:3000

If you start Chrome manually, make sure that no instance of Chrome is already
running, otherwise Chrome will simply open a new tab on the existing Chrome
instance, and the ``remote-debugging-port`` will not be set.
