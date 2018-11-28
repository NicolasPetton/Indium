Installation
============

.. NOTE:: If you already have installed ``Jade``, you should read the
          :ref:`migration-from-jade` page first.

Indium supports ``Emacs 25.3+``, works with ``Chrome`` (debugging protocol
``v1.2``, see :ref:`chrome_requirements`) and ``NodeJS``, see
:ref:`nodejs_requirements`.

Indium works with ``js-mode``, ``js2-mode``, ``js2-jsx-mode`` and
``rjsx-mode``. It supports the ECMAScript features of the runtime it connects
to.

Indium is availabe on `MELPA <https://melpa.org>`_, `MELPA Stable
<https://stable.melpa/org>`_.

.. _server_installation:

The Indium server
-----------------

Indium needs to communicate with a small server for evaluation and debugging.
Install the server with the following command (prepend ``sudo`` on GNU/Linux):
::

  npm install -g indium

Using MELPA
-----------

Unless you are already using MELPA, you will have to setup ``package.el`` to use
MELPA or MELPA Stable repositories.  You can follow `this documentation
<https://melpa.org/#/getting-started>`_.

You can install Indium with the following command: ::
  
  M-x package-install [RET] indium [RET]

or by adding this bit of Emacs Lisp code to your Emacs initialization file
(``.emacs`` or ``init.el``): ::

  (unless (package-installed-p 'indium)
    (package-install 'indium))

If the installation doesn't work try refreshing the package list: ::

  M-x package-refresh-contents [RET]

Manual installation
-------------------

If you want to install Indium manually, make sure to install ``websocket.el``.
Obtain the code of Indium `from the repository
<https://github.com/NicolasPetton/indium>`_.

Add the following to your Emacs configuration: ::

  ;; load Indium from its source code
  (add-to-list 'load-path "~/projects/indium")
  (require 'indium)
