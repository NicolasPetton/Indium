Installation
============

.. NOTE:: If you already have installed ``Jade``, you should read the
          :ref:`migration-from-jade` page first.

Indium supports ``Emacs 25.1+``, ``Chrome 54.0+`` (debugging protocol ``v1.2``)
and ``NodeJS 7+``.

Indium is availabe on `MELPA <https://melpa.org>`_, `MELPA Stable
<https://stable.melpa/org>`_.

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
