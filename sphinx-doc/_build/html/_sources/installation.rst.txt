Installation
============

Jade supports ``Emacs 25.1+``, ``Chrome 54.0+`` (debugging protocol ``v1.2``)
and ``NodeJS 7+``.

Jade is availabe on `MELPA <https://melpa.org>`_, `MELPA Stable
<https://stable.melpa/org>`_.

Using MELPA
-----------

Unless you are already using MELPA, you will have to setup ``package.el`` to use
MELPA or MELPA Stable repositories.  You can follow `this documentation
<https://melpa.org/#/getting-started>`_.

You can install Jade with the following command: ::
  
  M-x package-install [RET] jade [RET]

or by adding this bit of Emacs Lisp code to your Emacs initialization file
(``.emacs`` or ``init.el``): ::

  (unless (package-installed-p 'jade)
    (package-install 'jade))

If the installation doesn't work try refreshing the package list: ::

  M-x package-refresh-contents [RET]

Manual installation
-------------------

If you want to install Jade manually, make sure to install ``websocket.el``.
Obtain the code of Jade `from the repository
<https://github.com/NicolasPetton/jade>`_.

Add the following to your Emacs configuration: ::

  ;; load Jade from its source code
  (add-to-list 'load-path "~/projects/jade")
  (require 'jade)
