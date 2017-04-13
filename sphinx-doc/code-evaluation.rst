Interaction in JS buffers
=========================

Indium comes with a minor mode called ``indium-interaction-mode`` for interactive
programming. To enable it in all JavaScript buffers, add something
like the following to your Emacs configuration: ::

  (require 'indium)
  (add-hook 'js-mode-hook #'indium-interaction-mode)

When ``indium-interaction-mode`` is on, you can evaluate code, inspect objects and
add or remove breakpoints from your buffers.

Evaluating and inspecting
-------------------------

To evaluate the JavaScript expression preceding the point, press ``C-x C-e``.

To inspect the result of an expression, press ``C-c M-i`` (see :ref:`inspector`).

The entire buffer can be evaluating with ``M-x indium-eval-buffer``.

Switching to the REPL buffer
----------------------------

Press ``C-c C-z`` from any buffer with ``indium-interaction-mode`` turned on to
switch back to the REPL buffer (see :ref:`repl`).

Adding and removing breakpoints
-------------------------------

You need to first make sure that Indium is set up correctly to use local files
(see :ref:`local-files`).

- ``C-c b b``: Toggle a breakpoint
- ``C-u C-c b b``: Toggle a breakpoint with condition
- ``C-c b K``: Remove all breakpoints from the current buffer

Once a breakpoint is set, execution will stop when a breakpoint is hit, and the
Indium debugger pop up (see :ref:`debugger`).
  
.. Note:: Breakpoints are persistent: if the connection is closed, when a new
          connection is made Indium will attempt to add back all breakpoints.

.. Warning:: When modifying JavaScript buffers, it is up to the user to update
             the browser tab.  Indium will attempt to update breakpoint locations
             when saving a buffer, but the runtime script contents won't be
             updated automatically.  You can either refresh the page or evaluate
             ``indium-reload``.
             
