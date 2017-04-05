Interaction in JS buffers
=========================

Jade comes with a minor mode called ``jade-interaction-mode`` for interactive
programming. To enable it in all JavaScript buffers, add something
like the following to your Emacs configuration: ::

  (require 'jade)
  (add-hook 'js-mode-hook #'jade-interaction-mode)

When ``jade-interaction-mode`` is on, you can evaluate code, inspect objects and
add or remove breakpoints from your buffers.

Evaluating and inspecting
-------------------------

To evaluate the JavaScript expression preceding the point, press ``C-x C-e``.

To inspect the result of an expression, press ``C-x M-i``.

The entire buffer can be evaluating with ``M-x jade-eval-buffer``.

Switching to the REPL buffer
----------------------------

Press ``C-c C-z`` from any buffer with ``jade-interaction-mode`` turned on to
switch back to the REPL buffer.

Adding and removing breakpoints
-------------------------------

You need to first make sure that Jade is set up correctly to use local files
(see :ref:`local-files`).

- ``C-c b b``: Toggle a breakpoint
- ``C-u C-c b b``: Toggle a breakpoint with condition
- ``C-c b K``: Remove all breakpoints from the current buffer

.. Note:: Breakpoints are persistent: if the connection is closed, when a new
          connection is made Jade will attempt to add back all breakpoints.
