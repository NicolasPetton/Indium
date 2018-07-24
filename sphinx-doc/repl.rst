.. _repl:

The REPL
========

Starting a REPL
---------------

A REPL (Read Eval Print Loop) buffer is automatically open when a new Indium
connection is made (see :ref:`setup`).

.. image:: screenshots/repl.png

The REPL offers the following features:

* Auto completion with ``company-mode``
* JS syntax highlighting
* Pretty printing and preview of printed values
* Access to the object inspector (see :ref:`inspector`)

.. image:: screenshots/repl2.png
  
Using the REPL
--------------

Keybindings
^^^^^^^^^^^

Here is the list of available keybindings in a REPL buffer:

.. tabularcolumns:: |r|L|
                   
+-------------+----------------------------------------------------------------------------------------+
| Keybinding  | Description                                                                            |
+=============+========================================================================================+
| ``RET``     | Evalute the current input.  When the point is on a printed object, inspect the object. |
+-------------+----------------------------------------------------------------------------------------+
| ``C-RET``   | Insert a newline.                                                                      |
+-------------+----------------------------------------------------------------------------------------+
| ``C-c M-i`` | Evalute the current input and open an inspector on the result.                         |
+-------------+----------------------------------------------------------------------------------------+
| ``C-c C-o`` | Clear the output.                                                                      |
+-------------+----------------------------------------------------------------------------------------+
| ``C-c C-q`` | Kill the REPL buffer and close the current connection.                                 |
+-------------+----------------------------------------------------------------------------------------+
| ``M-n``     | Insert the previous input in the history.                                              |
+-------------+----------------------------------------------------------------------------------------+
| ``M-p``     | Instert the next input in the history.                                                 |
+-------------+----------------------------------------------------------------------------------------+

Reconnecting from the REPL buffer
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

When a connection is closed (most probably because other devtools were open on
the same runtime), the REPL will display two buttons, one to try to reopen the
connection, and another one to kill Emacs buffers using this connection (the
REPL buffer, inspectors & debuggers).
  
Code evaluation & context
-------------------------

When evaluating code in the REPL, Indium will always run the code on the current
execution context.

This means that while debugging, code execution will happen in the context of
the current stack frame, and will be able to access local variables from the
stack, etc.

