.. _inspector:

The inspector
=============

Indium features an object inspector that can be open on any object reference
from a REPL buffer (see :ref:`repl`), the debugger (see :ref:`debugger`), or
the result of any evaluation of JavaScript code (see :ref:`interaction`).

To inspect the result of the evaluation of an expression, press ``C-c M-i``.  An
inspector buffer will pop up.  You can also press ``RET`` or left click on
object links from the REPL buffer.

.. image:: screenshots/inspector.png

Using the inspector
-------------------

Here is a list of available keybindings in an inspector buffer:

.. tabularcolumns:: |r|L|
                   
+--------------------+---------------------------------------------------------------------------------+
| Keybinding         | Description                                                                     |
+====================+=================================================================================+
| ``RET``            | Follow a link to inspect the object at point                                    |
+--------------------+---------------------------------------------------------------------------------+
| ``l``              | Pop to the previous inspected object                                            |
+--------------------+---------------------------------------------------------------------------------+
| ``g``              | Update the inspector buffer                                                     |
+--------------------+---------------------------------------------------------------------------------+
| ``n`` or ``TAB``   | Jump to the next object in the inspector                                        |
+--------------------+---------------------------------------------------------------------------------+
| ``p`` or ``s-TAB`` | Jump to the previous object in the inspector                                    |
+--------------------+---------------------------------------------------------------------------------+

