.. _repl:

The REPL
========

Starting a REPL
---------------

A REPL (Read Eval Print Loop) buffer is automatically open when a new Indium
connection is made (see :ref:`up-and-running`).

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

Using Promises or callbacks in the REPL buffer
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

When your code is stopped at a breakpoint the `Node.js event loop
<https://nodejs.org/en/docs/guides/event-loop-timers-and-nexttick/>`_ is no
longer running.

If you use the REPL buffer while the debugger is stopped to invoke a Promise or
callback then you will not get any response until you allow your code to
continue. The debugger must continue to allow the event loop a chance to run and
this may not always be possible at certain points in your code.

Keeping the REPL around at the end of the NodeJS process
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
 
If your application file doesn't already have some sort of loop handling
(perhaps its a selenium test file) then there is no way to halt at the end of
the file to inspect the values of variables or perhaps invoke some experimental
code during runtime.

You will need to add your own debug REPL loop to the end of the application.
Without this loop you will not be able to receive the responses of Promises or
callbacks (see above) as the event loop will not run.

This example is for selenium. It attaches a callback to run when all tests have
been completed and runs our debugReplLoop. Remember you will need to close over
any variables that you want to access from the REPL buffer or else they will not
be in the context of the debugReplLoop.

Now you can invoke code in the REPL buffer and continue the code to allow the
event loop to process.

  const debugReplLoop = () => {
    const closeOverVariables = {
      // Any variables you need access to in the REPL buffer
    };

    // stop and wait for continue force recompile
    debugger;
    setImmediate(debugReplLoop);
  };

  test.onFinish(() => {
    setImmediate(debugReplLoop);
  });
