.. _listing-scripts:

Listing parsed scripts
======================

Indium includes the command ``indium-list-scripts`` to list all JavaScript
scripts parsed by the runtime.  When using a workspace, local file can be
visited from entries in the list (see :ref:`local-files`).

Using the listing buffer
------------------------

Here is a list of available keybindings in an script listing buffer:

.. tabularcolumns:: |r|L|
                   
+--------------------+---------------------------------------------------------------------------------+
| Keybinding         | Description                                                                     |
+====================+=================================================================================+
| ``RET``            | Follow a link open the local file associated with the script                    |
+--------------------+---------------------------------------------------------------------------------+
| ``g``              | Update the listing buffer                                                       |
+--------------------+---------------------------------------------------------------------------------+
| ``n`` or ``TAB``   | Jump to the next script                                                         |
+--------------------+---------------------------------------------------------------------------------+
| ``p`` or ``s-TAB`` | Jump to the previous script                                                     |
+--------------------+---------------------------------------------------------------------------------+

