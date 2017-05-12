.. _network:

Network
=====================
You can disable or enable network cache using the following commands: ::
  
  M-x indium-webkit-disable-cache
  M-x indium-webkit-enable-cache

Both commands save your choice which will be used for future Indium connections for the current Emacs session.

You can make the cache setting permament by setting `indium-webkit-cache-disabled`: ::
  
  (setq indium-webkit-cache-disabled t)
