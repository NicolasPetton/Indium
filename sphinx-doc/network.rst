.. _network:

Network
=====================
You can disable or enable network cache using the following commands: ::
  
  M-x indium-v8-disable-cache
  M-x indium-v8-enable-cache

Both commands save your choice which will be used for future Indium connections for the current Emacs session.

You can make the cache setting permament by setting `indium-v8-cache-disabled`: ::
  
  (setq indium-v8-cache-disabled t)
