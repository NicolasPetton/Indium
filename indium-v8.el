;;; indium-v8.el --- V8/Blink backend for indium  -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2018  Nicolas Petton

;; Author: Nicolas Petton <nicolas@petton.fr>
;; Keywords: tools, javascript

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Indium backend implementation for V8 and Blink.  Connection is handled in
;; indium-chrome.el and indium-nodejs.el.  This backend currently supports the
;; REPL, code completion, object inspection and the stepping debugger.
;;
;; Parts of the backend use the TOT (tip of tree) of the protocol, and may break
;; in the future.  When using the TOT, functions are flagged with "experimental
;; API".
;;
;; This backend supports both Chrome/Chromium 60 and Nodejs 8.x.
;;
;; The protocol is documented at
;; https://chromedevtools.github.io/debugger-protocol-viewer/1-2/.
;; https://chromedevtools.github.io/devtools-protocol/tot

;;; Code:

(require 'websocket)
(require 'json)
(require 'map)
(require 'seq)

(require 'indium-backend)
(require 'indium-structs)
(require 'indium-repl)
(require 'indium-debugger)
(require 'indium-workspace)
(require 'indium-script)
(require 'indium-breakpoint)

(defvar indium-v8-cache-disabled nil
  "Network cache disabled state.  If non-nil disable cache when Indium starts.")

(defvar indium-v8-completion-function "function getCompletions(type)\n{var object;if(type==='string')\nobject=new String('');else if(type==='number')\nobject=new Number(0);else if(type==='boolean')\nobject=new Boolean(false);else\nobject=this;var resultSet={};for(var o=object;o;o=o.__proto__){try{if(type==='array'&&o===object&&ArrayBuffer.isView(o)&&o.length>9999)\ncontinue;var names=Object.getOwnPropertyNames(o);for(var i=0;i<names.length;++i)\nresultSet[names[i]]=true;}catch(e){}}\nreturn resultSet;}")

(indium-register-backend 'v8)

(defun indium-connection-ws (connection)
  "Return the websocket associated to CONNECTION."
  (map-elt (indium-connection-props connection) 'ws))

(cl-defmethod (setf indium-connection-ws) (ws (connection indium-connection))
  (map-put (indium-connection-props connection) 'ws ws))

(defun indium-connection-nodejs-p (connection)
  "Return non-nil if CONNECTION is for Nodejs."
  (and connection
       (map-elt (indium-connection-props connection) 'nodejs)))

(cl-defmethod indium-backend-active-connection-p ((_backend (eql v8)))
  "Return non-nil if the current connection is active."
  (when-indium-connected
    (websocket-openp (indium-connection-ws indium-current-connection))))

(cl-defmethod indium-backend-close-connection ((_backend (eql v8)))
  "Close the websocket associated with the current connection."
  (websocket-close (indium-connection-ws indium-current-connection))
  (run-hooks 'indium-connection-closed-hook))

(cl-defmethod indium-backend-reconnect ((_backend (eql v8)))
  (let ((url (indium-current-connection-url))
	(ws-url (websocket-url (indium-connection-ws indium-current-connection))))
    ;; close all buffers related to the closed
    ;; connection the first
    (indium-quit)
    (indium-v8--open-ws-connection url ws-url)))

(cl-defmethod indium-backend-evaluate ((_backend (eql v8)) string &optional callback)
  "Evaluate STRING then call CALLBACK.
CALLBACK is called with two arguments, the value returned by the
evaluation and non-nil if the evaluation threw an error."
  (let* ((current-frame (indium-current-connection-current-frame))
	 (callFrameId (and current-frame (indium-frame-id current-frame))))
    (indium-v8--send-request
     `((method . ,(if callFrameId
                      "Debugger.evaluateOnCallFrame"
                    "Runtime.evaluate"))
       (params . ((expression . ,string)
                  (callFrameId . ,callFrameId)
                  (generatePreview . t))))
     (when callback
       (lambda (response)
         (indium-v8--handle-evaluation-response response callback))))))

(cl-defmethod indium-backend-get-completions ((_backend (eql v8)) expression prefix callback)
  "Get the completion candidates for EXPRESSION that match PREFIX.
Evaluate CALLBACK on the filtered candidates."
  (let ((expression (indium-v8--completion-expression expression)))
    (indium-v8--send-request
     `((method . "Runtime.evaluate")
       (params . ((expression . ,expression)
                  (objectGroup . "completion"))))
     (lambda (response)
       (indium-v8--handle-completions-response response prefix callback)))))

(cl-defmethod indium-backend-register-breakpoint ((_backend (eql v8)) breakpoint &optional callback)
  "Request the addition of BREAKPOINT."
  (let* ((location (indium-breakpoint-location breakpoint))
	 (file (indium-location-file location))
	 (url (indium-workspace-make-url file)))
    (unless url
      (user-error "No URL associated with the current buffer.  Setup an Indium workspace first"))
    (indium-v8--send-request
     `((method . "Debugger.setBreakpointByUrl")
       (params . ((url . ,url)
                  (lineNumber . ,(indium-location-line location))
		  (columnNumber . ,(indium-location-column location))
                  (condition . ,(indium-breakpoint-condition breakpoint)))))
     (lambda (response)
       (let* ((result (map-elt response 'result))
              (id (map-elt result 'breakpointId))
              (locations (map-elt result 'locations))
	      (location (seq--elt-safe locations 0))
              (line (map-elt location 'lineNumber)))
	 (setf (indium-breakpoint-id breakpoint) id)
	 (if line
	     (let ((script (indium-script-find-by-id
			    (map-elt location 'scriptId)))
		   (location (indium-v8--convert-from-v8-location location)))
	       (indium-breakpoint-resolve id script location)
	       (when callback
		 (funcall callback breakpoint)))
	   (message "Cannot get breakpoint location")))))))

(cl-defmethod indium-backend-unregister-breakpoint ((_backend (eql v8)) id &optional callback)
  "Request the removal of the breakpoint with id ID.
Evaluate CALLBACK on success"
  (indium-v8--send-request
   `((method . "Debugger.removeBreakpoint")
     (params . ((breakpointId . ,id))))
   (lambda (_response)
     (when callback (funcall callback)))))

(cl-defmethod indium-backend-deactivate-breakpoints ((_backend (eql v8)))
  "Deactivate all breakpoints.
The runtime will not pause on any breakpoint."
  (indium-v8--send-request
   `((method . "Debugger.setBreakpointsActive")
     (params . ((active . :json-false))))))

(cl-defmethod indium-backend-activate-breakpoints ((_backend (eql v8)))
  "Deactivate all breakpoints.
The runtime will not pause on any breakpoint."
  (indium-v8--send-request
   `((method . "Debugger.setBreakpointsActive")
     (params . ((active . t))))))

(cl-defmethod indium-backend-get-properties ((_backend (eql v8)) reference &optional callback all-properties)
  "Get the properties of the remote object represented by REFERENCE.
CALLBACK is evaluated with the list of properties.

If ALL-PROPERTIES is non-nil, get all the properties from the
prototype chain of the remote object."
  (indium-v8--send-request
   `((method . "Runtime.getProperties")
     (params . ((objectId . ,reference)
                (generatePreview . t)
                (ownProperties . ,(or all-properties :json-false)))))
   (lambda (response)
     (funcall callback
              (indium-v8--properties
               (map-nested-elt response '(result result))
	       (map-nested-elt response '(result internalProperties)))))))

(cl-defmethod indium-backend-set-script-source ((_backend (eql v8)) url source &optional callback)
  (when-let ((script (indium-script-find-from-url url)))
    (indium-v8--send-request
     `((method . "Debugger.setScriptSource")
       (params . ((scriptId . ,(indium-script-id script))
		  (scriptSource . ,source))))
     (lambda (_)
       (indium-v8--send-request
	`((method . "Runtime.compileScript")
	  (params . ((expression . ,source)
		     (sourceURL . ,url)
		     (persistScript . :json-false))))
        (lambda (_)
          (when callback
            (funcall callback))))))))

(cl-defmethod indium-backend-get-script-source ((_backend (eql v8)) frame callback)
  (let ((script (indium-frame-script frame)))
   (indium-v8--send-request
    `((method . "Debugger.getScriptSource")
      (params . ((scriptId . ,(indium-script-id script)))))
    callback)))

(cl-defmethod indium-backend-resume ((_backend (eql v8)) &optional callback)
  "Resume the debugger and evaluate CALLBACK if non-nil."
  (indium-v8--send-request
   `((method . "Debugger.resume"))
   callback))

(cl-defmethod indium-backend-step-into ((_backend (eql v8)) &optional callback)
  "Step into the current stack frame and evaluate CALLBACK if non-nil."
  (indium-v8--send-request
   `((method . "Debugger.stepInto"))
   callback))

(cl-defmethod indium-backend-step-out ((_backend (eql v8)) &optional callback)
  "Step out the current stack frame and evaluate CALLBACK if non-nil."
  (indium-v8--send-request
   `((method . "Debugger.stepOut"))
   callback))

(cl-defmethod indium-backend-step-over ((_backend (eql v8)) &optional callback)
  "Step over the current stack frame and evaluate CALLBACK if non-nil."
  (indium-v8--send-request
   `((method . "Debugger.stepOver"))
   callback))

(cl-defmethod indium-backend-continue-to-location ((_backend (eql v8)) location &optional callback)
  "Continue to LOCATION and evaluate CALLBACK if non-nil.

Location should be an alist with a `limeNumber' and `scriptId' key."
  (indium-v8--send-request
   `((method . "Debugger.continueToLocation")
     (params . ((location . ,(indium-v8--convert-to-v8-location location)))))
   callback))

(defun indium-v8-set-overlay-message (string)
  "Set the debugger page overlay to STRING."
  (indium-v8--send-request
   `((method . "Overlay.setPausedInDebuggerMessage")
     (params . ((message . ,string))))))

(defun indium-v8-remove-overlay-message ()
  "Remove any overlay message displayed on the page."
  (indium-v8--send-request
   `((method . "Overlay.setPausedInDebuggerMessage"))))

(defun indium-v8-set-pause-on-exceptions (state)
  "Defines on which STATE to pause.

Can be set to stop on all exceptions, uncaught exceptions or no
exceptions.  Initial pause on exceptions state is set by Indium to
`\"uncaught\"'.

Allowed states: `\"none\"', `\"uncaught\"', `\"all\"'."
  (interactive (list (completing-read "Pause on exceptions: "
                                      '("none" "uncaught" "all")
                                      nil
                                      t)))
  (indium-v8--send-request `((method . "Debugger.setPauseOnExceptions")
                                 (params . ((state . ,state))))))

(defun indium-v8--set-cache-disabled (disabled)
  "Toggle ignoring cache for each request.  If DISABLED, cache will not be used."
  (indium-v8--send-request
   '((method . "Network.enable"))
   (lambda (_)
     (indium-v8--send-request
      `((method . "Network.setCacheDisabled")
	(params . ((cacheDisabled . ,(if disabled t :json-false)))))
      (lambda (_)
	(indium-v8--send-request '((method . "Network.disable"))))))))

(defun indium-v8-enable-cache ()
  "Enabled network cache for each request."
  (interactive)
  (setq indium-v8-cache-disabled nil)
  (indium-v8--set-cache-disabled nil))

(defun indium-v8-disable-cache ()
  "Disable network cache for each request."
  (interactive)
  (setq indium-v8-cache-disabled t)
  (indium-v8--set-cache-disabled t))

(defun indium-v8--open-ws-connection (url websocket-url &optional on-open nodejs workspace)
  "Open a websocket connection to URL using WEBSOCKET-URL.

Evaluate ON-OPEN when the websocket is open, before setting up
the connection and buffers.

In a Chrom{e|ium} session, URL corresponds to the url of a tab,
and WEBSOCKET-URL to its associated `webSocketDebuggerUrl'.

If NODEJS is non-nil, add a `nodejs' flag to the
`indium-current-connection' to handle special cases.

If WORKSPACE is non-nil, make it the workspace directory for that
connection."
  (unless websocket-url
    (user-error "Cannot open connection, another devtools instance might be open"))
  (websocket-open websocket-url
                  :on-open (lambda (ws)
			     (indium-v8--handle-ws-open ws url nodejs workspace)
			     (when on-open
                               (funcall on-open)))
                  :on-message #'indium-v8--handle-ws-message
                  :on-close #'indium-v8--handle-ws-closed
                  :on-error #'indium-v8--handle-ws-error))

(defun indium-v8--make-connection (ws url &optional nodejs)
  "Return a new connection for WS and URL.
If NODEJS is non-nil, add a `nodejs' extra property to the
connection."
  (let ((conn (make-indium-connection
	       :backend 'v8
	       :url url)))
    (setf (indium-connection-ws conn) ws)
    (when nodejs
      (map-put (indium-connection-props conn) 'nodejs t))
    conn))

(defun indium-v8--handle-ws-open (ws url nodejs workspace)
  "Setup indium for a new connection for the websocket WS.
URL points to the browser tab.

If NODEJS is non-nil, set an extra property in the connection.
If WORKSPACE is non-nil, make it the workspace used for the connection."
  (setq indium-current-connection (indium-v8--make-connection ws url nodejs))
  (indium-v8--enable-tools)
  (switch-to-buffer (indium-repl-get-buffer-create))
  (when workspace (cd workspace))
  (run-hooks 'indium-connection-open-hook))

(defun indium-v8--handle-ws-message (_ws frame)
  "Handle a websocket message FRAME."
  (let* ((message (indium-v8--read-ws-message frame))
         (error (map-elt message 'error))
         (method (map-elt message 'method))
         (request-id (map-elt message 'id))
         (callback (map-elt (indium-current-connection-callbacks)
			    request-id)))
    (cond
     (error (message (map-elt error 'message)))
     (request-id (when callback
                   (funcall callback message)))
     (t (pcase method
          ("Inspector.detached" (indium-v8--handle-inspector-detached message))
          ("Log.entryAdded" (indium-v8--handle-log-entry message))
          ("Runtime.consoleAPICalled" (indium-v8--handle-console-message message))
          ("Runtime.exceptionThrown" (indium-v8--handle-exception-thrown message))
	  ("Debugger.breakpointResolved" (indium-v8--handle-breakpoint-resolved message))
          ("Debugger.paused" (indium-v8--handle-debugger-paused message))
          ("Debugger.scriptParsed" (indium-v8--handle-script-parsed message))
          ("Debugger.resumed" (indium-v8--handle-debugger-resumed message)))))))

(defun indium-v8--handle-breakpoint-resolved (message)
  "Handle a breakpoint resolution.
MESSAGE contains the breakpoint id and location."
  (let ((id (map-nested-elt message '(params breakpointId)))
	(script (indium-script-find-by-id
		 (map-nested-elt message '(params location scriptId))))
	(location (indium-v8--convert-from-v8-location
		   (map-nested-elt message '(params location)))))
    (indium-breakpoint-resolve id script location)))

(defun indium-v8--handle-inspector-detached (message)
  "Handle a closed connection event.
MESSAGE explains why the connection has been closed."
  (let ((msg (map-nested-elt message '(params reason))))
    (indium-backend-close-connection 'v8)
    (message "Indium connection closed: %s" msg)))

(defun indium-v8--handle-log-entry (message)
  "Handle a log entry event with MESSAGE."
  (let ((entry (map-nested-elt message '(params entry))))
    ;; unify console message and entry logs
    (map-put entry 'line (map-elt entry 'lineNumber))
    (indium-repl-emit-console-message entry)))

(defun indium-v8--handle-console-message (message)
  "Handle a console message event with MESSAGE."
  (let* ((msg (map-elt message 'params))
         (args (map-elt msg 'args)))
    (setf (map-elt msg 'values) (seq-map #'indium-v8--value args))
    (indium-repl-emit-console-message msg)))

(defun indium-v8--handle-exception-thrown (message)
  "Handle an exception event MESSAGE."
  (let ((exception (map-nested-elt message '(params exceptionDetails))))
    (indium-repl-emit-console-message (indium-v8--exception exception) t)))

(defun indium-v8--handle-debugger-paused (message)
  "Handle a debugger paused event with MESSAGE."
  (let* ((frames (map-nested-elt message '(params callFrames)))
         (exception (equal (map-nested-elt message '(params reason)) "exception"))
         (reason (if exception "Exception occured" "Breakpoint hit"))
         (description (map-nested-elt message '(params data description))))
    (unless (indium-connection-nodejs-p indium-current-connection)
      (indium-v8-set-overlay-message "Paused in Emacs debugger"))
    (indium-debugger-paused (indium-v8--frames frames) reason description)))

(defun indium-v8--handle-debugger-resumed (_message)
  "Handle a runtime execution resumed event."
  (unless (indium-connection-nodejs-p indium-current-connection)
    (indium-v8-remove-overlay-message))
  (indium-debugger-resumed))

(defun indium-v8--handle-script-parsed (message)
  "Handle a script parsed event with MESSAGE."
  (let* ((id (map-nested-elt message '(params scriptId)))
         (url (map-nested-elt message '(params url)))
         (sourcemap-url (map-nested-elt message '(params sourceMapURL))))
    (let ((script (indium-script-add-script-parsed id url sourcemap-url)))
      (run-hook-with-args 'indium-script-parsed-hook script))))

(defun indium-v8--handle-ws-closed (_ws)
  "Cleanup function called when the connection socket is closed."
  (run-hooks 'indium-connection-closed-hook))

(defun indium-v8--handle-ws-error (_ws _action error)
  "Display an error message for an exception in a websocket callback handling.
ERROR should be a description of the exception."
  (message "Exception in websocket callback! %s" error))

(defun indium-v8--send-request (request &optional callback)
  "Send REQUEST to the current connection.
Evaluate CALLBACK with the response.

If the current connection is closed, display a message."
  (if (indium-v8--connected-p)
      (let ((id (indium-v8--next-request-id)))
	(when callback
		(map-put (indium-current-connection-callbacks)
			 id
			 callback))
	(websocket-send-text (indium-connection-ws indium-current-connection)
			     (json-encode (cons `(id . ,id) request))))
    (message "Socket connection closed")))

(defun indium-v8--read-ws-message (frame)
  "Parse the payload from the websocket FRAME."
  (json-read-from-string (websocket-frame-payload frame)))

(defun indium-v8--enable-tools ()
  "Enable developer tools for the current tab.

There is currently no support for the DOM inspector and network
inspectors."
  (indium-v8--enable-runtime)
  (unless (indium-connection-nodejs-p indium-current-connection)
    (indium-v8--enable-dom)
    (indium-v8--enable-overlay)
    (indium-v8--enable-log)
    (indium-v8--set-cache-disabled indium-v8-cache-disabled))
  (indium-v8--enable-debugger))

(defun indium-v8--enable-log ()
  "Enable the log on the current tab."
  ;; experimental API
  (indium-v8--send-request '((method . "Log.enable"))))

(defun indium-v8--enable-overlay ()
  "Enable the page API on the current tab."
  ;; experimental API
  (indium-v8--send-request '((method . "Overlay.enable"))))

(defun indium-v8--enable-dom ()
  "Enable the DOM API on the current tab."
  (indium-v8--send-request '((method . "DOM.enable"))))

(defun indium-v8--enable-runtime ()
  "Enable the runtime on the current tab."
  (indium-v8--send-request '((method . "Runtime.enable")))
  (indium-v8--send-request '((method . "Runtime.runIfWaitingForDebugger"))))

(defun indium-v8--enable-debugger ()
  "Enable the debugger on the current tab."
  (indium-v8--send-request
   '((method . "Debugger.enable"))
   (lambda (&rest _)
     (indium-v8-set-pause-on-exceptions "uncaught")
     (indium-v8--set-blackbox-patterns indium-debugger-blackbox-regexps))))

(defun indium-v8--set-blackbox-patterns (regexps)
  "Replace previous blackbox patterns with passed REGEXPS."
  (indium-v8--send-request
   `((method . "Debugger.setBlackboxPatterns")
     (params . ((patterns . ,regexps))))))

(defun indium-v8--handle-evaluation-response (response callback)
  "Get the result of an evaluation in RESPONSE and evaluate CALLBACK with it."
  (let* ((result (map-nested-elt response '(result result)))
         (error (eq (map-nested-elt response '(result wasThrown)) t)))
    (funcall callback (indium-v8--value result) error)))

(defun indium-v8--handle-completions-response (response prefix callback)
  "Request a completion list for the object in RESPONSE.
The completion list is filtered using the PREFIX string, then
CALLBACK is evaluated with it."
  (let ((objectid (map-nested-elt response '(result result objectId)))
        (type (map-nested-elt response '(result result type))))
    (if objectid
        (indium-v8--get-completion-list-by-reference objectid prefix callback)
      (indium-v8--get-completion-list-by-type type prefix callback))))

(defun indium-v8--get-completion-list-by-reference (objectid prefix callback)
  "Request the completion list for a remote object referenced by OBJECTID.
The completion list is filtered using the PREFIX string, then
CALLBACK is evaluated with it."
  (indium-v8--send-request
   `((method . "Runtime.callFunctionOn")
     (params . ((objectId . ,objectid)
                (functionDeclaration . ,indium-v8-completion-function)
                (returnByValue . t))))
   (lambda (response)
     (indium-v8--handle-completion-list-response response prefix callback))))

(defun indium-v8--get-completion-list-by-type (type prefix callback)
  "Request the completion list for an object of type TYPE.
The completion list is filtered using the PREFIX string, then
CALLBACK is evaluated with it.

This method is used for strings, numbers and booleans.  See
`indium-v8--get-completion-list-by-reference' for getting
completions using references to remote objects (including
arrays)."
  (let ((expression (format "(%s)(\"%s\")" indium-v8-completion-function type)))
    (indium-v8--send-request
     `((method . "Runtime.evaluate")
       (params . ((expression . ,expression)
                  (returnByValue . t))))
     (lambda (response)
       (indium-v8--handle-completion-list-response response prefix callback)))))

(defun indium-v8--completion-expression (string)
  "Return the completion expression to be requested from STRING."
  (if (string-match-p "\\." string)
      (replace-regexp-in-string "\\.[^\\.]*$" "" string)
    "this"))

(defun indium-v8--handle-completion-list-response (response prefix callback)
  "Filter candidates from RESPONSE matching PREFIX.
Evaluate CALLBACK on the result."
  (let ((candidates (map-nested-elt response '(result result value))))
    (funcall callback (seq-filter (lambda (candidate)
                                    (string-prefix-p prefix candidate))
                                  (seq-map (lambda (candidate)
                                             (symbol-name (car candidate)))
                                           candidates)))))

(cl-defmethod indium-v8--connected-p ()
  "Return non-nil if the current connection is open."
  (indium-backend-active-connection-p 'v8))

(defun indium-v8--value (result)
  "Return an alist representing the value of RESULT.

The returned value can be a reference to a remote object, in
which case the value associated to the `objectid' key is
non-nil."
  (let* ((value (map-elt result 'value))
         (type (intern (map-elt result 'type)))
         (objectid (map-elt result 'objectId))
         (preview (indium-v8--preview result))
         (description (indium-v8--description result)))
    `((objectid . ,objectid)
      (description . ,description)
      (type . ,type)
      (preview . ,preview)
      (value . ,value))))

(defun indium-v8--exception (result)
  "Return an exception built from RESULT."
  (setf (map-elt result 'values)
        (list (indium-v8--value
               (map-elt result 'exception)))))

(defun indium-v8--description (result)
  "Return a description string built from RESULT.
RESULT should be a reference to a remote object."
  (let ((value (map-elt result 'value))
        (type (intern (map-elt result 'type))))
    (or (map-elt result 'description)
        (pcase type
          (`undefined "undefined")
          (`function "function")
          (`number (number-to-string value))
          (`string (format "\"%s\"" value))
          (`boolean (if (eq value t)
                        "true"
                      "false"))
          (_ (or value "null"))))))

(defun indium-v8--preview (result)
  "Return a preview string built from RESULT.
RESULT should be a reference to a remote object."
  (let* ((preview (map-elt result 'preview))
         (subtype (map-elt preview 'subtype)))
    (if (string= subtype "array")
        (indium-v8--preview-array preview)
      (indium-v8--preview-object preview))))

(defun indium-v8--preview-object (preview)
  "Return a preview string from the properties of the object PREVIEW."
  (concat "{ "
          (mapconcat (lambda (prop)
                       (format "%s: %s"
                               (map-elt prop 'name)
                               (indium-v8--preview-property prop)))
                     (map-elt preview 'properties)
                     ", ")
          (if (eq (map-elt preview 'lossless) :json-false)
              ", … }"
            " }")))

(defun indium-v8--preview-array (preview)
  "Return a preview string from the elements of the array PREVIEW."
  (concat "[ "
          (mapconcat (lambda (prop)
                       (indium-v8--preview-property prop))
                     (map-elt preview 'properties)
                     ", ")
          (if (eq (map-elt preview 'lossless) :json-false)
              "… ]"
            " ]")))

(defun indium-v8--preview-property (property)
  "Return the string for a single object or array PROPERTY preview."
  (if (equal (map-elt property 'type) "string")
      (format "\"%s\"" (map-elt property 'value))
    (let ((preview (map-elt property 'value)))
      (if (seq-empty-p preview)
          (map-elt property 'type)
        preview))))

(defun indium-v8--properties (properties &optional internal-properties)
  "Return a list of object properties built from PROPERTIES.
If INTERNAL-PROPERTIES is non-nil, also add them."
  (let ((properties (seq-map (lambda (prop)
			       `((name . ,(map-elt prop 'name))
				 (value . ,(indium-v8--value (or (map-elt prop 'value)
								 (map-elt prop 'get))))))
			     properties)))
    (if internal-properties
	(seq-concatenate 'list properties (indium-v8--properties internal-properties))
      properties)))

(defun indium-v8--scope-chain (frame)
  "Return a scope chain for a FRAME."
  (let ((scope-chain (seq-map (lambda (scope)
                                `((object . ,(indium-v8--value (map-elt scope 'object)))
                                  (name . ,(map-elt scope 'name))
                                  (type . ,(map-elt scope 'type))))
                              (map-elt frame 'scopeChain)))
        (this `((object . ,(indium-v8--value (map-elt frame 'this)))
                (name . "this")
                (type . "local"))))
    (cl-loop for scope in scope-chain
             collect scope
             when (string= (map-elt scope 'type) "local")
             collect this)))

(defun indium-v8--convert-to-v8-location (location)
  "Return an alist representing a V8 location from LOCATION."
  (let ((result '()))
    (when-let ((line (indium-location-line location)))
      (map-put result 'lineNumber line))
    (when-let ((column (indium-location-column location)))
      (map-put result 'columnNumber column))
    (when-let ((file (indium-location-file location))
	       (script (indium-script-find-from-file file)))
      (map-put result 'scriptId (indium-script-id script)))
    result))

(defun indium-v8--convert-from-v8-location (location)
  "Return a location struct built from a v8 LOCATION."
  (make-indium-location-from-script-id :line (map-elt location 'lineNumber)
				       :column (map-elt location 'columnNumber)
				       :script-id (map-elt location 'scriptId)))

(defun indium-v8--frames (list)
  "Return a list of frames built from LIST."
  (seq-filter #'identity
	      (seq-map
	       (lambda (frame)
		 ;; For some reason, sometimes V8 will send frames with
		 ;; a `scriptId' that was never parsed, so ignore these
		 ;; stack frames
		 (when-let ((script (indium-script-find-by-id
				     (map-nested-elt frame '(location scriptId)))))
		   (make-indium-frame
		    :scope-chain (indium-v8--scope-chain frame)
		    :location (indium-v8--convert-from-v8-location
			       (map-elt frame 'location))
		    :type (map-elt frame 'type)
		    :script script
		    :function-name (map-elt frame 'functionName)
		    :id (map-elt frame 'callFrameId))))
	       list)))

(defvar indium-v8--request-id 0)
(defun indium-v8--next-request-id ()
  "Return the next unique identifier to be used in a request."
  (cl-incf indium-v8--request-id))

(provide 'indium-v8)
;;; indium-v8.el ends here
