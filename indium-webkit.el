;;; indium-webkit.el --- Webkit/Blink backend for indium  -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2017  Nicolas Petton

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

;; Indium backend implementation for Webkit and Blink.  Connection is handled in
;; indium-chrome.el.  This backend currently supports the REPL, code completion,
;; object inspection and the debugger.
;;
;; The protocol supports both Chrome/Chromium and Nodejs.
;; The protocol is documented at
;; https://chromedevtools.github.io/debugger-protocol-viewer/1-2/.

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

(defvar indium-webkit-cache-disabled nil
  "Network cache disabled state.  If non-nil disable cache when Indium starts.")

(defvar indium-webkit-completion-function "function getCompletions(type)\n{var object;if(type==='string')\nobject=new String('');else if(type==='number')\nobject=new Number(0);else if(type==='boolean')\nobject=new Boolean(false);else\nobject=this;var resultSet={};for(var o=object;o;o=o.__proto__){try{if(type==='array'&&o===object&&ArrayBuffer.isView(o)&&o.length>9999)\ncontinue;var names=Object.getOwnPropertyNames(o);for(var i=0;i<names.length;++i)\nresultSet[names[i]]=true;}catch(e){}}\nreturn resultSet;}")

(indium-register-backend 'webkit)

(defun indium-connection-ws (connection)
  "Return the websocket associated to CONNECTION."
  (map-elt (indium-connection-props connection) 'ws))

(cl-defmethod (setf indium-connection-ws) (ws (connection indium-connection))
  (map-put (indium-connection-props connection) 'ws ws))

(defun indium-connection-nodejs-p (connection)
  "Return non-nil if CONNECTION is for Nodejs."
  (and connection
       (map-elt (indium-connection-props connection) 'nodejs)))

(cl-defmethod indium-backend-active-connection-p ((_backend (eql webkit)))
  "Return non-nil if the current connection is active."
  (when-indium-connected
    (websocket-openp (indium-connection-ws indium-current-connection))))

(cl-defmethod indium-backend-close-connection ((_backend (eql webkit)))
  "Close the websocket associated with the current connection."
  (websocket-close (indium-connection-ws indium-current-connection)))

(cl-defmethod indium-backend-reconnect ((_backend (eql webkit)))
  (indium-webkit--open-ws-connection
   (indium-current-connection-url)
   (websocket-url (indium-connection-ws indium-current-connection))
   ;; close all buffers related to the closed
   ;; connection the first
   #'indium-quit))

(cl-defmethod indium-backend-evaluate ((_backend (eql webkit)) string &optional callback)
  "Evaluate STRING then call CALLBACK.
CALLBACK is called with two arguments, the value returned by the
evaluation and non-nil if the evaluation threw an error."
  (let* ((current-frame (indium-current-connection-current-frame))
	 (callFrameId (and current-frame (indium-frame-id current-frame))))
    (indium-webkit--send-request
     `((method . ,(if callFrameId
                      "Debugger.evaluateOnCallFrame"
                    "Runtime.evaluate"))
       (params . ((expression . ,string)
                  (callFrameId . ,callFrameId)
                  (generatePreview . t))))
     (when callback
       (lambda (response)
         (indium-webkit--handle-evaluation-response response callback))))))

(cl-defmethod indium-backend-get-completions ((_backend (eql webkit)) expression prefix callback)
  "Get the completion candidates for EXPRESSION that match PREFIX.
Evaluate CALLBACK on the filtered candidates."
  (let ((expression (indium-webkit--completion-expression expression)))
    (indium-webkit--send-request
     `((method . "Runtime.evaluate")
       (params . ((expression . ,expression)
                  (objectGroup . "completion"))))
     (lambda (response)
       (indium-webkit--handle-completions-response response prefix callback)))))

(cl-defmethod indium-backend-add-breakpoint ((_backend (eql webkit)) location &optional callback condition)
  "Request the addition of a breakpoint.

The breakpoint is set at LOCATION.  When CALLBACK is
non-nil, evaluate it with the breakpoint's location and id."
  (let ((url (indium-workspace-make-url (indium-location-file location)))
        (condition (or condition "")))
    (unless url
      (user-error "No URL associated with the current buffer.  Setup an Indium workspace first"))
    (indium-webkit--send-request
     `((method . "Debugger.setBreakpointByUrl")
       (params . ((url . ,url)
                  (lineNumber . ,(indium-location-line location))
		  (columnNumber . ,(indium-location-column location))
                  (condition . ,condition))))
     (lambda (response)
       (let* ((breakpoint (map-elt response 'result))
              (id (map-elt breakpoint 'breakpointId))
              (locations (map-elt breakpoint 'locations))
              (line (map-elt (seq--elt-safe locations 0) 'lineNumber)))
         (when line
           (indium-backend-register-breakpoint id line (indium-location-file location) condition))
         (when callback
           (unless line
             (message "Cannot get breakpoint location"))
           (funcall callback id)))))))

(cl-defmethod indium-backend-remove-breakpoint ((_backend (eql webkit)) id)
  "Request the removal of the breakpoint with id ID."
  (indium-webkit--send-request
   `((method . "Debugger.removeBreakpoint")
     (params . ((breakpointId . ,id))))
   (lambda (_response)
     (indium-backend-unregister-breakpoint id))))

(cl-defmethod indium-backend-deactivate-breakpoints ((_backend (eql webkit)))
  "Deactivate all breakpoints.
The runtime will not pause on any breakpoint."
  (indium-webkit--send-request
   `((method . "Debugger.setBreakpointsActive")
     (params . ((active . :json-false))))))

(cl-defmethod indium-backend-activate-breakpoints ((_backend (eql webkit)))
  "Deactivate all breakpoints.
The runtime will not pause on any breakpoint."
  (indium-webkit--send-request
   `((method . "Debugger.setBreakpointsActive")
     (params . ((active . t))))))

(cl-defmethod indium-backend-get-properties ((_backend (eql webkit)) reference &optional callback all-properties)
  "Get the properties of the remote object represented by REFERENCE.
CALLBACK is evaluated with the list of properties.

If ALL-PROPERTIES is non-nil, get all the properties from the
prototype chain of the remote object."
  (indium-webkit--send-request
   `((method . "Runtime.getProperties")
     (params . ((objectId . ,reference)
                (generatePreview . t)
                (ownProperties . ,(or all-properties :json-false)))))
   (lambda (response)
     (funcall callback
              (indium-webkit--properties
               (map-nested-elt response '(result result)))))))

(cl-defmethod indium-backend-set-script-source ((_backend (eql webkit)) url source &optional callback)
  (when-let ((script (indium-script-find-from-url url)))
    (indium-webkit--send-request
     `((method . "Runtime.compileScript")
       (params . ((expression . ,source)
                  (sourceURL . ,url)
                  (persistScript . :json-false))))
     (lambda (_)
       (indium-webkit--send-request
        `((method . "Debugger.setScriptSource")
          (params . ((scriptId . ,(indium-script-id script))
                     (scriptSource . ,source))))
        (lambda (_)
          (when callback
            (funcall callback))))))))

(cl-defmethod indium-backend-get-script-source ((_backend (eql webkit)) frame callback)
  (let ((script (indium-frame-script frame)))
   (indium-webkit--send-request
    `((method . "Debugger.getScriptSource")
      (params . ((scriptId . ,(indium-script-id script)))))
    callback)))

(cl-defmethod indium-backend-resume ((_backend (eql webkit)) &optional callback)
  "Resume the debugger and evaluate CALLBACK if non-nil."
  (indium-webkit--send-request
   `((method . "Debugger.resume"))
   callback))

(cl-defmethod indium-backend-step-into ((_backend (eql webkit)) &optional callback)
  "Step into the current stack frame and evaluate CALLBACK if non-nil."
  (indium-webkit--send-request
   `((method . "Debugger.stepInto"))
   callback))

(cl-defmethod indium-backend-step-out ((_backend (eql webkit)) &optional callback)
  "Step out the current stack frame and evaluate CALLBACK if non-nil."
  (indium-webkit--send-request
   `((method . "Debugger.stepOut"))
   callback))

(cl-defmethod indium-backend-step-over ((_backend (eql webkit)) &optional callback)
  "Step over the current stack frame and evaluate CALLBACK if non-nil."
  (indium-webkit--send-request
   `((method . "Debugger.stepOver"))
   callback))

(cl-defmethod indium-backend-continue-to-location ((_backend (eql webkit)) location &optional callback)
  "Continue to LOCATION and evaluate CALLBACK if non-nil.

Location should be an alist with a `limeNumber' and `scriptId' key."
  (indium-webkit--send-request
   `((method . "Debugger.continueToLocation")
     (params . ((location . ,(indium-webkit--convert-to-webkit-location location)))))
   callback))

(defun indium-webkit-set-overlay-message (string)
  "Set the debugger page overlay to STRING."
  (indium-webkit--send-request
   `((method . "Page.configureOverlay")
     (params . ((suspended . :json-false)
                (message . ,string))))
   #'ignore))

(defun indium-webkit-remove-overlay-message ()
  "Remove any overlay message displayed on the page."
  (indium-webkit--send-request
   `((method . "Page.configureOverlay")
     (params . ((suspended . :json-false))))))

(defun indium-webkit-set-pause-on-exceptions (state)
  "Defines on which STATE to pause.

Can be set to stop on all exceptions, uncaught exceptions or no
exceptions.  Initial pause on exceptions state is set by Indium to
`\"uncaught\"'.

Allowed states: `\"none\"', `\"uncaught\"', `\"all\"'."
  (interactive (list (completing-read "Pause on exceptions: "
                                      '("none" "uncaught" "all")
                                      nil
                                      t)))
  (indium-webkit--send-request `((method . "Debugger.setPauseOnExceptions")
                                 (params . ((state . ,state))))))

(defun indium-webkit--set-cache-disabled (disabled)
  "Toggle ignoring cache for each request.  If DISABLED, cache will not be used."
  (indium-webkit--send-request `((method . "Network.setCacheDisabled")
                                 (params . ((cacheDisabled . ,(if disabled t :json-false)))))))

(defun indium-webkit-enable-cache ()
  "Enabled network cache for each request."
  (interactive)
  (setq indium-webkit-cache-disabled nil)
  (indium-webkit--set-cache-disabled nil))

(defun indium-webkit-disable-cache ()
  "Disable network cache for each request."
  (interactive)
  (setq indium-webkit-cache-disabled t)
  (indium-webkit--set-cache-disabled t))

(defun indium-webkit--open-ws-connection (url websocket-url &optional on-open nodejs workspace)
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
                             (when on-open
                               (funcall on-open))
                             (indium-webkit--handle-ws-open ws url nodejs workspace))
                  :on-message #'indium-webkit--handle-ws-message
                  :on-close #'indium-webkit--handle-ws-closed
                  :on-error #'indium-webkit--handle-ws-error))

(defun indium-webkit--make-connection (ws url &optional nodejs)
  "Return a new connection for WS and URL.
If NODEJS is non-nil, add a `nodejs' extra property to the
connection."
  (let ((conn (make-indium-connection
	       :backend 'webkit
	       :url url)))
    (setf (indium-connection-ws conn) ws)
    (when nodejs
      (map-put (indium-connection-props conn) 'nodejs t))
    conn))

(defun indium-webkit--handle-ws-open (ws url nodejs workspace)
  "Setup indium for a new connection for the websocket WS.
URL points to the browser tab.

If NODEJS is non-nil, set an extra property in the connection.
If WORKSPACE is non-nil, make it the workspace used for the connection."
  (setq indium-current-connection (indium-webkit--make-connection ws url nodejs))
  (indium-webkit--enable-tools)
  (switch-to-buffer (indium-repl-buffer-create))
  (when workspace (cd workspace))
  (indium-breakpoint-restore-breakpoints)
  (run-hooks 'indium-connection-open-hook))

(defun indium-webkit--handle-ws-message (_ws frame)
  "Handle a websocket message FRAME."
  (let* ((message (indium-webkit--read-ws-message frame))
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
          ("Inspector.detached" (indium-webkit--handle-inspector-detached message))
          ("Log.entryAdded" (indium-webkit--handle-log-entry message))
          ("Runtime.consoleAPICalled" (indium-webkit--handle-console-message message))
          ("Runtime.exceptionThrown" (indium-webkit--handle-exception-thrown message))
          ("Debugger.paused" (indium-webkit--handle-debugger-paused message))
          ("Debugger.scriptParsed" (indium-webkit--handle-script-parsed message))
          ("Debugger.resumed" (indium-webkit--handle-debugger-resumed message)))))))

(defun indium-webkit--handle-inspector-detached (message)
  "Handle a closed connection event.
MESSAGE explains why the connection has been closed."
  (let ((msg (map-nested-elt message '(params reason))))
    (indium-backend-close-connection 'webkit)
    (message "Indium connection closed: %s" msg)))

(defun indium-webkit--handle-log-entry (message)
  "Handle a log entry event with MESSAGE."
  (let ((entry (map-nested-elt message '(params entry))))
    ;; unify console message and entry logs
    (map-put entry 'line (map-elt entry 'lineNumber))
    (indium-repl-emit-console-message entry)))

(defun indium-webkit--handle-console-message (message)
  "Handle a console message event with MESSAGE."
  (let* ((msg (map-elt message 'params))
         (args (map-elt msg 'args)))
    (setf (map-elt msg 'values) (seq-map #'indium-webkit--value args))
    (indium-repl-emit-console-message msg)))

(defun indium-webkit--handle-exception-thrown (message)
  "Handle an exception event MESSAGE."
  (let ((exception (map-nested-elt message '(params exceptionDetails))))
    (indium-repl-emit-console-message (indium-webkit--exception exception) t)))

(defun indium-webkit--handle-debugger-paused (message)
  "Handle a debugger paused event with MESSAGE."
  (let* ((frames (map-nested-elt message '(params callFrames)))
         (exception (equal (map-nested-elt message '(params reason)) "exception"))
         (reason (if exception "Exception occured" "Breakpoint hit"))
         (description (map-nested-elt message '(params data description))))
    (unless (indium-connection-nodejs-p indium-current-connection)
      (indium-webkit-set-overlay-message "Paused in Emacs debugger"))
    (indium-debugger-paused (indium-webkit--frames frames) reason description)))

(defun indium-webkit--handle-debugger-resumed (_message)
  "Handle a runtime execution resumed event."
  (unless (indium-connection-nodejs-p indium-current-connection)
    (indium-webkit-remove-overlay-message))
  (indium-debugger-resumed))

(defun indium-webkit--handle-script-parsed (message)
  "Handle a script parsed event with MESSAGE."
  (let* ((id (map-nested-elt message '(params scriptId)))
         (url (map-nested-elt message '(params url)))
         (sourcemap-url (map-nested-elt message '(params sourceMapURL))))
    (indium-script-add-script-parsed id url sourcemap-url)))

(defun indium-webkit--handle-ws-closed (_ws)
  "Cleanup function called when the connection socket is closed."
  (run-hooks 'indium-connection-closed-hook)
  (indium-repl--handle-connection-closed))

(defun indium-webkit--handle-ws-error (_ws _action error)
  "Display an error message for an exception in a websocket callback handling.
ERROR should be a description of the exception."
  (message "Exception in websocket callback! %s" error))

(defun indium-webkit--send-request (request &optional callback)
  "Send REQUEST to the current connection.
Evaluate CALLBACK with the response.

If the current connection is closed, display a message."
  (if (indium-webkit--connected-p)
      (let ((id (indium-webkit--next-request-id)))
	(when callback
		(map-put (indium-current-connection-callbacks)
			 id
			 callback))
	(websocket-send-text (indium-connection-ws indium-current-connection)
			     (json-encode (cons `(id . ,id) request))))
    (message "Socket connection closed")))

(defun indium-webkit--read-ws-message (frame)
  "Parse the payload from the websocket FRAME."
  (json-read-from-string (websocket-frame-payload frame)))

(defun indium-webkit--enable-tools ()
  "Enable developer tools for the current tab.

There is currently no support for the DOM inspector and network
inspectors."
  (indium-webkit--enable-runtime)
  (unless (indium-connection-nodejs-p indium-current-connection)
    (indium-webkit--enable-page)
    (indium-webkit--enable-network)
    (indium-webkit--enable-log))
  (indium-webkit--enable-debugger))

(defun indium-webkit--enable-log ()
  "Enable the log on the current tab."
  (indium-webkit--send-request '((method . "Log.enable"))))

(defun indium-webkit--enable-page ()
  "Enable the page API on the current tab."
  (indium-webkit--send-request '((method . "Page.enable"))))

(defun indium-webkit--enable-runtime ()
  "Enable the runtime on the current tab."
  (indium-webkit--send-request '((method . "Runtime.enable")))
  (indium-webkit--send-request '((method . "Runtime.runIfWaitingForDebugger"))))

(defun indium-webkit--enable-network ()
  "Enable the runtime on the current tab."
  (indium-webkit--send-request '((method . "Network.enable"))
                               (lambda (_)
                                 (when indium-webkit-cache-disabled
                                   (indium-webkit--set-cache-disabled t)))))

(defun indium-webkit--enable-debugger ()
  "Enable the debugger on the current tab."
  (indium-webkit--send-request '((method . "Debugger.enable"))
                             (lambda (&rest _)
                               (indium-webkit-set-pause-on-exceptions "uncaught"))))

(defun indium-webkit--handle-evaluation-response (response callback)
  "Get the result of an evaluation in RESPONSE and evaluate CALLBACK with it."
  (let* ((result (map-nested-elt response '(result result)))
         (error (eq (map-nested-elt response '(result wasThrown)) t)))
    (funcall callback (indium-webkit--value result) error)))

(defun indium-webkit--handle-completions-response (response prefix callback)
  "Request a completion list for the object in RESPONSE.
The completion list is filtered using the PREFIX string, then
CALLBACK is evaluated with it."
  (let ((objectid (map-nested-elt response '(result result objectId)))
        (type (map-nested-elt response '(result result type))))
    (if objectid
        (indium-webkit--get-completion-list-by-reference objectid prefix callback)
      (indium-webkit--get-completion-list-by-type type prefix callback))))

(defun indium-webkit--get-completion-list-by-reference (objectid prefix callback)
  "Request the completion list for a remote object referenced by OBJECTID.
The completion list is filtered using the PREFIX string, then
CALLBACK is evaluated with it."
  (indium-webkit--send-request
   `((method . "Runtime.callFunctionOn")
     (params . ((objectId . ,objectid)
                (functionDeclaration . ,indium-webkit-completion-function)
                (returnByValue . t))))
   (lambda (response)
     (indium-webkit--handle-completion-list-response response prefix callback))))

(defun indium-webkit--get-completion-list-by-type (type prefix callback)
  "Request the completion list for an object of type TYPE.
The completion list is filtered using the PREFIX string, then
CALLBACK is evaluated with it.

This method is used for strings, numbers and booleans.  See
`indium-webkit--get-completion-list-by-reference' for getting
completions using references to remote objects (including
arrays)."
  (let ((expression (format "(%s)(\"%s\")" indium-webkit-completion-function type)))
    (indium-webkit--send-request
     `((method . "Runtime.evaluate")
       (params . ((expression . ,expression)
                  (returnByValue . t))))
     (lambda (response)
       (indium-webkit--handle-completion-list-response response prefix callback)))))

(defun indium-webkit--completion-expression (string)
  "Return the completion expression to be requested from STRING."
  (if (string-match-p "\\." string)
      (replace-regexp-in-string "\\.[^\\.]*$" "" string)
    "this"))

(defun indium-webkit--handle-completion-list-response (response prefix callback)
  "Filter candidates from RESPONSE matching PREFIX.
Evaluate CALLBACK on the result."
  (let ((candidates (map-nested-elt response '(result result value))))
    (funcall callback (seq-filter (lambda (candidate)
                                    (string-prefix-p prefix candidate))
                                  (seq-map (lambda (candidate)
                                             (symbol-name (car candidate)))
                                           candidates)))))

(cl-defmethod indium-webkit--connected-p ()
  "Return non-nil if the current connection is open."
  (indium-backend-active-connection-p 'webkit))

(defun indium-webkit--value (result)
  "Return an alist representing the value of RESULT.

The returned value can be a reference to a remote object, in
which case the value associated to the `objectid' key is
non-nil."
  (let* ((value (map-elt result 'value))
         (type (intern (map-elt result 'type)))
         (objectid (map-elt result 'objectId))
         (preview (indium-webkit--preview result))
         (description (indium-webkit--description result)))
    `((objectid . ,objectid)
      (description . ,description)
      (type . ,type)
      (preview . ,preview)
      (value . ,value))))

(defun indium-webkit--exception (result)
  "Return an exception built from RESULT."
  (setf (map-elt result 'values)
        (list (indium-webkit--value
               (map-elt result 'exception)))))

(defun indium-webkit--description (result)
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

(defun indium-webkit--preview (result)
  "Return a preview string built from RESULT.
RESULT should be a reference to a remote object."
  (let* ((preview (map-elt result 'preview))
         (subtype (map-elt preview 'subtype)))
    (if (string= subtype "array")
        (indium-webkit--preview-array preview)
      (indium-webkit--preview-object preview))))

(defun indium-webkit--preview-object (preview)
  "Return a preview string from the properties of the object PREVIEW."
  (concat "{ "
          (mapconcat (lambda (prop)
                       (format "%s: %s"
                               (map-elt prop 'name)
                               (indium-webkit--preview-property prop)))
                     (map-elt preview 'properties)
                     ", ")
          (if (eq (map-elt preview 'lossless) :json-false)
              ", … }"
            " }")))

(defun indium-webkit--preview-array (preview)
  "Return a preview string from the elements of the array PREVIEW."
  (concat "[ "
          (mapconcat (lambda (prop)
                       (indium-webkit--preview-property prop))
                     (map-elt preview 'properties)
                     ", ")
          (if (eq (map-elt preview 'lossless) :json-false)
              "… ]"
            " ]")))

(defun indium-webkit--preview-property (property)
  "Return the string for a single object or array PROPERTY preview."
  (if (equal (map-elt property 'type) "string")
      (format "\"%s\"" (map-elt property 'value))
    (let ((preview (map-elt property 'value)))
      (if (seq-empty-p preview)
          (map-elt property 'type)
        preview))))

(defun indium-webkit--properties (result)
  "Return a list of object properties built from RESULT."
  (seq-map (lambda (prop)
             `((name . ,(map-elt prop 'name))
               (value . ,(indium-webkit--value (or (map-elt prop 'value)
                                                 (map-elt prop 'get))))))
           result))

(defun indium-webkit--scope-chain (frame)
  "Return a scope chain for a FRAME."
  (let ((scope-chain (seq-map (lambda (scope)
                                `((object . ,(indium-webkit--value (map-elt scope 'object)))
                                  (name . ,(map-elt scope 'name))
                                  (type . ,(map-elt scope 'type))))
                              (map-elt frame 'scopeChain)))
        (this `((object . ,(indium-webkit--value (map-elt frame 'this)))
                (name . "this")
                (type . "local"))))
    (cl-loop for scope in scope-chain
             collect scope
             when (string= (map-elt scope 'type) "local")
             collect this)))

(defun indium-webkit--convert-to-webkit-location (location)
  "Return an alist representing a Webkit location from LOCATION."
  (let ((result '()))
    (when-let ((line (indium-location-line location)))
      (map-put result 'lineNumber line))
    (when-let ((column (indium-location-column location)))
      (map-put result 'columnNumber column))
    (when-let ((file (indium-location-file location))
	       (script (indium-script-find-from-file file)))
      (map-put result 'scriptId (indium-script-id script)))
    result))

(defun indium-webkit--convert-from-webkit-location (location)
  "Return a location struct built from a webkit LOCATION."
  (make-indium-location-from-script-id :line (map-elt location 'lineNumber)
				       :column (map-elt location 'columnNumber)
				       :script-id (map-elt location 'scriptId)))

(defun indium-webkit--frames (list)
  "Return a list of frames built from LIST."
  (seq-map (lambda (frame)
	     (make-indium-frame
	      :scope-chain (indium-webkit--scope-chain frame)
	      :location (indium-webkit--convert-from-webkit-location (map-elt frame 'location))
	      :type (map-elt frame 'type)
	      :script (indium-script-find-by-id (map-nested-elt frame '(location scriptId)))
	      :function-name (map-elt frame 'functionName)
	      :id (map-elt frame 'callFrameId)))
           list))

(defvar indium-webkit--request-id 0)
(defun indium-webkit--next-request-id ()
  "Return the next unique identifier to be used in a request."
  (cl-incf indium-webkit--request-id))

(provide 'indium-webkit)
;;; indium-webkit.el ends here
