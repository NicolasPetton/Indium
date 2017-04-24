;;; indium-v8-inspector.el --- V8-Inspector backend for indium  -*- lexical-binding: t; -*-

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

;; Indium backend implementation for V8-Inspector to be used with nodejs
;; processes.  Connection is handled in indium-nodejs.el.  This backend currently
;; supports the REPL, code completion, object inspection and the debugger.
;;
;; The protocol is documented at
;; https://chromedevtools.github.io/debugger-protocol-viewer/v8/

;;; Code:

(require 'websocket)
(require 'json)
(require 'map)
(require 'seq)
(require 'cl-lib)

(require 'indium-backend)
(require 'indium-repl)
(require 'indium-debugger)


(defvar indium-v8-inspector-completion-function "function getCompletions(type)\n{var object;if(type==='string')\nobject=new String('');else if(type==='number')\nobject=new Number(0);else if(type==='boolean')\nobject=new Boolean(false);else\nobject=this;var resultSet={};for(var o=object;o;o=o.__proto__){try{if(type==='array'&&o===object&&ArrayBuffer.isView(o)&&o.length>9999)\ncontinue;var names=Object.getOwnPropertyNames(o);for(var i=0;i<names.length;++i)\nresultSet[names[i]]=true;}catch(e){}}\nreturn resultSet;}")

(indium-register-backend 'v8-inspector)

(cl-defmethod indium-backend-active-connection-p ((backend (eql v8-inspector)))
  "Return non-nil if the current connection is active."
  (and indium-connection
       (websocket-openp (map-elt indium-connection 'ws))))

(cl-defmethod indium-backend-close-connection ((backend (eql v8-inspector)))
  "Close the websocket associated with the current connection."
  (websocket-close (map-elt indium-connection 'ws)))

(cl-defmethod indium-backend-reconnect ((backend (eql v8-inspector)))
  (let* ((url (map-elt indium-connection 'url))
         (websocket-url (websocket-url (map-elt indium-connection 'ws))))
    (indium-v8-inspector--open-ws-connection url
                                     websocket-url
                                     ;; close all buffers related to the closed
                                     ;; connection the first
                                     #'indium-quit)))

(cl-defmethod indium-backend-evaluate ((backend (eql v8-inspector)) string &optional callback)
  "Evaluate STRING then call CALLBACK.
CALLBACK is called with two arguments, the value returned by the
evaluation and non-nil if the evaluation threw an error."
  (let ((callFrameId (map-elt (map-elt indium-connection 'current-frame)
                              'callFrameId)))
    (indium-v8-inspector--send-request
     `((method . ,(if callFrameId
                      "Debugger.evaluateOnCallFrame"
                    "Runtime.evaluate"))
       (params . ((expression . ,string)
                  (callFrameId . ,callFrameId)
                  (generatePreview . t))))
     (lambda (response)
       (when callback
         (indium-webkit--handle-evaluation-response response callback))))))

(cl-defmethod indium-backend-get-completions ((backend (eql v8-inspector)) expression prefix callback)
  "Get the completion candidates for EXPRESSION that match PREFIX.
Evaluate CALLBACK on the filtered candidates."
  (let ((expression (indium-v8-inspector--completion-expression expression)))
    (indium-v8-inspector--send-request
     `((method . "Runtime.evaluate")
       (params . ((expression . ,expression)
                  (objectGroup . "completion"))))
     (lambda (response)
       (indium-v8-inspector--handle-completions-response response prefix callback)))))

(cl-defmethod indium-backend-add-breakpoint ((backend (eql v8-inspector)) file line &optional callback condition)
    "Request the addition of a breakpoint.

The breakpoint is set at URL on line LINE.  When CALLBACK is
non-nil, evaluate it with the breakpoint's location and id."
  (indium-v8-inspector--send-request
   `((method . "Debugger.setBreakpointByUrl")
     (params . ((url . ,file)
                (lineNumber . ,line)
                (condition . ,(or condition "")))))
   (lambda (response)
     (let* ((breakpoint (map-elt response 'result))
            (id (map-elt breakpoint 'breakpointId))
            (locations (map-elt breakpoint 'locations))
            (line (map-elt (seq--elt-safe locations 0) 'lineNumber)))
       (when line
         (indium-v8-inspector--register-breakpoint id line buffer-file-name))
       (when callback
         (unless line
           (message "Cannot get breakpoint location"))
         (funcall callback line id condition))))))

(cl-defmethod indium-backend-remove-breakpoint ((backend (eql v8-inspector)) id)
  "Request the removal of the breakpoint with id ID."
  (indium-v8-inspector--send-request
   `((method . "Debugger.removeBreakpoint")
     (params . ((breakpointId . ,id))))
   (lambda (response)
     (indium-v8-inspector--unregister-breakpoint id))))

(cl-defmethod indium-backend-get-breakpoints ((backend (eql v8-inspector)))
  "Return all breakpoints.
A breakpoint is a map with the keys `id', `file', and `line'."
  (let ((breakpoints (map-elt indium-connection 'breakpoints)))
    (map-keys-apply (lambda (key)
                      `((id . ,key)
                        (file . ,(map-nested-elt breakpoints `(,key file)))
                        (line . ,(map-nested-elt breakpoints `(,key line)))))
                    breakpoints)))

(defun indium-v8-inspector--register-breakpoint (id line file)
  "Register the breakpoint with ID at LINE in FILE.
If a buffer visits FILE with `indium-interaction-mode' turned on,
the breakpoint can be added back to the buffer."
  (let ((breakpoint `((line . ,line)
                      (file . ,file))))
    (map-put (map-elt indium-connection 'breakpoints) id breakpoint)))

(defun indium-v8-inspector--unregister-breakpoint (id)
  "Remove the breakpoint with ID from the current connection."
  (map-delete (map-elt indium-connection 'breakpoints) id))

(cl-defmethod indium-backend-get-properties ((backend (eql v8-inspector)) reference &optional callback all-properties)
  "Get the properties of the remote object represented by REFERENCE.
CALLBACK is evaluated with the list of properties.

If ALL-PROPERTIES is non-nil, get all the properties from the
prototype chain of the remote object."
  (indium-v8-inspector--send-request
   `((method . "Runtime.getProperties")
     (params . ((objectId . ,reference)
                (generatePreview . t)
                (ownProperties . ,(or all-properties :json-false)))))
   (lambda (response)
     (funcall callback
              (indium-v8-inspector--properties
               (map-nested-elt response '(result result)))))))

(cl-defmethod indium-backend-get-properties ((backend (eql v8-inspector)) reference &optional callback all-properties)
  "Get the properties of the remote object represented by REFERENCE.
CALLBACK is evaluated with the list of properties.

If ALL-PROPERTIES is non-nil, get all the properties from the
prototype chain of the remote object."
  (indium-v8-inspector--send-request
   `((method . "Runtime.getProperties")
     (params . ((objectId . ,reference)
                (ownProperties . ,(not all-properties)))))
   (lambda (response)
     (funcall callback
              (indium-v8-inspector--properties
               (map-nested-elt response '(result result)))))))

(cl-defmethod indium-backend-get-script-source ((backend (eql v8-inspector)) frame callback)
  (let ((script-id (map-nested-elt frame '(location scriptId))))
   (indium-v8-inspector--send-request
    `((method . "Debugger.getScriptSource")
      (params . ((scriptId . ,script-id))))
    callback)))

(cl-defmethod indium-backend-get-script-url ((backend (eql v8-inspector)) frame)
  (let ((script-id (map-nested-elt frame '(location scriptId))))
    (indium-v8-inspector--get-script-url script-id)))

(cl-defmethod indium-backend-resume ((backend (eql v8-inspector)) &optional callback)
  "Resume the debugger and evaluate CALLBACK if non-nil."
  (indium-v8-inspector--send-request
   `((method . "Debugger.resume"))
   callback))

(cl-defmethod indium-backend-step-into ((backend (eql v8-inspector)) &optional callback)
  "Step into the current stack frame and evaluate CALLBACK if non-nil."
  (indium-v8-inspector--send-request
   `((method . "Debugger.stepInto"))
   callback))

(cl-defmethod indium-backend-step-out ((backend (eql v8-inspector)) &optional callback)
  "Step out the current stack frame and evaluate CALLBACK if non-nil."
  (indium-v8-inspector--send-request
   `((method . "Debugger.stepOut"))
   callback))

(cl-defmethod indium-backend-step-over ((backend (eql v8-inspector)) &optional callback)
  "Step over the current stack frame and evaluate CALLBACK if non-nil."
  (indium-v8-inspector--send-request
   `((method . "Debugger.stepOver"))
   callback))

(cl-defmethod indium-backend-continue-to-location ((backend (eql v8-inspector)) location &optional callback)
  "Continue to LOCATION and evaluate CALLBACK if non-nil.

Location should be an alist with a `limeNumber' and `scriptId' key."
  (indium-v8-inspector--send-request
   `((method . "Debugger.continueToLocation")
     (params . ((location . ,location))))
   callback))

(defun indium-v8-inspector-set-pause-on-exceptions (state)
  "Defines on which STATE to pause.

Can be set to stop on all exceptions, uncaught exceptions or no
exceptions. Initial pause on exceptions state is set by Indium to
`\"uncaught\"'.

Allowed states: `\"none\"', `\"uncaught\"', `\"all\"'."
  (interactive (list (completing-read "Pause on exceptions: "
                                      '("none" "uncaught" "all")
                                      nil
                                      t)))
  (indium-v8-inspector--send-request `((method . "Debugger.setPauseOnExceptions")
                               (params . ((state . ,state))))))

(defun indium-v8-inspector--open-ws-connection (websocket-url &optional on-open)
  "Open a websocket connection to WEBSOCKET-URL.

Evaluate ON-OPEN when the websocket is open, before setting up
the connection and buffers."
  (unless websocket-url
    (user-error "Cannot open connection, another devtools instance might be open"))
  (websocket-open websocket-url
                  :on-open (lambda (ws)
                             (when on-open
                               (funcall on-open))
                             (indium-v8-inspector--handle-ws-open ws))
                  :on-message #'indium-v8-inspector--handle-ws-message
                  :on-close #'indium-v8-inspector--handle-ws-closed
                  :on-error #'indium-v8-inspector--handle-ws-error))

(defun indium-v8-inspector--make-connection (ws)
  "Return a new connection for WS."
  (let ((connection (make-hash-table)))
    (map-put connection 'ws ws)
    (map-put connection 'url (format "file://%s" default-directory))
    (map-put connection 'backend 'v8-inspector)
    (map-put connection 'callbacks (make-hash-table))
    connection))

(defun indium-v8-inspector--callbacks ()
  "Return the callbacks associated with the current connection."
  (map-elt indium-connection 'callbacks))

(defun indium-v8-inspector--handle-ws-open (ws)
  (setq indium-connection (indium-v8-inspector--make-connection ws))
  (indium-v8-inspector--enable-tools)
  (switch-to-buffer (indium-repl-buffer-create))
  (indium-breakpoint-restore-breakpoints))

(defun indium-v8-inspector--handle-ws-message (ws frame)
  (let* ((message (indium-v8-inspector--read-ws-message frame))
         (error (map-elt message 'error))
         (method (map-elt message 'method))
         (request-id (map-elt message 'id))
         (callback (map-elt (indium-v8-inspector--callbacks) request-id)))
    (cond
     (error (message (map-elt error 'message)))
     (request-id (when callback
                   (funcall callback message)))
     (t (pcase method
          ("Inspector.detached" (indium-v8-inspector--handle-inspector-detached message))
          ("Console.messageAdded" (indium-v8-inspector--handle-console-message message))
          ("Debugger.paused" (indium-v8-inspector--handle-debugger-paused message))
          ("Debugger.scriptParsed" (indium-v8-inspector--handle-script-parsed message))
          ("Debugger.resumed" (indium-v8-inspector--handle-debugger-resumed message)))))))

(defun indium-v8-inspector--handle-inspector-detached (message)
  "Handle connection closed because it was detached."
  (let ((msg (map-nested-elt message '(params reason))))
    (indium-backend-close-connection 'v8-inspector)
    (message "Indium connection closed: %s" msg)))

(defun indium-v8-inspector--handle-console-message (message)
  (let* ((msg (map-nested-elt message '(params message)))
         (parameters (map-elt msg 'parameters)))
    (setf (map-elt msg 'parameters) (seq-map #'indium-v8-inspector--value parameters))
    (indium-repl-emit-console-message msg)))

(defun indium-v8-inspector--handle-debugger-paused (message)
  (let ((frames (map-nested-elt message '(params callFrames)))
        (exception (equal (map-nested-elt message '(params reason)) "exception")))
    (if (and exception (not (y-or-n-p "Uncaught exception!  Open a debugger? ")))
        (indium-backend-resume 'v8-inspector)
      (indium-debugger-paused (indium-webkit--frames frames)))))

(defun indium-v8-inspector--handle-debugger-resumed (_message)
  (indium-debugger-resumed))

(defun indium-v8-inspector--handle-script-parsed (message)
  (let* ((scriptId (map-nested-elt message '(params scriptId)))
         (url (map-nested-elt message '(params url))))
    (indium-v8-inspector--add-script-parsed scriptId url)))

(defun indium-v8-inspector--handle-ws-closed (_ws)
  (indium-repl--handle-connection-closed))

(defun indium-v8-inspector--handle-ws-error (ws action error)
  (message "WS Error! %s %s" action error))

(defun indium-v8-inspector--send-request (request &optional callback)
  "Send REQUEST to the current connection.
Evaluate CALLBACK with the response.

If the current connection is closed, display an error message in
the REPL buffer."
  (if (indium-v8-inspector--connected-p)
      (let ((id (indium-v8-inspector--next-request-id))
            (callbacks (indium-v8-inspector--callbacks)))
        (when callback
          (map-put callbacks id callback))
        (websocket-send-text (map-elt indium-connection 'ws)
                             (json-encode (cons `(id . ,id) request))))
    (indium-repl-emit-console-message '((level . "error") (text . "Socket connection closed")))))

(defun indium-v8-inspector--read-ws-message (frame)
  (json-read-from-string (websocket-frame-payload frame)))

(defun indium-v8-inspector--enable-tools ()
  "Enable developer tools for the current tab.

There is currently no support for the DOM inspector and network
inspectors."
  (indium-v8-inspector--enable-console)
  (indium-v8-inspector--enable-runtime)
  (indium-v8-inspector--enable-debugger)
  )

(defun indium-v8-inspector--enable-console ()
  "Enable the console on the current tab."
  (indium-v8-inspector--send-request '((method . "Console.enable"))))

(defun indium-v8-inspector--enable-runtime ()
  "Enable the runtime on the current tab."
  (indium-v8-inspector--send-request '((method . "Runtime.enable")))
  (indium-v8-inspector--send-request '((method . "Runtime.runIfWaitingForDebugger"))))

(defun indium-v8-inspector--enable-debugger ()
  "Enable the debugger on the current tab."
  (indium-v8-inspector--send-request '((method . "Debugger.enable"))
                             (lambda (&rest _)
                               (indium-v8-inspector-set-pause-on-exceptions "uncaught"))))

(defun indium-v8-inspector--handle-evaluation-response (response callback)
  "Get the result of an evaluation in RESPONSE and evaluate CALLBACK with it."
  (let* ((result (map-nested-elt response '(result result)))
         (error (eq (map-nested-elt response '(result wasThrown)) t)))
    (funcall callback (indium-v8-inspector--value result) error)))

(defun indium-v8-inspector--handle-completions-response (response prefix callback)
  "Request a completion list for the object in RESPONSE.
The completion list is filtered using the PREFIX string, then
CALLBACK is evaluated with it."
  (let ((objectid (map-nested-elt response '(result result objectId)))
        (type (map-nested-elt response '(result result type))))
    (if objectid
        (indium-v8-inspector--get-completion-list-by-reference objectid prefix callback)
      (indium-v8-inspector--get-completion-list-by-type type prefix callback))))

(defun indium-v8-inspector--get-completion-list-by-reference (objectid prefix callback)
  "Request the completion list for a remote object referenced by OBJECTID.
The completion list is filtered using the PREFIX string, then
CALLBACK is evaluated with it."
  (indium-v8-inspector--send-request
   `((method . "Runtime.callFunctionOn")
     (params . ((objectId . ,objectid)
                (functionDeclaration . ,indium-v8-inspector-completion-function)
                (returnByValue . t))))
   (lambda (response)
     (indium-v8-inspector--handle-completion-list-response response prefix callback))))

(defun indium-v8-inspector--get-completion-list-by-type (type prefix callback)
  "Request the completion list for an object of type TYPE.
The completion list is filtered using the PREFIX string, then
CALLBACK is evaluated with it.

This method is used for strings, numbers and booleans.  See
`indium-v8-inspector--get-completion-list-by-reference' for getting
completions using references to remote objects (including
arrays)."
  (let ((expression (format "(%s)(\"%s\")" indium-v8-inspector-completion-function type)))
    (indium-v8-inspector--send-request
     `((method . "Runtime.evaluate")
       (params . ((expression . ,expression)
                  (returnByValue . t))))
     (lambda (response)
       (indium-v8-inspector--handle-completion-list-response response prefix callback)))))

(defun indium-v8-inspector--completion-expression (string)
  "Return the completion expression to be requested from STRING."
  (if (string-match-p "\\." string)
      (replace-regexp-in-string "\\.[^\\.]*$" "" string)
    "this"))

(defun indium-v8-inspector--handle-completion-list-response (response prefix callback)
  "Evauate CALLBACK on the completion candidates from RESPONSE.
Candidates are filtered using the PREFIX string."
  (let ((candidates (map-nested-elt response '(result result value))))
    (funcall callback (seq-filter (lambda (candidate)
                                    (string-prefix-p prefix candidate))
                                  (seq-map (lambda (candidate)
                                             (symbol-name (car candidate)))
                                           candidates)))))

(cl-defmethod indium-v8-inspector--connected-p ()
  "Return non-nil if the current connection is open."
  (indium-backend-active-connection-p 'v8-inspector))

(defun indium-v8-inspector--value (result)
  "Return an alist representing the value of RESULT.

The returned value can be a reference to a remote object, in
which case the value associated to the `objectid' key is
non-nil."
  (let* ((value (map-elt result 'value))
         (type (intern (map-elt result 'type)))
         (objectid (map-elt result 'objectId))
         (preview (indium-v8-inspector--preview result))
         (description (indium-v8-inspector--description result)))
    `((objectid . ,objectid)
      (description . ,description)
      (type . ,type)
      (preview . ,preview)
      (value . ,value))))

(defun indium-v8-inspector--description (result)
  "Return a description string built from RESULT.
RESULT should be a reference to a remote object."
  (let ((value (map-elt result 'value))
        (type (intern (map-elt result 'type))))
    (or (map-elt result 'description)
        (pcase type
          (`undefined "undefined")
          (`function "function")
          (`number (if (numberp value)
                       (number-to-string value)
                     value))
          (`string (format "\"%s\"" value))
          (`boolean (pcase value
                      (`t "true")
                      (_ "false")))
          (_ (or value "null"))))))

(defun indium-v8-inspector--preview (result)
  "Return a preview string built from RESULT.
RESULT should be a reference to a remote object."
  (let* ((preview (map-elt result 'preview))
         (subtype (map-elt preview 'subtype)))
    (if (string= subtype "array")
        (indium-v8-inspector--preview-array preview)
      (indium-v8-inspector--preview-object preview))))

(defun indium-v8-inspector--preview-object (preview)
  "Return a preview string from the properties of the object PREVIEW."
  (concat " { "
          (mapconcat (lambda (prop)
                       (format "%s: %s"
                               (map-elt prop 'name)
                               (indium-v8-inspector--description prop)))
                     (map-elt preview 'properties)
                     ", ")
          (if (eq (map-elt preview 'lossless) :json-false)
              ", … }"
            " }")))

(defun indium-v8-inspector--preview-array (preview)
  "Return a preview string from the elements of the array PREVIEW."
  (concat " [ "
          (mapconcat (lambda (prop)
                       (format "%s" (indium-v8-inspector--description prop)))
                     (map-elt preview 'properties)
                     ", ")
          (if (eq (map-elt preview 'lossless) :json-false)
              "… ]"
            " ]")))

(defun indium-v8-inspector--properties (result)
  "Return a list of object properties built from RESULT."
  (seq-map (lambda (prop)
             `((name . ,(map-elt prop 'name))
               (value . ,(indium-v8-inspector--value (or (map-elt prop 'value)
                                                 (map-elt prop 'get))))))
           result))

(defun indium-v8-inspector--frames (list)
  "Return a list of frames built from LIST."
  (seq-map (lambda (frame)
             `((scope-chain . ,(seq-map (lambda (scope)
                                          `((object . ,(indium-v8-inspector--value (map-elt scope 'object)))
                                            (name . ,(map-elt scope 'name))
                                            (type . ,(map-elt scope 'type))))
                                  (map-elt frame 'scopeChain)))
               (location . ,(map-elt frame 'location))
               (type . ,(map-elt frame 'type))
               (functionName . ,(map-elt frame 'functionName))
               (callFrameId . ,(map-elt frame 'callFrameId))))
           list))

(defun indium-v8-inspector--add-script-parsed (scriptId url)
  (unless (map-elt indium-connection 'scripts)
    (map-put indium-connection 'scripts '()))
  (map-put (map-elt indium-connection 'scripts)
           (intern scriptId)
           url))

(defun indium-v8-inspector--get-script-url (scriptId)
  (map-nested-elt indium-connection (list 'scripts (intern scriptId))))

(let ((id 0))
  (defun indium-v8-inspector--next-request-id ()
    "Return the next unique identifier to be used in a request."
    (cl-incf id)))

(provide 'indium-v8-inspector)
;;; indium-v8-inspector.el ends here
