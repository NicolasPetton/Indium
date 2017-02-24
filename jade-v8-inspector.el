;;; jade-v8-inspector.el --- V8-Inspector backend for jade  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Nicolas Petton

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

;; Jade backend implementation for V8-Inspector to be used with nodejs
;; processes.  Connection is handled in jade-nodejs.el.  This backend currently
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

(require 'jade-backend)
(require 'jade-repl)
(require 'jade-debugger)


(defvar jade-v8-inspector-completion-function "function getCompletions(type)\n{var object;if(type==='string')\nobject=new String('');else if(type==='number')\nobject=new Number(0);else if(type==='boolean')\nobject=new Boolean(false);else\nobject=this;var resultSet={};for(var o=object;o;o=o.__proto__){try{if(type==='array'&&o===object&&ArrayBuffer.isView(o)&&o.length>9999)\ncontinue;var names=Object.getOwnPropertyNames(o);for(var i=0;i<names.length;++i)\nresultSet[names[i]]=true;}catch(e){}}\nreturn resultSet;}")

(jade-register-backend 'v8-inspector)

(cl-defmethod jade-backend-active-connection-p ((backend (eql v8-inspector)) connection)
  "Return non-nil if CONNECTION is active."
  (websocket-openp (map-elt connection 'ws)))

(cl-defmethod jade-backend-close-connection ((backend (eql v8-inspector)) connection)
  "Close the websocket associated with CONNECTION."
  (websocket-close (map-elt connection 'ws)))

(cl-defmethod jade-backend-reconnect ((backend (eql v8-inspector)))
  (let* ((connection jade-connection)
         (url (map-elt connection 'url))
         (websocket-url (websocket-url (map-elt connection 'ws))))
    (jade-v8-inspector--open-ws-connection url
                                     websocket-url
                                     ;; close all buffers related to the closed
                                     ;; connection the first
                                     #'jade-quit)))

(cl-defmethod jade-backend-evaluate ((backend (eql v8-inspector)) string &optional callback)
  "Evaluate STRING then call CALLBACK.
CALLBACK is called with two arguments, the value returned by the
evaluation and non-nil if the evaluation threw an error."
  (jade-v8-inspector--send-request
   `((method . "Runtime.evaluate")
     (params . ((expression . ,string)
                (generatePreview . t))))
   (lambda (response)
     (when callback
      (jade-v8-inspector--handle-evaluation-response response callback)))))

(cl-defmethod jade-backend-evaluate-on-frame ((backend (eql v8-inspector)) string frame &optional callback)
  "Evaluate STRING on the call frame FRAME then call CALLBACK.
CALLBACK is called with two arguments, the value returned by the
evaluation and non-nil if the evaluation threw an error."
  (jade-v8-inspector--send-request
   `((method . "Debugger.evaluateOnCallFrame")
     (params . ((expression . ,string)
                (callFrameId . ,(map-elt frame 'callFrameId))
                (generatePreview . t))))
   (lambda (response)
     (jade-v8-inspector--handle-evaluation-response response callback))))

(cl-defmethod jade-backend-get-completions ((backend (eql v8-inspector)) expression prefix callback)
  "Get the completion candidates for EXPRESSION that match PREFIX.
Evaluate CALLBACK on the filtered candidates."
  (let ((expression (jade-v8-inspector--completion-expression expression)))
    (jade-v8-inspector--send-request
     `((method . "Runtime.evaluate")
       (params . ((expression . ,expression)
                  (objectGroup . "completion"))))
     (lambda (response)
       (jade-v8-inspector--handle-completions-response response prefix callback)))))

(cl-defmethod jade-backend-get-properties ((backend (eql v8-inspector)) reference &optional callback all-properties)
  "Get the properties of the remote object represented by REFERENCE.
CALLBACK is evaluated with the list of properties.

If ALL-PROPERTIES is non-nil, get all the properties from the
prototype chain of the remote object."
  (jade-v8-inspector--send-request
   `((method . "Runtime.getProperties")
     (params . ((objectId . ,reference)
                (ownProperties . ,(not all-properties)))))
   (lambda (response)
     (funcall callback
              (jade-v8-inspector--properties
               (map-nested-elt response '(result result)))))))

(cl-defmethod jade-backend-get-script-source ((backend (eql v8-inspector)) frame callback)
  (let ((script-id (map-nested-elt frame '(location scriptId))))
   (jade-v8-inspector--send-request
    `((method . "Debugger.getScriptSource")
      (params . ((scriptId . ,script-id))))
    callback)))

(cl-defmethod jade-backend-get-script-url ((backend (eql v8-inspector)) frame)
  (let ((script-id (map-nested-elt frame '(location scriptId))))
    (jade-v8-inspector--get-script-url script-id)))

(cl-defmethod jade-backend-resume ((backend (eql v8-inspector)) &optional callback)
  "Resume the debugger and evaluate CALLBACK if non-nil."
  (jade-v8-inspector--send-request
   `((method . "Debugger.resume"))
   callback))

(cl-defmethod jade-backend-step-into ((backend (eql v8-inspector)) &optional callback)
  "Step into the current stack frame and evaluate CALLBACK if non-nil."
  (jade-v8-inspector--send-request
   `((method . "Debugger.stepInto"))
   callback))

(cl-defmethod jade-backend-step-out ((backend (eql v8-inspector)) &optional callback)
  "Step out the current stack frame and evaluate CALLBACK if non-nil."
  (jade-v8-inspector--send-request
   `((method . "Debugger.stepOut"))
   callback))

(cl-defmethod jade-backend-step-over ((backend (eql v8-inspector)) &optional callback)
  "Step over the current stack frame and evaluate CALLBACK if non-nil."
  (jade-v8-inspector--send-request
   `((method . "Debugger.stepOver"))
   callback))

(cl-defmethod jade-backend-continue-to-location ((backend (eql v8-inspector)) location &optional callback)
  "Continue to LOCATION and evaluate CALLBACK if non-nil.

Location should be an alist with a `limeNumber' and `scriptId' key."
  (jade-v8-inspector--send-request
   `((method . "Debugger.continueToLocation")
     (params . ((location . ,location))))
   callback))

(defun jade-v8-inspector-set-pause-on-exceptions (state)
  "Defines on which STATE to pause.

Can be set to stop on all exceptions, uncaught exceptions or no
exceptions. Initial pause on exceptions state is set by Jade to
`\"uncaught\"'.

Allowed states: `\"none\"', `\"uncaught\"', `\"all\"'."
  (interactive (list (completing-read "Pause on exceptions: "
                                      '("none" "uncaught" "all")
                                      nil
                                      t)))
  (jade-v8-inspector--send-request `((method . "Debugger.setPauseOnExceptions")
                               (params . ((state . ,state))))))

(defun jade-v8-inspector--open-ws-connection (url websocket-url &optional on-open)
  "Open a websocket connection to URL using WEBSOCKET-URL.

Evaluate ON-OPEN when the websocket is open, before setting up
the connection and buffers.

In a Chrom{e|ium} session, URL corresponds to the url of a tab,
and WEBSOCKET-URL to its associated `webSocketDebuggerUrl'.

In a NodeJS session, URL and WEBSOCKET-URL should point to the
same url."
  (unless websocket-url
    (user-error "Cannot open connection, another devtools instance might be open"))
  (websocket-open websocket-url
                  :on-open (lambda (ws)
                             (when on-open
                               (funcall on-open))
                             (jade-v8-inspector--handle-ws-open ws url))
                  :on-message #'jade-v8-inspector--handle-ws-message
                  :on-close #'jade-v8-inspector--handle-ws-closed
                  :on-error #'jade-v8-inspector--handle-ws-error))

(defun jade-v8-inspector--make-connection (ws url)
  "Return a new connection for WS and URL."
  (let ((connection (make-hash-table)))
    (map-put connection 'ws ws)
    (map-put connection 'url url)
    (map-put connection 'backend 'v8-inspector)
    (map-put connection 'callbacks (make-hash-table))
    (add-to-list 'jade-connections connection)
    connection))

(defun jade-v8-inspector--callbacks ()
  "Return the callbacks associated with the current connection."
  (map-elt jade-connection 'callbacks))

(defun jade-v8-inspector--connection-for-ws (ws)
  "Return the v8-inspector connection associated with the websocket WS."
  (seq-find (lambda (connection)
              (eq (map-elt connection 'ws) ws))
            jade-connections))

(defun jade-v8-inspector--handle-ws-open (ws url)
  (let* ((connection (jade-v8-inspector--make-connection ws url)))
    (jade-with-connection connection
      (jade-v8-inspector--enable-tools))
    (switch-to-buffer (jade-repl-get-buffer-create connection))))

(defun jade-v8-inspector--handle-ws-message (ws frame)
  (jade-with-connection (jade-v8-inspector--connection-for-ws ws)
    (let* ((message (jade-v8-inspector--read-ws-message frame))
           (error (map-elt message 'error))
           (method (map-elt message 'method))
           (request-id (map-elt message 'id))
           (callback (map-elt (jade-v8-inspector--callbacks) request-id)))
      (cond
       (error (message (map-elt error 'message)))
       (request-id (when callback
                     (funcall callback message)))
       (t (pcase method
            ("Inspector.detached" (jade-v8-inspector--handle-inspector-detached message))
            ("Console.messageAdded" (jade-v8-inspector--handle-console-message message))
            ("Debugger.paused" (jade-v8-inspector--handle-debugger-paused message))
            ("Debugger.scriptParsed" (jade-v8-inspector--handle-script-parsed message))
            ("Debugger.resumed" (jade-v8-inspector--handle-debugger-resumed message))))))))

(defun jade-v8-inspector--handle-inspector-detached (message)
  "Handle connection closed because it was detached."
  (let ((msg (map-nested-elt message '(params reason))))
    (jade-backend-close-connection 'v8-inspector jade-connection)
    (message "Jade connection closed: %s" msg)))

(defun jade-v8-inspector--handle-console-message (message)
  (let* ((msg (map-nested-elt message '(params message)))
         (parameters (map-elt msg 'parameters)))
    (setf (map-elt msg 'parameters) (seq-map #'jade-v8-inspector--value parameters))
    (jade-repl-emit-console-message msg)))

(defun jade-v8-inspector--handle-debugger-paused (message)
  (let ((frames (map-nested-elt message '(params callFrames))))
    (jade-debugger-paused (jade-v8-inspector--frames frames))))

(defun jade-v8-inspector--handle-debugger-resumed (_message)
  (jade-debugger-resumed))

(defun jade-v8-inspector--handle-script-parsed (message)
  (let* ((scriptId (map-nested-elt message '(params scriptId)))
         (url (map-nested-elt message '(params url))))
    (jade-v8-inspector--add-script-parsed scriptId url)))

(defun jade-v8-inspector--handle-ws-closed (_ws)
  (jade-repl--handle-connection-closed))

(defun jade-v8-inspector--handle-ws-error (ws action error)
  (message "WS Error! %s %s" action error))

(defun jade-v8-inspector--send-request (request &optional callback)
  "Send REQUEST to the current connection.
Evaluate CALLBACK with the response.

If the current connection is closed, display an error message in
the REPL buffer."
  (if (jade-v8-inspector--connected-p)
      (let ((id (jade-v8-inspector--next-request-id))
            (callbacks (jade-v8-inspector--callbacks)))
        (when callback
          (map-put callbacks id callback))
        (websocket-send-text (map-elt jade-connection 'ws)
                             (json-encode (cons `(id . ,id) request))))
    (jade-repl-emit-console-message '((level . "error") (text . "Socket connection closed")))))

(defun jade-v8-inspector--read-ws-message (frame)
  (json-read-from-string (websocket-frame-payload frame)))

(defun jade-v8-inspector--enable-tools ()
  "Enable developer tools for the current tab.

There is currently no support for the DOM inspector and network
inspectors."
  (jade-v8-inspector--enable-console)
  (jade-v8-inspector--enable-runtime)
  (jade-v8-inspector--enable-debugger)
  )

(defun jade-v8-inspector--enable-console ()
  "Enable the console on the current tab."
  (jade-v8-inspector--send-request '((method . "Console.enable"))))

(defun jade-v8-inspector--enable-runtime ()
  "Enable the runtime on the current tab."
  (jade-v8-inspector--send-request '((method . "Runtime.enable")))
  (jade-v8-inspector--send-request '((method . "Runtime.run")))
  (jade-v8-inspector--send-request '((method . "Runtime.runIfWaitingForDebugger"))))

(defun jade-v8-inspector--enable-debugger ()
  "Enable the debugger on the current tab."
  (jade-v8-inspector--send-request '((method . "Debugger.enable"))
                             (lambda (&rest _)
                               (jade-v8-inspector-set-pause-on-exceptions "uncaught"))))

(defun jade-v8-inspector--handle-evaluation-response (response callback)
  "Get the result of an evaluation in RESPONSE and evaluate CALLBACK with it."
  (let* ((result (map-nested-elt response '(result result)))
         (error (eq (map-nested-elt response '(result wasThrown)) t)))
    (funcall callback (jade-v8-inspector--value result) error)))

(defun jade-v8-inspector--handle-completions-response (response prefix callback)
  "Request a completion list for the object in RESPONSE.
The completion list is filtered using the PREFIX string, then
CALLBACK is evaluated with it."
  (let ((objectid (map-nested-elt response '(result result objectId)))
        (type (map-nested-elt response '(result result type))))
    (if objectid
        (jade-v8-inspector--get-completion-list-by-reference objectid prefix callback)
      (jade-v8-inspector--get-completion-list-by-type type prefix callback))))

(defun jade-v8-inspector--get-completion-list-by-reference (objectid prefix callback)
  "Request the completion list for a remote object referenced by OBJECTID.
The completion list is filtered using the PREFIX string, then
CALLBACK is evaluated with it."
  (jade-v8-inspector--send-request
   `((method . "Runtime.callFunctionOn")
     (params . ((objectId . ,objectid)
                (functionDeclaration . ,jade-v8-inspector-completion-function)
                (returnByValue . t))))
   (lambda (response)
     (jade-v8-inspector--handle-completion-list-response response prefix callback))))

(defun jade-v8-inspector--get-completion-list-by-type (type prefix callback)
  "Request the completion list for an object of type TYPE.
The completion list is filtered using the PREFIX string, then
CALLBACK is evaluated with it.

This method is used for strings, numbers and booleans.  See
`jade-v8-inspector--get-completion-list-by-reference' for getting
completions using references to remote objects (including
arrays)."
  (let ((expression (format "(%s)(\"%s\")" jade-v8-inspector-completion-function type)))
    (jade-v8-inspector--send-request
     `((method . "Runtime.evaluate")
       (params . ((expression . ,expression)
                  (returnByValue . t))))
     (lambda (response)
       (jade-v8-inspector--handle-completion-list-response response prefix callback)))))

(defun jade-v8-inspector--completion-expression (string)
  "Return the completion expression to be requested from STRING."
  (if (string-match-p "\\." string)
      (replace-regexp-in-string "\\.[^\\.]*$" "" string)
    "this"))

(defun jade-v8-inspector--handle-completion-list-response (response prefix callback)
  "Evauate CALLBACK on the completion candidates from RESPONSE.
Candidates are filtered using the PREFIX string."
  (let ((candidates (map-nested-elt response '(result result value))))
    (funcall callback (seq-filter (lambda (candidate)
                                    (string-prefix-p prefix candidate))
                                  (seq-map (lambda (candidate)
                                             (symbol-name (car candidate)))
                                           candidates)))))

(cl-defmethod jade-v8-inspector--connected-p ()
  "Return non-nil if the current connection is open."
  (jade-backend-active-connection-p 'v8-inspector jade-connection))

(defun jade-v8-inspector--value (result)
  "Return an alist representing the value of RESULT.

The returned value can be a reference to a remote object, in
which case the value associated to the `objectid' key is
non-nil."
  (let* ((value (map-elt result 'value))
         (type (intern (map-elt result 'type)))
         (objectid (map-elt result 'objectId))
         (preview (jade-v8-inspector--preview result))
         (description (jade-v8-inspector--description result)))
    `((objectid . ,objectid)
      (description . ,description)
      (type . ,type)
      (preview . ,preview)
      (value . ,value))))

(defun jade-v8-inspector--description (result)
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

(defun jade-v8-inspector--preview (result)
  "Return a preview string built from RESULT.
RESULT should be a reference to a remote object."
  (let* ((preview (map-elt result 'preview))
         (subtype (map-elt preview 'subtype)))
    (if (string= subtype "array")
        (jade-v8-inspector--preview-array preview)
      (jade-v8-inspector--preview-object preview))))

(defun jade-v8-inspector--preview-object (preview)
  "Return a preview string from the properties of the object PREVIEW."
  (concat " { "
          (mapconcat (lambda (prop)
                       (format "%s: %s"
                               (map-elt prop 'name)
                               (jade-v8-inspector--description prop)))
                     (map-elt preview 'properties)
                     ", ")
          (if (eq (map-elt preview 'lossless) :json-false)
              ", … }"
            " }")))

(defun jade-v8-inspector--preview-array (preview)
  "Return a preview string from the elements of the array PREVIEW."
  (concat " [ "
          (mapconcat (lambda (prop)
                       (format "%s" (jade-v8-inspector--description prop)))
                     (map-elt preview 'properties)
                     ", ")
          (if (eq (map-elt preview 'lossless) :json-false)
              "… ]"
            " ]")))

(defun jade-v8-inspector--properties (result)
  "Return a list of object properties built from RESULT."
  (seq-map (lambda (prop)
             `((name . ,(map-elt prop 'name))
               (value . ,(jade-v8-inspector--value (or (map-elt prop 'value)
                                                 (map-elt prop 'get))))))
           result))

(defun jade-v8-inspector--frames (list)
  "Return a list of frames built from LIST."
  (seq-map (lambda (frame)
             `((scope-chain . ,(seq-map (lambda (scope)
                                          `((object . ,(jade-v8-inspector--value (map-elt scope 'object)))
                                            (name . ,(map-elt scope 'name))
                                            (type . ,(map-elt scope 'type))))
                                  (map-elt frame 'scopeChain)))
               (location . ,(map-elt frame 'location))
               (type . ,(map-elt frame 'type))
               (functionName . ,(map-elt frame 'functionName))
               (callFrameId . ,(map-elt frame 'callFrameId))))
           list))

(defun jade-v8-inspector--add-script-parsed (scriptId url)
  (unless (map-elt jade-connection 'scripts)
    (map-put jade-connection 'scripts '()))
  (map-put (map-elt jade-connection 'scripts)
           (intern scriptId)
           url))

(defun jade-v8-inspector--get-script-url (scriptId)
  (map-nested-elt jade-connection (list 'scripts (intern scriptId))))

(let ((id 0))
  (defun jade-v8-inspector--next-request-id ()
    "Return the next unique identifier to be used in a request."
    (cl-incf id)))

(provide 'jade-v8-inspector)
;;; jade-v8-inspector.el ends here
