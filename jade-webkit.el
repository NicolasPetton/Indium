;;; jade-webkit.el --- Webkit/Blink backend for jade  -*- lexical-binding: t; -*-

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

;; Jade backend implementation for Webkit and Blink.  Connection is handled in
;; jade-chrome.el and jade-nodejs.el.  This backend currently supports the REPL,
;; code completion, object inspection and the debugger.
;;
;; The protocol is documented at
;; https://chromedevtools.github.io/debugger-protocol-viewer/1-2/.

;;; Code:

(require 'websocket)
(require 'json)
(require 'map)
(require 'seq)
(require 'cl-lib)

(require 'jade-backend)
(require 'jade-repl)
(require 'jade-debugger)


(defvar jade-webkit-completion-function "function getCompletions(type)\n{var object;if(type==='string')\nobject=new String('');else if(type==='number')\nobject=new Number(0);else if(type==='boolean')\nobject=new Boolean(false);else\nobject=this;var resultSet={};for(var o=object;o;o=o.__proto__){try{if(type==='array'&&o===object&&ArrayBuffer.isView(o)&&o.length>9999)\ncontinue;var names=Object.getOwnPropertyNames(o);for(var i=0;i<names.length;++i)\nresultSet[names[i]]=true;}catch(e){}}\nreturn resultSet;}")

(jade-register-backend 'webkit)

(cl-defmethod jade-backend-active-connection-p ((backend (eql webkit)) connection)
  "Return non-nil if CONNECTION is active."
  (websocket-openp (map-elt connection 'ws)))

(cl-defmethod jade-backend-close-connection ((backend (eql webkit)) connection)
  "Close the websocket associated with CONNECTION."
  (websocket-close (map-elt connection 'ws)))

(cl-defmethod jade-backend-reconnect ((backend (eql webkit)))
  (let* ((connection jade-connection)
         (url (map-elt connection 'url))
         (websocket-url (websocket-url (map-elt connection 'ws))))
    (jade-webkit--open-ws-connection url
                                     websocket-url
                                     ;; close all buffers related to the closed
                                     ;; connection the first
                                     #'jade-quit)))

(cl-defmethod jade-backend-evaluate ((backend (eql webkit)) string &optional callback)
  "Evaluate STRING then call CALLBACK.
CALLBACK is called with two arguments, the value returned by the
evaluation and non-nil if the evaluation threw an error."
  (jade-webkit--send-request
   `((method . "Runtime.evaluate")
     (params . ((expression . ,string)
                (generatePreview . t))))
   (lambda (response)
     (when callback
      (jade-webkit--handle-evaluation-response response callback)))))

(cl-defmethod jade-backend-evaluate-on-frame ((backend (eql webkit)) string frame &optional callback)
  "Evaluate STRING on the call frame FRAME then call CALLBACK.
CALLBACK is called with two arguments, the value returned by the
evaluation and non-nil if the evaluation threw an error."
  (jade-webkit--send-request
   `((method . "Debugger.evaluateOnCallFrame")
     (params . ((expression . ,string)
                (callFrameId . ,(map-elt frame 'callFrameId))
                (generatePreview . t))))
   (lambda (response)
     (jade-webkit--handle-evaluation-response response callback))))

(cl-defmethod jade-backend-get-completions ((backend (eql webkit)) expression prefix callback)
  "Get the completion candidates for EXPRESSION that match PREFIX.
Evaluate CALLBACK on the filtered candidates."
  (let ((expression (jade-webkit--completion-expression expression)))
    (jade-webkit--send-request
     `((method . "Runtime.evaluate")
       (params . ((expression . ,expression)
                  (objectGroup . "completion"))))
     (lambda (response)
       (jade-webkit--handle-completions-response response prefix callback)))))

(cl-defmethod jade-backend-get-properties ((backend (eql webkit)) reference &optional callback all-properties)
  "Get the properties of the remote object represented by REFERENCE.
CALLBACK is evaluated with the list of properties.

If ALL-PROPERTIES is non-nil, get all the properties from the
prototype chain of the remote object."
  (jade-webkit--send-request
   `((method . "Runtime.getProperties")
     (params . ((objectId . ,reference)
                (ownProperties . ,(not all-properties)))))
   (lambda (response)
     (funcall callback
              (jade-webkit--properties
               (map-nested-elt response '(result result)))))))

(cl-defmethod jade-backend-get-script-source ((backend (eql webkit)) frame callback)
  (let ((script-id (map-nested-elt frame '(location scriptId))))
   (jade-webkit--send-request
    `((method . "Debugger.getScriptSource")
      (params . ((scriptId . ,script-id))))
    callback)))

(cl-defmethod jade-backend-get-script-url ((backend (eql webkit)) frame)
  (let ((script-id (map-nested-elt frame '(location scriptId))))
    (jade-webkit--get-script-url script-id)))

(cl-defmethod jade-backend-resume ((backend (eql webkit)) &optional callback)
  "Resume the debugger and evaluate CALLBACK if non-nil."
  (jade-webkit--send-request
   `((method . "Debugger.resume"))
   callback))

(cl-defmethod jade-backend-step-into ((backend (eql webkit)) &optional callback)
  "Step into the current stack frame and evaluate CALLBACK if non-nil."
  (jade-webkit--send-request
   `((method . "Debugger.stepInto"))
   callback))

(cl-defmethod jade-backend-step-out ((backend (eql webkit)) &optional callback)
  "Step out the current stack frame and evaluate CALLBACK if non-nil."
  (jade-webkit--send-request
   `((method . "Debugger.stepOut"))
   callback))

(cl-defmethod jade-backend-step-over ((backend (eql webkit)) &optional callback)
  "Step over the current stack frame and evaluate CALLBACK if non-nil."
  (jade-webkit--send-request
   `((method . "Debugger.stepOver"))
   callback))

(cl-defmethod jade-backend-continue-to-location ((backend (eql webkit)) location &optional callback)
  "Continue to LOCATION and evaluate CALLBACK if non-nil.

Location should be an alist with a `limeNumber' and `scriptId' key."
  (jade-webkit--send-request
   `((method . "Debugger.continueToLocation")
     (params . ((location . ,location))))
   callback))

(defun jade-webkit-set-overlay-message (string)
  "Sets the overlay string displayed when entering the debugger."
  (jade-webkit--send-request
   `((method . "Page.setOverlayMessage")
     (params . ((message . ,string))))))

(defun jade-webkit-remove-overlay-message ()
  "Remove any overlay message displayed on the page."
  (jade-webkit--send-request
   `((method . "Page.setOverlayMessage"))))

(defun jade-webkit-set-pause-on-exceptions (state)
  "Defines on which STATE to pause.

Can be set to stop on all exceptions, uncaught exceptions or no
exceptions. Initial pause on exceptions state is set by Jade to
`\"uncaught\"'.

Allowed states: `\"none\"', `\"uncaught\"', `\"all\"'."
  (interactive (list (completing-read "Pause on exceptions: "
                                      '("none" "uncaught" "all")
                                      nil
                                      t)))
  (jade-webkit--send-request `((method . "Debugger.setPauseOnExceptions")
                               (params . ((state . ,state))))))

(defun jade-webkit--open-ws-connection (url websocket-url &optional on-open)
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
                             (jade-webkit--handle-ws-open ws url))
                  :on-message #'jade-webkit--handle-ws-message
                  :on-close #'jade-webkit--handle-ws-closed
                  :on-error #'jade-webkit--handle-ws-error))

(defun jade-webkit--make-connection (ws url)
  "Return a new connection for WS and URL."
  (let ((connection (make-hash-table)))
    (map-put connection 'ws ws)
    (map-put connection 'url url)
    (map-put connection 'backend 'webkit)
    (map-put connection 'callbacks (make-hash-table))
    (add-to-list 'jade-connections connection)
    connection))

(defun jade-webkit--callbacks ()
  "Return the callbacks associated with the current connection."
  (map-elt jade-connection 'callbacks))

(defun jade-webkit--connection-for-ws (ws)
  "Return the webkit connection associated with the websocket WS."
  (seq-find (lambda (connection)
              (eq (map-elt connection 'ws) ws))
            jade-connections))

(defun jade-webkit--handle-ws-open (ws url)
  (let* ((connection (jade-webkit--make-connection ws url)))
    (jade-with-connection connection
      (jade-webkit--enable-tools))
    (switch-to-buffer (jade-repl-get-buffer-create connection))))

(defun jade-webkit--handle-ws-message (ws frame)
  (jade-with-connection (jade-webkit--connection-for-ws ws)
    (let* ((message (jade-webkit--read-ws-message frame))
           (error (map-elt message 'error))
           (method (map-elt message 'method))
           (request-id (map-elt message 'id))
           (callback (map-elt (jade-webkit--callbacks) request-id)))
      (cond
       (error (message (map-elt error 'message)))
       (request-id (when callback
                     (funcall callback message)))
       (t (pcase method
            ("Inspector.detached" (jade-webkit--handle-inspector-detached message))
            ("Console.messageAdded" (jade-webkit--handle-console-message message))
            ("Debugger.paused" (jade-webkit--handle-debugger-paused message))
            ("Debugger.scriptParsed" (jade-webkit--handle-script-parsed message))
            ("Debugger.resumed" (jade-webkit--handle-debugger-resumed message))))))))

(defun jade-webkit--handle-inspector-detached (message)
  "Handle connection closed because it was detached."
  (let ((msg (map-nested-elt message '(params reason))))
    (jade-backend-close-connection 'webkit jade-connection)
    (message "Jade connection closed: %s" msg)))

(defun jade-webkit--handle-console-message (message)
  (let* ((msg (map-nested-elt message '(params message)))
         (parameters (map-elt msg 'parameters)))
    (setf (map-elt msg 'parameters) (seq-map #'jade-webkit--value parameters))
    (jade-repl-emit-console-message msg)))

(defun jade-webkit--handle-debugger-paused (message)
  (let ((frames (map-nested-elt message '(params callFrames))))
    (jade-webkit-set-overlay-message "Paused in Emacs debugger")
    (jade-debugger-paused (jade-webkit--frames frames))))

(defun jade-webkit--handle-debugger-resumed (_message)
  (jade-webkit-remove-overlay-message)
  (jade-debugger-resumed))

(defun jade-webkit--handle-script-parsed (message)
  (let* ((scriptId (map-nested-elt message '(params scriptId)))
         (url (map-nested-elt message '(params url))))
    (jade-webkit--add-script-parsed scriptId url)))

(defun jade-webkit--handle-ws-closed (_ws)
  (jade-repl--handle-connection-closed))

(defun jade-webkit--handle-ws-error (ws action error)
  (message "WS Error! %s %s" action error))

(defun jade-webkit--send-request (request &optional callback)
  "Send REQUEST to the current connection.
Evaluate CALLBACK with the response.

If the current connection is closed, display an error message in
the REPL buffer."
  (if (jade-webkit--connected-p)
      (let ((id (jade-webkit--next-request-id))
            (callbacks (jade-webkit--callbacks)))
        (when callback
          (map-put callbacks id callback))
        (websocket-send-text (map-elt jade-connection 'ws)
                             (json-encode (cons `(id . ,id) request))))
    (jade-repl-emit-console-message '((level . "error") (text . "Socket connection closed")))))

(defun jade-webkit--read-ws-message (frame)
  (json-read-from-string (websocket-frame-payload frame)))

(defun jade-webkit--enable-tools ()
  "Enable developer tools for the current tab.

There is currently no support for the DOM inspector and network
inspectors."
  (jade-webkit--enable-console)
  (jade-webkit--enable-runtime)
  (jade-webkit--enable-page)
  (jade-webkit--enable-debugger))

(defun jade-webkit--enable-console ()
  "Enable the console on the current tab."
  (jade-webkit--send-request '((method . "Console.enable"))))

(defun jade-webkit--enable-page ()
  "Enable the page API on the current tab."
  (jade-webkit--send-request '((method . "Page.enable"))))

(defun jade-webkit--enable-runtime ()
  "Enable the runtime on the current tab."
  (jade-webkit--send-request '((method . "Runtime.enable")))
  (jade-webkit--send-request '((method . "Runtime.run"))))

(defun jade-webkit--enable-debugger ()
  "Enable the debugger on the current tab."
  (jade-webkit--send-request '((method . "Debugger.enable"))
                             (lambda (&rest _)
                               (jade-webkit-set-pause-on-exceptions "uncaught"))))

(defun jade-webkit--handle-evaluation-response (response callback)
  "Get the result of an evaluation in RESPONSE and evaluate CALLBACK with it."
  (let* ((result (map-nested-elt response '(result result)))
         (error (eq (map-nested-elt response '(result wasThrown)) t)))
    (funcall callback (jade-webkit--value result) error)))

(defun jade-webkit--handle-completions-response (response prefix callback)
  "Request a completion list for the object in RESPONSE.
The completion list is filtered using the PREFIX string, then
CALLBACK is evaluated with it."
  (let ((objectid (map-nested-elt response '(result result objectId)))
        (type (map-nested-elt response '(result result type))))
    (if objectid
        (jade-webkit--get-completion-list-by-reference objectid prefix callback)
      (jade-webkit--get-completion-list-by-type type prefix callback))))

(defun jade-webkit--get-completion-list-by-reference (objectid prefix callback)
  "Request the completion list for a remote object referenced by OBJECTID.
The completion list is filtered using the PREFIX string, then
CALLBACK is evaluated with it."
  (jade-webkit--send-request
   `((method . "Runtime.callFunctionOn")
     (params . ((objectId . ,objectid)
                (functionDeclaration . ,jade-webkit-completion-function)
                (returnByValue . t))))
   (lambda (response)
     (jade-webkit--handle-completion-list-response response prefix callback))))

(defun jade-webkit--get-completion-list-by-type (type prefix callback)
  "Request the completion list for an object of type TYPE.
The completion list is filtered using the PREFIX string, then
CALLBACK is evaluated with it.

This method is used for strings, numbers and booleans.  See
`jade-webkit--get-completion-list-by-reference' for getting
completions using references to remote objects (including
arrays)."
  (let ((expression (format "(%s)(\"%s\")" jade-webkit-completion-function type)))
    (jade-webkit--send-request
     `((method . "Runtime.evaluate")
       (params . ((expression . ,expression)
                  (returnByValue . t))))
     (lambda (response)
       (jade-webkit--handle-completion-list-response response prefix callback)))))

(defun jade-webkit--completion-expression (string)
  "Return the completion expression to be requested from STRING."
  (if (string-match-p "\\." string)
      (replace-regexp-in-string "\\.[^\\.]*$" "" string)
    "this"))

(defun jade-webkit--handle-completion-list-response (response prefix callback)
  "Evauate CALLBACK on the completion candidates from RESPONSE.
Candidates are filtered using the PREFIX string."
  (let ((candidates (map-nested-elt response '(result result value))))
    (funcall callback (seq-filter (lambda (candidate)
                                    (string-prefix-p prefix candidate))
                                  (seq-map (lambda (candidate)
                                             (symbol-name (car candidate)))
                                           candidates)))))

(cl-defmethod jade-webkit--connected-p ()
  "Return non-nil if the current connection is open."
  (jade-backend-active-connection-p 'webkit jade-connection))

(defun jade-webkit--value (result)
  "Return an alist representing the value of RESULT.

The returned value can be a reference to a remote object, in
which case the value associated to the `objectid' key is
non-nil."
  (let* ((value (map-elt result 'value))
         (type (intern (map-elt result 'type)))
         (objectid (map-elt result 'objectId))
         (preview (jade-webkit--preview result))
         (description (jade-webkit--description result)))
    `((objectid . ,objectid)
      (description . ,description)
      (type . ,type)
      (preview . ,preview)
      (value . ,value))))

(defun jade-webkit--description (result)
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

(defun jade-webkit--preview (result)
  "Return a preview string built from RESULT.
RESULT should be a reference to a remote object."
  (let* ((preview (map-elt result 'preview))
         (subtype (map-elt preview 'subtype)))
    (if (string= subtype "array")
        (jade-webkit--preview-array preview)
      (jade-webkit--preview-object preview))))

(defun jade-webkit--preview-object (preview)
  "Return a preview string from the properties of the object PREVIEW."
  (concat " { "
          (mapconcat (lambda (prop)
                       (format "%s: %s"
                               (map-elt prop 'name)
                               (jade-webkit--description prop)))
                     (map-elt preview 'properties)
                     ", ")
          (if (eq (map-elt preview 'lossless) :json-false)
              ", … }"
            " }")))

(defun jade-webkit--preview-array (preview)
  "Return a preview string from the elements of the array PREVIEW."
  (concat " [ "
          (mapconcat (lambda (prop)
                       (format "%s" (jade-webkit--description prop)))
                     (map-elt preview 'properties)
                     ", ")
          (if (eq (map-elt preview 'lossless) :json-false)
              "… ]"
            " ]")))

(defun jade-webkit--properties (result)
  "Return a list of object properties built from RESULT."
  (seq-map (lambda (prop)
             `((name . ,(map-elt prop 'name))
               (value . ,(jade-webkit--value (or (map-elt prop 'value)
                                                 (map-elt prop 'get))))))
           result))

(defun jade-webkit--frames (list)
  "Return a list of frames built from LIST."
  (seq-map (lambda (frame)
             `((scope-chain . ,(seq-map (lambda (scope)
                                          `((object . ,(jade-webkit--value (map-elt scope 'object)))
                                            (name . ,(map-elt scope 'name))
                                            (type . ,(map-elt scope 'type))))
                                  (map-elt frame 'scopeChain)))
               (location . ,(map-elt frame 'location))
               (type . ,(map-elt frame 'type))
               (functionName . ,(map-elt frame 'functionName))
               (callFrameId . ,(map-elt frame 'callFrameId))))
           list))

(defun jade-webkit--add-script-parsed (scriptId url)
  (unless (map-elt jade-connection 'scripts)
    (map-put jade-connection 'scripts '()))
  (map-put (map-elt jade-connection 'scripts)
           (intern scriptId)
           url))

(defun jade-webkit--get-script-url (scriptId)
  (map-nested-elt jade-connection (list 'scripts (intern scriptId))))

(let ((id 0))
  (defun jade-webkit--next-request-id ()
    "Return the next unique identifier to be used in a request."
    (cl-incf id)))

(provide 'jade-webkit)
;;; jade-webkit.el ends here
