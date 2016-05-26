;;; jade.el --- JavaScript Awesome Development Environment  -*- lexical-binding: t; -*-

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

;;

;;; Code:

(require 'websocket)
(require 'url)
(require 'json)
(require 'map)
(require 'seq)

(require 'jade-repl)
(require 'jade-debugger)

(defvar jade-connections (list) "List of connections.")

(defvar jade-connection nil
  "Current connection to the browser tab.

A connection should be an alist with the following required keys:
`backend' and `url'.  Other backend-specific keys might be used
by backends.")

(make-variable-buffer-local 'jade-connection)

(defvar jade-backend-completion-function "function getCompletions(type)\n{var object;if(type===\"string\")\nobject=new String(\"\");else if(type===\"number\")\nobject=new Number(0);else if(type===\"boolean\")\nobject=new Boolean(false);else\nobject=this;var resultSet={};for(var o=object;o;o=o.__proto__){try{if(type===\"array\"&&o===object&&ArrayBuffer.isView(o)&&o.length>9999)\ncontinue;var names=Object.getOwnPropertyNames(o);for(var i=0;i<names.length;++i)\nresultSet[names[i]]=true;}catch(e){}}\nreturn resultSet;}")

(defun jade-connect (host port)
  "Open a connection to a browser tab on HOST:PORT."
  (interactive (list (read-from-minibuffer "Host: " "127.0.0.1")
                     (read-from-minibuffer "Port: " "9222")))
  (jade-backend-get-tabs-data host port #'jade-backend-connect-to-tab))

(defun jade-quit ()
  "Close the current connection and kill its REPL buffer if any."
  (interactive)
  (unless jade-connection
    (user-error "No active connection to close"))
  (when (y-or-n-p (format "Do you really want to close the connection to %s ? "
                          (map-elt jade-connection 'url)))
    (when (jade-connected-p)
      (websocket-close (map-elt jade-connection 'ws)))
    (map-put jade-connection 'callbacks nil)
    (setq jade-connections (remq jade-connection jade-connections))
    (kill-buffer (jade-repl-get-buffer))))

(defun jade-backend-get-tabs-data (host port callback)
  "Get the list of open tabs on HOST:PORT and evaluate CALLBACK with it."
  (url-retrieve (format "http://%s:%s/json" host port)
                (lambda (status)
                  ;; TODO: handle errors
                  (funcall callback (jade-backend--read-tab-data)))))

(defun jade-backend-connect-to-tab (tabs)
  "Ask the user for a tab in the list TABS and connects to it."
  (let* ((titles (seq-map (lambda (tab)
                            (map-elt tab 'title))
                          tabs))
         (title (completing-read "Tab: " titles nil t)))
    (jade-backend--open-ws-connection (seq-find (lambda (tab)
                                                 (string= (map-elt tab 'title) title))
                                               tabs))))

(defun jade-backend--read-tab-data ()
  "Return the JSON tabs data in the current buffer."
  (when (save-match-data
          (looking-at "^HTTP/1\\.1 200 OK$"))
    (goto-char (point-min))
    (search-forward "\n\n")
    (delete-region (point-min) (point))
    (json-read)))

(defun jade-backend--open-ws-connection (tab)
  "Open a websocket connection to the `webSocketDebuggerUrl' of TAB.

If TAB does not have a `webSocketDebuggerUrl', throw a user
error.  This might happen when trying to connect to a tab twice,
or if an inspector is open on that tab."
  (let ((url (map-elt tab 'url))
        (debugger-url (map-elt tab 'webSocketDebuggerUrl)))
    (unless debugger-url
      (user-error "Cannot open connection, another devtools instance might be open"))
   (websocket-open debugger-url
                   :on-open (lambda (ws) (jade-backend-handle-ws-open ws url))
                   :on-message #'jade-backend-handle-ws-message
                   :on-close #'jade-backend-handle-ws-closed
                   :on-error #'jade-backend-handle-ws-error)))

(defun jade-backend--make-connection (ws url)
  "Return a new connection for WS and URL."
  (let ((connection `((ws . ,ws)
                      (url . ,url)
                      (backend . chrome)
                      (callbacks . ,(make-hash-table)))))
    (add-to-list 'jade-connections connection)
    connection))

(defun jade-backend--callbacks ()
  "Return the callbacks associated with the current connection."
  (map-elt jade-connection 'callbacks))

(defun jade-backend--connection-for-ws (ws)
  "Return the connection associated with the websocket WS."
  (seq-find (lambda (connection)
              (eq (map-elt connection 'ws) ws))
            jade-connections))

(defun jade-backend-handle-ws-open (ws url)
  (let* ((connection (jade-backend--make-connection ws url))
         (jade-connection connection))
    (jade-backend-enable-tools)
    (switch-to-buffer (jade-repl-get-buffer-create connection))))

(defun jade-backend-handle-ws-message (ws frame)
  (let* ((jade-connection (jade-backend--connection-for-ws ws))
         (message (jade-backend--read-ws-message frame))
         (error (map-elt message 'error))
         (method (map-elt message 'method))
         (request-id (map-elt message 'id))
         (callback (map-elt (jade-backend--callbacks) request-id)))
    (cond
     (error (message (map-elt error 'message)))
     (request-id (when callback
                   (funcall callback message)))
     (t (pcase method
          ("Console.messageAdded" (jade-backend-handle-console-message message))
          ("Debugger.paused" (jade-backend-handle-debugger-paused message))
          ("Debugger.resumed" (jade-backend-handle-debugger-resumed message)))))))

(defun jade-backend-handle-console-message (message)
  (let* ((level (map-nested-elt message '(params message level)))
         (text (map-nested-elt message '(params message text))))
    (jade-repl-emit-console-message text level)))

(defun jade-backend-handle-debugger-paused (message)
  (let ((frames (map-nested-elt message '(params callFrames))))
    (jade-debugger-paused (jade-backend-frames frames))))

(defun jade-backend-handle-debugger-resumed (_message)
  (jade-debugger-resumed))

(defun jade-backend-handle-ws-closed (_ws)
  )

(defun jade-backend-handle-ws-error (ws action error)
  (message "WS Error! %s %s" action error))

(defun jade-backend-send-request (request &optional callback)
  "Send REQUEST to the current connection.
Evaluate CALLBACK with the response.

If the current connection is closed, display an error message in
the REPL buffer."
  (when (not (jade-connected-p))
    (jade-repl-emit-console-message "Socket connection closed" "error"))
  (let ((id (jade-next-request-id))
        (callbacks (jade-backend--callbacks)))
    (when callback
      (map-put callbacks id callback))
    (websocket-send-text (map-elt jade-connection 'ws)
                         (json-encode (cons `(id . ,id) request)))))

(defun jade-backend--read-ws-message (frame)
  (with-temp-buffer
    (insert (websocket-frame-payload frame))
    (goto-char (point-min))
    (json-read)))

(defun jade-backend-enable-tools ()
  "Enable developer tools for the current tab.

There is currently no support for the DOM inspector and network
inspectors."
  (jade-backend-enable-console)
  (jade-backend-enable-runtime)
  (jade-backend-enable-debugger))

(defun jade-backend-enable-console ()
  "Enable the console on the current tab."
  (jade-backend-send-request '((method . "Console.enable"))))

(defun jade-backend-enable-runtime ()
  "Enable the runtime on the current tab."
  (jade-backend-send-request '((method . "Runtime.enable"))))

(defun jade-backend-enable-debugger ()
  "Enable the debugger on the current tab."
  (jade-backend-send-request '((method . "Debugger.enable"))))

(defun jade-backend-evaluate (string &optional callback)
  "Evaluate STRING then call CALLBACK.
CALLBACK is called with two arguments, the value returned by the
evaluation and non-nil if the evaluation threw an error."
  (jade-backend-send-request
   `((method . "Runtime.evaluate")
     (params . ((expression . ,string)
                (generatePreview . t))))
   (lambda (response)
     (jade-backend-handle-evaluation-response response callback))))

(defun jade-backend-handle-evaluation-response (response callback)
  "Get the result of an evaluation in RESPONSE and evaluate CALLBACK with it."
  (let* ((result (map-nested-elt response '(result result)))
         (error (eq (map-nested-elt response '(result wasThrown)) t)))
    (funcall callback (jade-backend-value result) error)))

(defun jade-backend-get-completions (expression prefix callback)
  "Get the completion candidates for EXPRESSION that match PREFIX.
Evaluate CALLBACK on the filtered candidates."
  (let ((expression (jade-backend-completion-expression expression)))
    (jade-backend-send-request
     `((method . "Runtime.evaluate")
       (params . ((expression . ,expression)
                  (objectGroup . "completion"))))
     (lambda (response)
       (jade-backend-handle-completions-response response prefix callback)))))

(defun jade-backend-handle-completions-response (response prefix callback)
  "Request a completion list for the object in RESPONSE.
The completion list is filtered using the PREFIX string, then
CALLBACK is evaluated with it."
  (let ((objectid (map-nested-elt response '(result result objectId)))
        (type (map-nested-elt response '(result result type))))
    (if objectid
        (jade-backend-get-completion-list-by-reference objectid prefix callback)
      (jade-backend-get-completion-list-by-type type prefix callback))))

(defun jade-backend-get-completion-list-by-reference (objectid prefix callback)
  "Request the completion list for a remote object referenced by OBJECTID.
The completion list is filtered using the PREFIX string, then
CALLBACK is evaluated with it."
  (jade-backend-send-request
   `((method . "Runtime.callFunctionOn")
     (params . ((objectId . ,objectid)
                (functionDeclaration . ,jade-backend-completion-function)
                (returnByValue . t))))
   (lambda (response)
     (jade-backend-handle-completion-list-response response prefix callback))))

(defun jade-backend-get-completion-list-by-type (type prefix callback)
  "Request the completion list for an object of type TYPE.
The completion list is filtered using the PREFIX string, then
CALLBACK is evaluated with it.

This method is used for strings, numbers and booleans.  See
`jade-backend-get-completion-list-by-reference' for getting
completions using references to remote objects (including
arrays)."
  (let ((expression (format "(%s)(\"%s\")" jade-backend-completion-function type)))
    (jade-backend-send-request
     `((method . "Runtime.evaluate")
       (params . ((expression . ,expression)
                  (returnByValue . t))))
     (lambda (response)
       (jade-backend-handle-completion-list-response response prefix callback)))))

(defun jade-backend-completion-expression (string)
  "Return the completion expression to be requested from STRING."
  (if (string-match-p "\\." string)
      (replace-regexp-in-string "\\.[^\\.]*$" "" string)
    "this"))

(defun jade-backend-handle-completion-list-response (response prefix callback)
  "Evauate CALLBACK on the completion candidates from RESPONSE.
Candidates are filtered using the PREFIX string."
  (let ((candidates (map-nested-elt response '(result result value))))
    (funcall callback (seq-filter (lambda (candidate)
                                    (string-prefix-p prefix candidate))
                                  (seq-map (lambda (candidate)
                                             (symbol-name (car candidate)))
                                           candidates)))))

(defun jade-backend-get-properties (reference &optional callback all-properties)
  "Get the properties of the remote object represented by REFERENCE.
CALLBACK is evaluated with the list of properties.

If ALL-PROPERTIES is non-nil, get all the properties from the
prototype chain of the remote object."
  (jade-backend-send-request
   `((method . "Runtime.getProperties")
     (params . ((objectId . ,reference)
                (ownProperties . ,(not all-properties)))))
   (lambda (response)
     (funcall callback
              (jade-backend-properties
               (map-nested-elt response '(result result)))))))

(defun jade-backend-get-script-source (frame callback)
  (let ((script-id (map-nested-elt frame '(location scriptId))))
   (jade-backend-send-request
    `((method . "Debugger.getScriptSource")
      (params . ((scriptId . ,script-id))))
    callback)))

(defun jade-backend-resume (&optional callback)
  "Resume the debugger and evaluate CALLBACK if non-nil."
  (jade-backend-send-request
   `((method . "Debugger.resume"))
   callback))

(defun jade-backend-step-into (&optional callback)
  "Step into the current stack frame and evaluate CALLBACK if non-nil."
  (jade-backend-send-request
   `((method . "Debugger.stepInto"))
   callback))

(defun jade-backend-step-out (&optional callback)
  "Step out the current stack frame and evaluate CALLBACK if non-nil."
  (jade-backend-send-request
   `((method . "Debugger.stepOut"))
   callback))

(defun jade-backend-step-over (&optional callback)
  "Step over the current stack frame and evaluate CALLBACK if non-nil."
  (jade-backend-send-request
   `((method . "Debugger.stepOver"))
   callback))

(defun jade-backend-continue-to-location (location &optional callback)
  "Continue to LOCATION and evaluate CALLBACK if non-nil.

Location should be an alist with a `column' and `row' key."
  (jade-backend-send-request
   `((method . "Debugger.continueToLocation")
     (params . ((location . ,location))))
   callback))

(defun jade-backend-value (result)
  "Return an alist representing the value of RESULT.

The returned value can be a reference to a remote object, in
which case the value associated to the `objectid' key is
non-nil."
  (let* ((value (map-elt result 'value))
         (type (intern (map-elt result 'type)))
         (objectid (map-elt result 'objectId))
         (preview (jade-backend-preview result))
         (description (jade-backend-description result)))
    `((objectid . ,objectid)
      (description . ,description)
      (type . ,type)
      (preview . ,preview)
      (value . ,value))))

(defun jade-backend-description (result)
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

(defun jade-backend-preview (result)
  "Return a preview string built from RESULT.
RESULT should be a reference to a remote object."
  (let* ((preview (map-elt result 'preview))
         (subtype (map-elt preview 'subtype)))
    (if (string= subtype "array")
        (jade-backend--preview-array preview)
      (jade-backend--preview-object preview))))

(defun jade-backend--preview-object (preview)
  "Return a preview string from the properties of the object PREVIEW."
  (concat " { "
          (mapconcat (lambda (prop)
                       (format "%s: %s"
                               (map-elt prop 'name)
                               (jade-backend-description prop)))
                     (map-elt preview 'properties)
                     ", ")
          (if (eq (map-elt preview 'lossless) :json-false)
              ", … }"
            " }")))

(defun jade-backend--preview-array (preview)
  "Return a preview string from the elements of the array PREVIEW."
  (concat " [ "
          (mapconcat (lambda (prop)
                       (format "%s" (jade-backend-description prop)))
                     (map-elt preview 'properties)
                     ", ")
          (if (eq (map-elt preview 'lossless) :json-false)
              "… ]"
            " ]")))

(defun jade-backend-properties (result)
  "Return a list of object properties built from RESULT."
  (seq-map (lambda (prop)
             `((name . ,(map-elt prop 'name))
               (value . ,(jade-backend-value (or (map-elt prop 'value)
                                                 (map-elt prop 'get))))))
           result))

(defun jade-backend-frames (list)
  "Return a list of frames built from LIST."
  (seq-map (lambda (frame)
             `((scope-chain . ,(seq-map (lambda (scope)
                                          `((object . ,(jade-backend-value (map-elt scope 'object)))
                                            (name . ,(map-elt scope 'name))
                                            (type . ,(map-elt scope 'type))))
                                  (map-elt frame 'scopeChain)))
               (location . ,(map-elt frame 'location))))
           list))

(defun jade-backend-object-reference-p (value)
  "Return non-nil if VALUE is a reference to a remote object."
  (map-elt value 'objectid))

(defun jade-connected-p ()
  "Return non-nil if the current connction is open."
  (websocket-openp (map-elt jade-connection 'ws)))

(let ((id 0))
  (defun jade-next-request-id ()
    "Return the next unique identifier to be used in a request."
    (setq id (1+ id))
    id))

(provide 'jade)
;;; jade.el ends here
