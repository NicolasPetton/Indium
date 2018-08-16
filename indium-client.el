;;; indium-client.el --- Indium process client       -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Nicolas Petton

;; Author: Nicolas Petton <nicolas@petton.fr>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; The Indium process client starts and communicates with an "indium" process.
;;
;; Make sure to install the indium process with:
;;   npm install -g indium

;;; Code:


(require 'json)
(require 'map)
(require 'subr-x)

(require 'indium-structs)

(defcustom indium-client-closed-hook nil
  "Hook called after a client is closed."
  :group 'indium-client
  :type 'hook)

(defcustom indium-client-connected-hook nil
  "Hook called after a client is connected."
  :group 'indium-client
  :type 'hook)

(defcustom indium-client-log-hook nil
  "Hook called when a client receives a log event."
  :group 'indium-client
  :type 'hook)

(defcustom indium-client-breakpoint-resolved-hook nil
  "Hook called upon breakpoint resolution."
  :group 'indium-client
  :type 'hook)

(defcustom indium-client-debugger-resumed-hook nil
  "Hook called when the debugger is resumed."
  :group 'indium-client
  :type 'hook)

(defcustom indium-client-debugger-paused-hook nil
  "Hook called when the debugger is paused."
  :group 'indium-client
  :type 'hook)

(defvar indium-client-debug nil
  "When non-nil, log server output to *indium-client-log*.")

(defun indium-client-find-executable ()
  "Return the indium executable file."
  (if-let ((lisp-filename (or load-file-name (buffer-file-name))))
      (let ((executable (thread-last
                            (file-name-directory lisp-filename)
                          (expand-file-name "server")
                          (expand-file-name "bin")
                          (expand-file-name "indium"))))
        (if (file-executable-p executable)
            executable
          (indium-client-default-executable)))
    (indium-client-default-executable)))

(defun indium-client-default-executable ()
  "Return the default process executable."
  "indium")

(defcustom indium-client-executable (indium-client-find-executable)
  "Process executable."
  :group 'indium-client
  :type 'file)

(defvar indium-client--connection nil
  "The client connection to the server process.")

(defvar indium-client--process nil
  "The Indium server process.")

(defvar indium-client--process-port 13840
  "The port on which the server should listen.")

(defvar indium-client--callbacks nil
  "Alist of functions to be evaluated as callbacks on process response.")

(defun indium-client-start (callback)
  "Start an Indium process and store it as the client process.
Evaluate CALLBACK once the server is started."
  (when (indium-client-process-live-p)
    (user-error "An indium process is already running"))
  (let ((executable (executable-find indium-client-executable)))
    (unless executable
      (user-error "Cannot find the indium executable.  Please run \"npm install -g indium\""))
    (when indium-client-debug
      (with-current-buffer (get-buffer-create "*indium-debug-log*")
	(erase-buffer)))
    (indium-client--start-server executable callback)))

(defun indium-client-stop ()
  "Stop the indium process."
  (when (process-live-p indium-client--connection)
    (kill-buffer (process-buffer indium-client--process))
    (kill-buffer (process-buffer indium-client--connection)))
  (setq indium-client--connection nil)
  (setq indium-client--process nil)
  (setq indium-client--callbacks nil)
  (run-hooks 'indium-client-closed-hook))

(defun indium-client-send (message &optional callback)
  "Send MESSAGE to the Indium process.
When CALLBACK is non-nil, evaluate it with the process response."
  (indium-client--ensure-process)
  (let* ((id (indium-client--next-id))
	 (json (json-encode (cons `(id . ,id) message))))
    (map-put indium-client--callbacks id callback)
    (when indium-client-debug
      (with-current-buffer (get-buffer-create "*indium-debug-log*")
	(goto-char (point-max))
	(insert (format "Sent: %s\n\n" (cons `(id . ,id) message)))))
    (process-send-string indium-client--connection (format "%s\n" json))))


(defun indium-client-list-configurations (directory &optional callback)
  "Request the list of configurations found in DIRECTORY.

Evaluate CALLBACK with the result."
  (indium-client-send `((type . "configurations")
			(payload . ((action . "list")
                                    (directory . ,directory))))
                      callback))

(defun indium-client-connect (directory name)
  "Connect to a runtime.
DIRECTORY is the path of the directory where the project file can be found.
NAME is the name of the configuration to use for connecting.

Once the client is connected, run the hook `indium-client-connected-hook'."
  (indium-client-send `((type . "connection")
                        (payload . ((action . "connect")
				    (directory . ,directory)
                                    (name . ,name))))
                      (lambda (&rest _)
			(run-hooks 'indium-client-connected-hook))))

(defun indium-client-evaluate (expression &optional callback)
  "Evaluate EXPRESSION.

When non-nil, evaluate CALLBACK with the result."
  (indium-client-send
   `((type . "runtime")
     (payload . ((action . "evaluate")
		 (expression . ,expression))))
   (lambda (obj)
     (when callback
       (funcall callback (indium-remote-object-from-alist obj))))))

(defun indium-client-get-completion (expression &optional callback)
  "Request the list of completion for EXPRESSION.
When non-nil, evaluate CALLBACK with the result."
  (indium-client-send `((type . "runtime")
			(payload . ((action . "getCompletion")
                                    (expression . ,expression))))
                      callback))

(defun indium-client-get-properties (id &optional callback)
  "Request the list of properties for the remote object with ID.
When non-nil, evaluate CALLBACK with the result."
  (indium-client-send
   `((type . "runtime")
     (payload . ((action . "getProperties")
                 (id . ,id))))
   (lambda (properties)
     (when callback
       (funcall callback (seq-map #'indium-property-from-alist
				  properties))))))

(defun indium-client-activate-breakpoints ()
  "Activate all breakpoints."
  (indium-client-send `((type . "runtime")
			(payload . ((action . "activateBreakpoints"))))))

(defun indium-client-deactivate-breakpoints ()
  "Deactivate all breakpoints."
  (indium-client-send `((type . "runtime")
			(payload . ((action . "deactivateBreakpoints"))))))

(defun indium-client-add-breakpoint (breakpoint)
  "Request the addition of BREAKPOINT."
  (let* ((id (indium-breakpoint-id breakpoint))
	 (location (indium-breakpoint-location breakpoint))
	 (file (indium-location-file location))
	 (line (indium-location-line location)))
    (indium-client-send `((type . "runtime")
			  (payload . ((action . "addBreakpoint")
				      (id . ,id)
				      (file . ,file)
				      (line . ,line)))))))

(defun indium-client-remove-breakpoint (breakpoint)
  "Request the removal of BREAKPOINT."
  (let ((id (indium-breakpoint-id breakpoint)))
    (indium-client-send `((type . "runtime")
			  (payload . ((action . "removeBreakpoint")
				      (id . ,id)))))))

(defun indium-client-resume ()
  "Resume the runtime execution."
  (indium-client-send `((type . "runtime")
			(payload . ((action . "resume"))))))

(defun indium-client-step-into ()
  "Request a step into."
  (indium-client-send `((type . "runtime")
			(payload . ((action . "stepInto"))))))

(defun indium-client-step-out ()
  "Request a step out."
  (indium-client-send `((type . "runtime")
			(payload . ((action . "stepOut"))))))

(defun indium-client-step-over ()
  "Request a step over."
  (indium-client-send `((type . "runtime")
			(payload . ((action . "stepOver"))))))

(defun indium-client-continue-to-location (location)
  "Request the runtime to resume until LOCATION is reached."
  (indium-client-send
   `((type . "runtime")
     (payload . ((action . "continueToLocation")
		 (location . ((file . ,(indium-location-file location))
			      (line . ,(indium-location-line location))
			      (column . ,(indium-location-column location)))))))))

(defun indium-client-get-frame-source (frame &optional callback)
  "Request the source of FRAME.

When CALLBACK is non-nil, evaluate it with the source"
  (indium-client-send
   `((type . "runtime")
     (payload . ((action . "getSource")
		 (id . ,(indium-frame-script-id frame)))))
   callback))

(defun indium-client-get-sourcemap-sources (&optional callback)
  "Request the all the sourcemap source paths.

When CALLBACK is non-nil, evaluate it with the list of sources."
  (indium-client-send
   `((type . "runtime")
     (payload . ((action . "getSourcemapSources"))))
   callback))

(defun indium-client-get-script-sources (&optional callback)
  "Request the all the script source paths.

When CALLBACK is non-nil, evaluate it with the list of sources."
  (indium-client-send
   `((type . "runtime")
     (payload . ((action . "getScriptSources"))))
   callback))


(defun indium-client--ensure-process ()
  "Signal an error if the Indium is not started."
  (unless (indium-client-process-live-p)
    (user-error "Indium server not started")))

(defun indium-client-process-live-p ()
  "Return non-nil if the indium process is running."
  (process-live-p indium-client--connection))

(defun indium-client--start-server (executable callback)
  "Start the Indium server process in EXECUTABLE.

Evaluate CALLBACK once the server is started and the TCP
connection established."
  (setq indium-client--process
	(start-process "indium server"
		       (generate-new-buffer "*indium-process*")
                       executable
		       (format "%s" indium-client--process-port)))
  (set-process-query-on-exit-flag indium-client--process nil)
  (set-process-filter indium-client--process
		      (indium-client--process-filter-function callback)))

(defun indium-client--process-filter-function (callback)
  "Return a process filter function for an Indium server process.

Evaluate CALLBACK when the server starts listening to TCP connections."
  (lambda (process output)
    (with-current-buffer (process-buffer process)
      (goto-char (point-max))
      (insert output))
    (unless (process-live-p indium-client--connection) ;; do not try to open TCP connections multiple times
      (if (string-match-p "server listening" output)
	  (indium-client--open-network-stream callback)
	(progn
	  (indium-client-stop)
	  (error "Indium server process error: %s" output))))))

(defun indium-client--open-network-stream (callback)
  "Open a network connection to the indium server TCP process.
Evaluate CALLBACK once the connection is established."
  (let ((process (open-network-stream "indium"
				      (generate-new-buffer " indium-client-conn ")
				      "localhost"
				      indium-client--process-port)))
    (set-process-filter process #'indium-client--connection-filter)
    (set-process-coding-system process 'utf-8)
    (set-process-query-on-exit-flag process nil)
    ;; TODO: Set a process sentinel
    ;; (set-process-sentinel process #'indium-client--connection-sentinel)
    (setq indium-client--connection process)
    (funcall callback)))

(defun indium-client--connection-sentinel (callback)
  "Evaluate CALLBACK when the network process is open."
  (lambda (proc _event)
    (when (eq (process-status proc) 'open)
      (funcall callback))))

(defun indium-client--connection-filter (process output)
  "Filter function for handling the indium PROCESS OUTPUT."
  (let ((buf (process-buffer process)))
    (with-current-buffer buf
      (save-excursion
	(goto-char (point-max))
	(insert output)))
    (indium-client--handle-data buf)))

(defun indium-client--handle-data (buffer)
  "Handle process data in BUFFER.

Read the complete messages sequentially and handle them.  Each
read message is deleted from BUFFER."
  (let ((data))
    (with-current-buffer buffer
      (when (indium-client--complete-message-p)
	(save-excursion
	  (goto-char (point-min))
	  (setq data (ignore-errors (json-read)))
	  (delete-region (point-min) (point)))))
    (when data
      (indium-client--handle-message data)
      (indium-client--handle-data buffer))))

(defun indium-client--complete-message-p ()
  "Return non-nil if the current buffer has a complete message.
Messages end with a line feed."
  (save-excursion
    (goto-char (point-max))
    (search-backward "\n" nil t)))

(defun indium-client--handle-message (data)
  "Handle a server message with DATA."
  (when indium-client-debug
    (with-current-buffer (get-buffer-create "*indium-debug-log*")
      (goto-char (point-max))
      (insert (format "Received: %s\n\n" data))))
  (let-alist data
    (pcase .type
      ("error" (indium-client--handle-error .payload))
      ("success" (indium-client--handle-response .id .payload))
      ("notification" (indium-client--handle-notification .payload))
      ("log" (indium-client--handle-log .payload)))))

(defun indium-client--handle-error (payload)
  "Handle an error from the server.
PAYLOAD is an alist containing the details of the error."
  (let-alist payload
    (message "Indium server error: %s" .error)))

(defun indium-client--handle-response (id payload)
  "Handle a response to a client request.
ID is the id of the request for which the server has answered.
PAYLOAD contains the data of the response.

If a callback function has been registered for ID, evaluate it
with the PAYLOAD."
  (let ((callback (map-elt indium-client--callbacks id)))
    (when callback
      (unwind-protect
	  (funcall callback payload)
	(map-delete indium-client--callbacks id)))))

(defun indium-client--handle-log (payload)
  "Handle a log event from the server.

PAYLOAD is an alist with the details of the log event.
If has the following keys:
  type		type of message
  url		url of the message origin
  line		line number in the resource that generated this message
  result 	object to be logged."
  (map-put payload 'result (indium-remote-object-from-alist
			    (map-elt payload 'result)))
  (run-hook-with-args 'indium-client-log-hook
		      payload))

(defun indium-client--handle-notification (payload)
  "Handle a notification event sent from the server.
PAYLOAD is an alist with the details of the notification."
  (let-alist payload
    (pcase .type
      ("breakpointResolved"
       (progn
	 (run-hook-with-args 'indium-client-breakpoint-resolved-hook .id .line)))
      ("paused"
       (run-hook-with-args 'indium-client-debugger-paused-hook
			   (seq-map #'indium-frame-from-alist .frames)
			   .reason
			   .description))
      ("resumed"
       (run-hooks 'indium-client-debugger-resumed-hook))
      (_ (message "Indium notification %s" payload)))))

(defvar indium-client--id 0)
(defun indium-client--next-id ()
  "Return the next unique identifier to be used."
  (cl-incf indium-client--id))

(provide 'indium-client)
;;; indium-client.el ends here
