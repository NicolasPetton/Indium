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
(require 'json-process-client)

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

(defvar indium-client--application nil
  "The client connection as returned by `json-process-client-start'.")

(defvar indium-client--process-port 13840
  "The port on which the server should listen.")

(defun indium-client-start (callback)
  "Start an Indium process and store it as the client process.
Evaluate CALLBACK once the server is started."
  (when (indium-client-process-live-p)
    (user-error "An indium process is already running"))
  (let ((executable (executable-find indium-client-executable)))
    (unless executable
      (user-error "Cannot find the indium executable.  Please run \"npm install -g indium\""))
    (setq indium-client--application
          (json-process-client-start-with-id
           :name "indium"
           :executable executable
           :port indium-client--process-port
           :started-regexp "server listening"
           :tcp-started-callback callback

           :exec-callback #'indium-client--handle-message
           :debug "*indium-debug-log*"
           :args (list (number-to-string indium-client--process-port))))))

(defun indium-client-stop ()
  "Stop the indium process."
  (json-process-client-stop indium-client--application)
  (setq indium-client--application nil)
  (run-hooks 'indium-client-closed-hook))

(defun indium-client-send (message &optional callback)
  "Send MESSAGE to the Indium process.
When CALLBACK is non-nil, evaluate it with the process response."
  (json-process-client-send indium-client--application message callback))


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

(defun indium-client-disconnect (&optional callback)
  "Disconnect from the runtime, but do not stop the indium process.

When non-nil, evaluate CALLBACK with the result."
  (indium-client-send `((type . "connection")
                        (payload . ((action . "disconnect"))))
                      callback))

(defun indium-client-evaluate (expression &optional frame callback)
  "Evaluate EXPRESSION in the context of FRAME.

When non-nil, evaluate CALLBACK with the result."
  (indium-client-send
   `((type . "runtime")
     (payload . ((action . "evaluate")
		 (expression . ,expression)
                 (frameId . ,(when frame (indium-frame-id frame))))))
   (lambda (obj)
     (when callback
       (funcall callback (indium-remote-object-from-alist obj))))))

(defun indium-client-get-completion (expression &optional frame callback)
  "Request the list of completion for EXPRESSION in the context of FRAME.
When non-nil, evaluate CALLBACK with the result."
  (indium-client-send `((type . "runtime")
			(payload . ((action . "getCompletion")
                                    (expression . ,expression)
				    (frameId . ,(when frame (indium-frame-id frame))))))
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
	 (line (indium-location-line location))
	 (column (indium-location-column location)))
    (indium-client-send `((type . "runtime")
			  (payload . ((action . "addBreakpoint")
				      (id . ,id)
				      (file . ,(indium-client--convert-path file))
				      (line . ,line)
				      (column . ,column)))))))

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
		 (location . ((file . ,(indium-client--convert-path
					(indium-location-file location)))
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


(defun indium-client-process-live-p ()
  "Return non-nil if the indium process is running."
  (json-process-client-process-live-p indium-client--application))

(defun indium-client--handle-message (data callback)
  "Handle a server message with DATA.
If DATA is a successful response to a previously-sent message,
evaluate CALLBACK with the payload."
  (let-alist data
    (pcase .type
      ("error" (indium-client--handle-error .payload))
      ("success" (indium-client--handle-response .payload callback))
      ("notification" (indium-client--handle-notification .payload))
      ("log" (indium-client--handle-log .payload)))))

(defun indium-client--handle-error (payload)
  "Handle an error from the server.
PAYLOAD is an alist containing the details of the error."
  (let-alist payload
    (message "Indium server error: %s" .error)))

(defun indium-client--handle-response (payload callback)
  "Handle a response to a client request.
PAYLOAD contains the data of the response.

If CALLBACK is non-nil, evaluate it with the PAYLOAD."
  (when callback
    (unwind-protect
        (funcall callback payload))))

(defun indium-client--handle-log (payload)
  "Handle a log event from the server.

PAYLOAD is an alist with the details of the log event.
If has the following keys:
  type		type of message
  url		url of the message origin
  line		line number in the resource that generated this message
  result 	object to be logged."
  (setf (map-elt payload 'result nil #'equal) (indium-remote-object-from-alist
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

(defun indium-client--convert-path (path)
  "Convert PATH to a system path that the server component understands."
  (when (eq system-type 'windows-nt)
    (setq path (replace-regexp-in-string "/" "\\" path nil t)))
  path)

(provide 'indium-client)
;;; indium-client.el ends here
