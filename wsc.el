;;; wsc.el --- WebSocket client                      -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Nicolas Petton

;; Author: Nicolas Petton <nicolas@petton.fr>
;; Keywords: network

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

;; WebSocket client built with fast data reception in mind.  It implements
;; RFC6455 (https://tools.ietf.org/html/rfc6455).
;;
;; There is currently no support for:
;; - fragmented frames
;; - sending or receiving binary data
;; - protocol extensions

;;; Code:


(require 'seq)
(require 'url-parse)
(require 'subr-x)

(eval-when-compile (require 'cl-lib))

(defconst wsc-protocol-version 13
  "Version of the WebSocket protocol used.")

(defconst wsc-guid "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
  "The WebSocket GUID from the RFC6455.")

(defvar wsc--current-process nil
  "Current WebSocket process.")

(defvar wsc--handshake-template (concat "GET %s HTTP/1.1\r\n"
					"Host: %s\r\n"
					"Upgrade: websocket\r\n"
					"Connection: Upgrade\r\n"
					"Sec-WebSocket-Key: %s\r\n"
					"Sec-WebSocket-Version: %s\r\n\r\n")
  "Template string used for sending handshake requests.")

(defvar wsc-debug nil
  "When non-nil, output all frames to the *wsc-dbg* buffer.")

(cl-defstruct (wsc-connection (:constructor wsc-connection-create)
			      (:copier nil))
  (state :connecting)
  (url nil)
  (process nil)
  (key (wsc--make-key))
  (on-message nil)
  (on-open nil)
  (on-close nil))

(defun wsc-connection-open-p (connection)
  "Return non-nil if CONNECTION's state is open."
  (eq (wsc-connection-state connection) :open))


(defun wsc-open (url &optional on-open on-message on-close)
  "Open a new WebSocket connection at URL and return it.
If non-nil, evaluate ON-OPEN when the connection is established.
If non-nil, evaluate ON-MESSAGE when a message is received.
If non-nil, evaluate ON-CLOSE when a the connection is closed."
  (let* ((parsed-url (url-generic-parse-url url))
	 (process (wsc--make-process parsed-url))
	 (connection (wsc-connection-create
		      :process process
		      :url parsed-url
		      :on-open on-open
		      :on-message on-message
		      :on-close on-close)))
    (process-put process :wsc-connection connection)
    (set-process-filter process #'wsc--process-filter)
    (set-process-coding-system process 'no-conversion)
    (set-process-query-on-exit-flag process nil)
    (set-process-sentinel process #'wsc--process-sentinel)
    (wsc--open-handshake connection)
    connection))

(defun wsc-close (connection)
  "Close CONNECTION."
  (unless (wsc-connection-open-p connection)
    (error "Connection not open, cannot send data"))
  (let ((input-buffer (wsc--create-data-buffer)))
    (unwind-protect
	(with-current-buffer input-buffer
	  (let ((wsc--current-process (wsc-connection-process connection)))
	    ;; The process might not be running, so ignore errors
	    (ignore-errors
	      (wsc--send-frame 8))))
      (kill-buffer input-buffer))))

(defun wsc-send (connection data)
  "Send DATA to the WebSocket process associated to CONNECTION."
  (unless (wsc-connection-open-p connection)
    (error "Connection not open, cannot send data"))
  (when wsc-debug
    (with-current-buffer (get-buffer-create "*wsc-dbg*")
      (goto-char (point-max))
      (insert (format "\n\nSENDING FRAME\n---\n%s\n---" data))))
  (let ((wsc--current-process (wsc-connection-process connection)))
    (wsc--send-frame 1 (encode-coding-string data 'no-conversion))))


(defun wsc--make-process (url)
  "Open a TCP connection to URL, and return the process object."
  (let* ((use-tls (string= (url-type url) "wss"))
	 (type (if use-tls 'tls 'plain))
	 (host (url-host url))
	 (port (if (zerop (url-port url))
		   (if use-tls 443 80)
		 (url-port url)))
	 (name "wsc process")
	 (buf (wsc--create-data-buffer)))
    (open-network-stream name buf host port :type type)))

(defun wsc--process-sentinel (process _)
  "Handle PROCESS closed."
  (when (member (process-status process) '(closed failed exit signal))
    (when-let ((connection (wsc--current-connection))
	       (handler (wsc-connection-on-close connection)))
      (funcall handler connection))))

(defun wsc--process-filter (process output)
  "Filter function for WebSocket network PROCESS.

OUTPUT is always appended to the PROCESS buffer."
  (when wsc-debug
    (with-current-buffer (get-buffer-create "*wsc-dbg*")
      (goto-char (point-max))
      (insert "\n\n" output)))
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert output)
    (let* ((wsc--current-process process)
	   (state (wsc-connection-state (wsc--current-connection))))
      (goto-char (point-min))
      (wsc--handle-output state))))

(defun wsc--handle-output (state)
  "Handle new output based on the STATE of a connection.

Each time a full message is received and processed (either a
complete frame or handshake response), its frames are removed
from the process buffer."
  (pcase state
    (:connecting (wsc--handle-handshake))
    (:open (wsc--handle-data))
    (:closed (error "WebSocket closed"))))

(defun wsc--handle-handshake ()
  "Validate the handshake server response.

Wait for more data and do nothing if the handshake response is
not complete.

Once the full handshake response has been outputed and validated,
erase it from the process buffer."
  (when (wsc--handshake-response-complete-p)
    (wsc--validate-handshake-status-code)
    (wsc--validate-handshake-accept-value)
    (setf (wsc-connection-state (wsc--current-connection)) :open)
    (erase-buffer)
    (when-let ((connection (wsc--current-connection))
	       (handler (wsc-connection-on-open connection)))
      (funcall handler (wsc--current-connection)))))

(defun wsc--handle-data ()
  "Handle incoming data in the process buffer.

Read data frame by frame as long as frames can be read from the
process buffer."
  (save-excursion
    (when-let ((headers (wsc--read-frame-headers)))
      (apply #'wsc--handle-frame headers)
      (wsc--handle-data))))

(defun wsc--handle-frame (final opcode length)
  "Handle a new frame output based on the value of OPCODE.
FINAL is non-nil if the message contains no more frame fragments.
LENGTH is the length of the payload in bytes.

List of OPCODEs based on the RFC:

  0	Continuation frame
  1	Text data frame
  2	Binary data frame
  3-7	Reserved (unused)
  8	Connection close
  9	Ping
  10	Pong
  11-15	Reserved (unused)

If the frame is processed, delete its data from the process
buffer."
  (let ((payload (wsc--read-frame-payload length)))
    (unless (null final)
      (delete-region (point-min) (point))
      (pcase opcode
	(0 (wsc--handle-continuation-frame payload))
	(1 (wsc--handle-text-frame payload))
	(8 (wsc--handle-close-frame))
	(9 (wsc--handle-ping-frame))
	(10 (wsc--handle-pong-frame))
	(_ (wsc--error-and-close
	    (format "Unsupported frame with opcode %s" opcode)))))))

(defun wsc--handle-close-frame ()
  "Handle a connnection close frame."
  (wsc--ensure-closed))

(defun wsc--handle-text-frame (payload)
  "Handle an unfragmented text data frame with PAYLOAD."
  (when-let ((handler (wsc-connection-on-message
		       (wsc--current-connection))))
    (funcall handler payload)))

(defun wsc--handle-continuation-frame (_payload)
  "Handle a final continuation frame."
  (when-let ((handler (wsc-connection-on-message
		       (wsc--current-connection))))
    (funcall handler (wsc--read-fragmented-frame-payloads))))

(defun wsc--handle-ping-frame ()
  "Handle a ping frame by sending a pong frame."
  (wsc--send-frame 10))

(defun wsc--handle-pong-frame ()
  "Handle a pong frame."
  ;; NO-OP
  )


;;; Handshake handling

(defun wsc--open-handshake (connection)
  "Open a handshake for CONNECTION."
  (let* ((process (wsc-connection-process connection))
	 (url (wsc-connection-url connection))
	 (absolute-path (if (string-empty-p (url-filename url))
			    "/"
			  (url-filename url)))
	 (host-and-port (if (url-port-if-non-default url)
			    (format "%s:%s" (url-host url) (url-port url))
			  (url-host url))))
    (process-send-string process (format wsc--handshake-template
					 absolute-path
					 host-and-port
					 (wsc-connection-key connection)
					 wsc-protocol-version))))

(defun wsc--close-handshake (_connection)
  "Send a closing frame for CONNECTION.")

(defun wsc--handshake-response-complete-p ()
  "Return non-nil if the current buffer has all handshake headers."
  (save-excursion
    (save-match-data
      (goto-char (point-max))
      (search-backward "\r\n\r\n" nil t))))

(defun wsc--validate-handshake-status-code ()
  "Check the status code in the server response.
If the status code is invalid, close the connection and signal an error."
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (unless (search-forward "HTTP/1.1 101" nil t)
	(wsc--error-and-close "Invalid status code from server")))))

(defun wsc--validate-handshake-accept-value ()
  "Check the accept value from the server based on the connection key.
If the accept value is invalid, close the connection and signal an error."
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (let ((accept (wsc--accept-value (wsc--current-connection))))
	(unless (search-forward (format "Sec-WebSocket-Accept: %s" accept) nil t)
	  (wsc--error-and-close "Invalid websocket accept key from server"))))))

;;; Sending/Reading frames

(defun wsc--send-data (data)
  "Send DATA as WebSocket frames to the current WebSocket process."
  (wsc--send-frame 1 data))

(defun wsc--send-frame (opcode &optional payload)
  "Send a frame with OPCODE and PAYLOAD."
  (let ((input-buffer (wsc--create-data-buffer))
	(mask-key (wsc--make-masking-key)))
    (unwind-protect
	(with-current-buffer input-buffer
	  (insert (logior #b10000000 ;; FIN bit
			  #b00000000 ;; RSV1 (ignored, always 0)
			  #b00000000 ;; RSV2 (ignored, always 0)
			  #b00000000 ;; RSV3 (ignored, always 0)
			  opcode	  ;; OPCODE 4bits
			  ))
	  (wsc--insert-payload-length (length payload))
	  (insert mask-key)
	  (apply #'insert (wsc--mask payload mask-key))
	  (process-send-region wsc--current-process (point-min) (point-max)))
      (kill-buffer input-buffer))))

(defun wsc--mask (payload key)
  "Return a list of bytes masking PAYLOAD using masking KEY."
  (seq-map-indexed (lambda (byte index)
		     (logxor byte (elt key (mod index 4))))
		   payload))

(defun wsc--insert-payload-length (length)
  "Insert the LENGTH of the payload in the current buffer.

The first bit is the masking bit.

The next 7 bits are the payload length if 0-125.

If 126, the following 2 bytes are interpreted as a 16-bit
unsigned integer are the payload length.

If 127, the following 8 bytes are interpreted as a 64-bit
unsigned integer."
  ;; Masking bit + 7 first bits of length
  (insert (logior #b10000000 ;; Masking bit
		  (cond ((< length 126) length)
                        ((< length 65536) 126)
                        (t 127))))
  (when (> length 125)
    (if (< length 65536)
	;; Extended length on 2 bytes
	(progn
	  (insert (logand (lsh length -8) #b11111111))
	  (insert (logand length #b11111111)))
      ;; Extended length on 8 bytes
      (progn
	(insert (logand (lsh length -56) #b11111111))
	(insert (logand (lsh length -48) #b11111111))
	(insert (logand (lsh length -40) #b11111111))
	(insert (logand (lsh length -32) #b11111111))
	(insert (logand (lsh length -24) #b11111111))
	(insert (logand (lsh length -16) #b11111111))
	(insert (logand (lsh length -8) #b11111111))
	(insert (logand length #b11111111))))))

(defun wsc--read-frame-headers ()
  "Return a list of headers for ther frame at point.

The returned list contains:

  - non-nil if the frame is final, nil otherwise;
  - the frame opcode;
  - the length of the frame payload.

The point is moved forward to the beginning of the frame payload.

If there is no data or if the frame is incomplete (because all
the frame data has not yet been received), return nil."
  (unless (eobp)
    (let* ((byte (char-after))
	   (final (not (zerop (logand #b10000000 byte))))
	   (opcode (logand #b00001111 byte))
	   (length (wsc--read-frame-payload-length)))
      ;; Check if the frame is complete
      (when (>= (point-max) (+ (point) length))
	(list final opcode length)))))

(defun wsc--read-frame-payload-length ()
  "Return the payload length from the frame in the current buffer.

The point is moved forward as the payload length is being read
from the buffer."
  ;; Skip from FIN bit up to opcode
  (forward-char)
  (let ((initial-length (logand #b01111111	;; Masking bit
				(char-after)))) ;; 7 bits of initial length
    (forward-char)
    (if (<= initial-length 125)
	initial-length
      (let ((nbytes (if (= initial-length 126) 2 8))
	    (length 0))
	(dotimes (i nbytes)
	  (cl-incf length (lsh (char-after) (* 8 (- nbytes i 1))))
	  (forward-char))
	length))))

(defun wsc--read-frame-payload (length)
  "Return the payload of LENGTH bytes of the frame at point.

The point is moved forward to the end of the frame payload."
  (let* ((start (point))
	 (end (+ start length)))
    (goto-char end)
    (decode-coding-region start end 'utf-8 t)))

(defun wsc--read-fragmented-frame-payloads ()
  "Not yet implemented."
  (error "Fragmented frames are not yet supported"))


;;; Utilities

(defun wsc--create-data-buffer ()
  "Return a new buffer to receive data."
  (let ((buf (generate-new-buffer " wsc ")))
    (with-current-buffer buf
      (set-buffer-multibyte nil))
    buf))

(defun wsc--make-key ()
  "Generate a base64-encoded 16 bytes long connection key."
  (base64-encode-string
   (wsc--generate-bytes 16)))

(defun wsc--make-masking-key ()
  "Generate a 4 bytes long masking key."
  (wsc--generate-bytes 4))

(defun wsc--generate-bytes (n)
  "Return a string of N bytes."
  (seq-into (seq-map (lambda (_)
		       (random 256))
		     (make-vector n 0))
	    'string))

(defun wsc--accept-value (connection)
  "Return the computed accept value for CONNECTION based on its key."
  (base64-encode-string
   (sha1 (format "%s%s" (wsc-connection-key connection) wsc-guid)
	 nil nil t)))

(defun wsc--error-and-close (reason)
  "Sinal an error with REASON and close the websocket process."
  (wsc--ensure-closed)
  (error reason))

(defun wsc--ensure-closed ()
  "Ensure the TCP process is closed and set the connection state."
  (setf (wsc-connection-state (wsc--current-connection)) :closed)
  (ignore-errors
    (kill-process wsc--current-process)))

(defun wsc--current-connection ()
  "Return the connection associated with the current WebSocket process."
  (when-let ((process wsc--current-process))
    (process-get process :wsc-connection)))

(provide 'wsc)
;;; wsc.el ends here
