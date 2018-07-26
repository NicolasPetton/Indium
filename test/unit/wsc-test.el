;;; wsc-test.el --- Tests for wsc.el

;; Copyright (C) 2018 Nicolas Petton

;; Author: Nicolas Petton <nicolas@petton.fr>

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; Tests for wsc.el

;;; Code:

(require 'buttercup)
(require 'wsc)

(describe "WSC"

  (describe "Client key"
    (it "should generate a unique key"
      (dotimes (_ 100)
	(expect (wsc--make-key) :not :to-equal (wsc--make-key))))

    (it "has a different client key for each connection"
      (dotimes (_ 100)
	(expect (wsc-connection-key (wsc-connection-create)) :not :to-equal
		(wsc-connection-key (wsc-connection-create))))))

  (describe "Connection state"
    (it "should be in :connecting state by default"
      (expect (wsc-connection-state (wsc-connection-create)) :to-be :connecting)))

  (describe "Opening TCP connections"
    (it "should not use TLS protocol when over ws://"
      (let ((url "ws://localhost:8889"))
	(spy-on 'open-network-stream)
	(spy-on 'wsc--create-data-buffer :and-return-value 'buffer)
	(wsc--make-process (url-generic-parse-url url))
	(expect #'open-network-stream :to-have-been-called-with
		"wsc process"
		'buffer
		"localhost"
		8889
		:type 'plain)))

    (it "should use TLS when over  wss://"
      (let ((url "wss://localhost:8889"))
	(spy-on 'open-network-stream)
	(spy-on 'wsc--create-data-buffer :and-return-value 'buffer)
	(wsc--make-process (url-generic-parse-url url))
	(expect #'open-network-stream :to-have-been-called-with
		"wsc process"
		'buffer
		"localhost"
		8889
		:type 'tls))))

  (describe "Process buffer"
    (it "should not use multibyte"
      (with-current-buffer (wsc--create-data-buffer)
	(expect enable-multibyte-characters :to-be nil)))

    (it "should generate unique process buffer names"
      (let ((buf1 (wsc--create-data-buffer))
	    (buf2 (wsc--create-data-buffer)))
	(expect (buffer-name buf1) :not :to-equal (buffer-name buf2)))))

  (describe "Error handling"
    (it "should signal an error"
      (let ((conn (wsc-connection-create))
	    (wsc--current-process 'process))
	(spy-on 'process-get :and-return-value conn)
	(spy-on 'kill-process)
	(expect (wsc--error-and-close "Oops") :to-throw)))

    (it "should close the connection when signaling an error"
      (let ((conn (wsc-connection-create :url 'foobar))
	    (wsc--current-process 'process))
	(spy-on 'process-get :and-return-value conn)
	(spy-on 'kill-process)
	(expect (wsc-connection-state conn) :to-be :connecting)
	(ignore-errors
	  (wsc--error-and-close "Oops"))
	(expect (wsc-connection-state conn) :to-be :closed)))

    (it "should kill the process when signaling an error"
      (let ((conn (wsc-connection-create))
	    (wsc--current-process 'process))
	(spy-on 'process-get :and-return-value conn)
	(spy-on 'kill-process)
	(ignore-errors
	    (wsc--error-and-close "Oops"))
	(expect #'kill-process :to-have-been-called-with 'process))))

  (describe "Handshake"
    (it "should signal an error if the status code is not 101"
      (spy-on 'wsc--error-and-close)
      (with-temp-buffer
	(insert "HTTP/1.1 500

")
	(expect (wsc--handle-output :connecting) :to-throw)))

    (it "should signal an error when the accept value is wrong"
      (let ((conn (wsc-connection-create)))
	(spy-on 'wsc--current-connection :and-return-value conn)
	(with-temp-buffer
	  (insert "HTTP/1.1 101 Yay
Sec-WebSocket-Accept: foobar

")
	  (expect (wsc--handle-output :connecting) :to-throw))))

    (it "should succeed if the accept value is correct"
      (let ((conn (wsc-connection-create)))
	(spy-on 'wsc--current-connection :and-return-value conn)
	(with-temp-buffer
	  (insert (format "HTTP/1.1 101 Yay
Sec-WebSocket-Accept: %s

" (wsc--accept-value conn)))
	  (expect (wsc--handle-output :connecting) :not :to-throw)
	  (expect (wsc-connection-state conn) :to-be :open)))))

  (describe "Masking data"
    (it "should return a different string when masking"
      (expect (seq-into (wsc--mask "hello world" (wsc--make-masking-key)) 'string)
	      :not :to-equal "hello world"))

    (it "should not modify the data if the key is null"
      (expect (seq-into (wsc--mask "hello world" [0 0 0 0]) 'string)
	      :to-equal "hello world"))

    (it "should produce masked data of the same length"
      (expect (length (wsc--mask "hello world" (wsc--make-masking-key)))
	      :to-be (length "hello world")))

    (it "should get back the original data when unmasking"
      (let ((key (wsc--make-masking-key)))
	(expect (seq-into (wsc--mask (wsc--mask "hello world" key) key) 'string)
		:to-equal "hello world")))))

(provide 'wsc-test)
;;; wsc-test.el ends here
