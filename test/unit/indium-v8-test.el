;;; indium-v8-test.el --- Tests for indium-v8.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2018  Nicolas Petton

;; Author: Nicolas Petton <nicolas@petton.fr>
;; Keywords:

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

;; Tests for indium-v8.el

;;; Code:

(require 'buttercup)
(require 'indium-v8)

(describe "Generating request ids"
  (it "should increment request ids"
    (let ((indium-v8--request-id 0))
      (expect (indium-v8--next-request-id) :to-be 1)
      (expect (indium-v8--next-request-id) :to-be 2)
      (expect (indium-v8--next-request-id) :to-be 3)
      (expect (indium-v8--next-request-id) :to-be 4))))

(describe "V8 connection websocket"
  (it "should be able to set a websocket"
    (let ((conn (indium-connection-create)))
      (setf (indium-connection-ws conn) 'foo)
      (expect (indium-connection-ws conn)
	      :to-be 'foo))))

(describe "V8 connection handling"
  (it "should be active if the websocket is open"
    (spy-on 'websocket-openp :and-return-value t)
    (with-fake-indium-connection
      (expect (indium-backend-active-connection-p 'v8) :to-be-truthy)))

  (it "should be inactive if the websocket is closed"
    (let ((indium-current-connection (indium-connection-create :backend 'v8)))
     (expect (indium-backend-active-connection-p 'v8) :to-be nil)))

  (it "should close the socket when closing the connection"
    (spy-on 'websocket-close)
    (with-indium-connection (indium-connection-create :backend 'v8)
      (map-put (indium-current-connection-props) 'ws 'ws)
      (indium-backend-close-connection 'v8)
      (expect #'websocket-close :to-have-been-called-with 'ws))))

(describe "Sending requests"
  (it "should not send requests if the connection is closed"
    (spy-on 'websocket-send-test)
    (spy-on 'message)
    (with-fake-indium-connection
      (indium-v8--send-request 'foo)
      (expect #'websocket-send-test :not :to-have-been-called)))

  (it "should display a warning message if the connection is closed"
    (spy-on 'message)
    (with-fake-indium-connection
      (indium-v8--send-request 'foo)
      (expect #'message :to-have-been-called-with "Socket connection closed")))

  (it "should send requests if the connection is active"
    (spy-on 'websocket-send-text)
    (spy-on 'indium-backend-active-connection-p :and-return-value t)
    (spy-on 'indium-v8--next-request-id :and-return-value 'id)
    (with-indium-connection (indium-connection-create :backend 'v8)
      (map-put (indium-current-connection-props) 'ws 'ws)
      (indium-v8--send-request '((message . "message")))
      (expect #'websocket-send-text :to-have-been-called-with
              'ws (json-encode '((id . id) (message . "message"))))))

  (it "should register callbacks when sending requests"
    (spy-on 'websocket-send-text)
    (spy-on 'indium-backend-active-connection-p :and-return-value t)
    (spy-on 'indium-v8--next-request-id :and-return-value 'id)
    (with-indium-connection (indium-connection-create :backend 'v8)
      (indium-v8--send-request '((message . "message")) 'callback)
      (expect (map-elt (indium-current-connection-callbacks) 'id) :to-equal 'callback))))

(describe "Receiving responses"
  (it "should evaluate `indium-script-parsed-hook' when a script gets parsed"
    (spy-on 'indium-script-add-script-parsed :and-return-value 'script)
    ;; Don't actually update breakpoints
    (spy-on 'indium-breakpoint--update-after-script-parsed)
    (spy-on 'test-hook-run)
    (add-hook 'indium-script-parsed-hook 'test-hook-run)
    (with-indium-connection (indium-connection-create :backend 'v8)
      (indium-v8--handle-script-parsed '(params))
      (remove-hook 'indium-script-parsed-hook 'test-hook-run)
      (expect #'test-hook-run :to-have-been-called-with 'script))))

(describe "Making completion expressions"
  (it "should return \"this\" if there is no property to complete"
    (expect (indium-v8--completion-expression "foo")
      :to-equal "this"))

  (it "should return the parent property"
    (expect (indium-v8--completion-expression "foo.bar")
      :to-equal "foo")
    (expect (indium-v8--completion-expression "foo.bar.baz")
      :to-equal "foo.bar")))

(describe "Evaluating code"
  (it "calls Runtime.evaluate with the expression to evaluate"
    (spy-on 'indium-v8--send-request)
    (with-fake-indium-connection
      (indium-backend-evaluate 'v8 "foo")
      (expect #'indium-v8--send-request :to-have-been-called-with
              '((method . "Runtime.evaluate")
                (params . ((expression . "foo")
                           (callFrameId . nil)
                           (generatePreview . t))))
              nil)))

  (it "calls Debugger.evaluateOnCallFrame when there is stack frame"
    (spy-on 'indium-v8--send-request)
    (with-indium-connection (indium-connection-create
			     :current-frame (indium-frame-create :id 1))
      (indium-backend-evaluate 'v8 "foo")
      (expect #'indium-v8--send-request :to-have-been-called-with
              '((method . "Debugger.evaluateOnCallFrame")
                (params . ((expression . "foo")
                           (callFrameId . 1)
                           (generatePreview . t))))
              nil))))

(describe "V8 backend result description string"
  (it "can render boolean descriptions formatted as string values (GitHub issue #52)"
    (expect (indium-v8--description '((type . "boolean") (value . t)))
            :to-equal "true")
    (expect (indium-v8--description '((type . "boolean") (value . :json-false)))
            :to-equal "false")))

(describe "V8 backend object preview"
  (it "can render array previews with booleans (GitHub issue #52)"
    (expect (indium-v8--preview '((type . "object")
                                      (subtype . "array")
                                      (className . "Array")
                                      (description . "Array[1]")
                                      (objectId . "{\"injectedScriptId\":12,\"id\":38}")
                                      (preview (type . "object")
                                               (subtype . "array")
                                               (description . "Array[1]")
                                               (overflow . :json-false)
                                               (properties . [((name . "0") (type . "boolean") (value . "true"))]))))
            :to-equal "[ true ]"))

  (it "can render object previews with booleans (GitHub issue #52)"
    (expect (indium-v8--preview '((type . "object")
                                      (className . "Object")
                                      (description . "Object")
                                      (objectId . "{\"injectedScriptId\":12,\"id\":43}")
                                      (preview (type . "object")
                                               (description . "Object")
                                               (overflow . :json-false)
                                               (properties . [((name . "a") (type . "boolean") (value . "true"))]))))
            :to-equal "{ a: true }")))

(describe "Location conversion"
  (it "can convert a location struct into a v8 location"
    (let ((location (indium-location-create :line 10 :column 5 :file "/foo/bar.js")))
      (spy-on #'indium-script-find-from-location :and-return-value nil)
      (expect (indium-v8--convert-to-v8-location location)
	      :to-equal '((columnNumber . 5)
			  (lineNumber . 10)))))

  (it "can convert a location struct with file into a v8 location"
    (with-fake-indium-connection
      (let ((location (indium-location-create :line 10 :column 5 :file "foo")))
	(spy-on #'indium-location-url :and-return-value "foo")
	(spy-on #'indium-script-find-from-location :and-return-value (indium-script-create :id "1"))
	(expect (indium-v8--convert-to-v8-location location)
		:to-equal '((scriptId . "1")
			    (columnNumber . 5)
			    (lineNumber . 10)))))))

(provide 'indium-v8-test)
;;; indium-v8-test.el ends here
