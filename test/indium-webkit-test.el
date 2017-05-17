;;; indium-webkit-test.el --- Tests for indum-webkit.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Nicolas Petton

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

;; Tests for indium-webkit.el

;;; Code:

(require 'buttercup)
(require 'indium-webkit)

(describe "Generating request ids"
  (it "should increment request ids"
    (let ((indium-webkit--request-id 0))
      (expect (indium-webkit--next-request-id) :to-be 1)
      (expect (indium-webkit--next-request-id) :to-be 2)
      (expect (indium-webkit--next-request-id) :to-be 3)
      (expect (indium-webkit--next-request-id) :to-be 4))))

(describe "Webkit connection handling"
  (it "should be active if the websocket is open"
    (spy-on 'websocket-openp :and-return-value t)
    (with-fake-indium-connection
      (expect (indium-backend-active-connection-p 'webkit) :to-be-truthy)))

  (it "should be inactive if the websocket is closed"
    (expect (indium-backend-active-connection-p 'webkit) :to-be nil))

  (it "closing the connection should close the socket"
    (spy-on 'websocket-close)
    (with-indium-connection '((backend . webkit)
                              (ws . ws))
      (indium-backend-close-connection 'webkit)
      (expect #'websocket-close :to-have-been-called-with 'ws))))

(describe "Sending requests"
  (it "should not send requests if the connection is closed"
    (spy-on 'websocket-send-test)
    (spy-on 'message)
    (with-fake-indium-connection
      (indium-webkit--send-request 'foo)
      (expect #'websocket-send-test :not :to-have-been-called)))

  (it "should display a warning message if the connection is closed"
    (spy-on 'message)
    (with-fake-indium-connection
      (indium-webkit--send-request 'foo)
      (expect #'message :to-have-been-called-with "Socket connection closed")))

  (it "should send requests if the connection is active"
    (spy-on 'websocket-send-text)
    (spy-on 'indium-backend-active-connection-p :and-return-value t)
    (spy-on 'indium-webkit--next-request-id :and-return-value 'id)
    (with-indium-connection '((backend . webkit)
                              (ws . ws))
      (indium-webkit--send-request '((message ."message")))
      (expect #'websocket-send-text :to-have-been-called-with
              'ws (json-encode '((id . id) (message . "message"))))))

  (it "should register callbacks when sending requests"
    (spy-on 'websocket-send-text)
    (spy-on 'indium-backend-active-connection-p :and-return-value t)
    (spy-on 'indium-webkit--next-request-id :and-return-value 'id)
    (with-indium-connection `((backend . webkit)
                              (callbacks . ,(make-hash-table)))
      (indium-webkit--send-request '((message . "message")) 'callback)
      (expect (map-elt (indium-webkit--callbacks) 'id) :to-equal 'callback))))

(describe "Making completion expressions"
  (it "should return \"this\" if there is no property to complete"
    (expect (indium-webkit--completion-expression "foo")
      :to-equal "this"))

  (it "should return the parent property"
    (expect (indium-webkit--completion-expression "foo.bar")
      :to-equal "foo")
    (expect (indium-webkit--completion-expression "foo.bar.baz")
      :to-equal "foo.bar")))

(describe "Evaluating code"
  (it "calls Runtime.evaluate with the expression to evaluate"
    (spy-on 'indium-webkit--send-request)
    (with-fake-indium-connection
      (indium-backend-evaluate 'webkit "foo")
      (expect #'indium-webkit--send-request :to-have-been-called-with
              '((method . "Runtime.evaluate")
                (params . ((expression . "foo")
                           (callFrameId . nil)
                           (generatePreview . t))))
              nil)))

  (it "calls Debugger.evaluateOnCallFrame when there is stack frame"
    (spy-on 'indium-webkit--send-request)
    (with-indium-connection '((current-frame . ((callFrameId . 1))))
      (indium-backend-evaluate 'webkit "foo")
      (expect #'indium-webkit--send-request :to-have-been-called-with
              '((method . "Debugger.evaluateOnCallFrame")
                (params . ((expression . "foo")
                           (callFrameId . 1)
                           (generatePreview . t))))
              nil))))


(describe "Webkit backend result description string"
  (it "can render booleans (GitHub issue #52)"
    (expect (indium-webkit--description '((type . "boolean") (value . "true")))
      :to-equal "true")
    (expect (indium-webkit--description '((type . "boolean") (value . "false")))
      :to-equal "false")))

(provide 'indium-webkit-test)
;;; indium-webkit-test.el ends here
