;;; indium-client-test.el --- Test for indium-client.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2018  Nicolas Petton

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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'buttercup)
(require 'indium-client)

(describe "Regression test for GitHub issue #163"
  (it "should signal a user error when the indium executable cannot be found"
    (let ((indium-client-executable ""))
      (expect (indium-client-start (lambda ())) :to-throw
	      'user-error '("Cannot find the indium executable.  Please run \"npm install -g indium\"")))))

(describe "Reading server messages"
  (it "should not change the buffer reading an incomplete message"
    (with-temp-buffer
      (insert "{foo")
      (indium-client--handle-data (current-buffer))
      (expect (buffer-string) :to-equal "{foo")))

  (it "should call `indium-client--handle-message' when reading a complete message"
    (spy-on #'indium-client--handle-message)
    (with-temp-buffer
      (insert "{\"foo\": 1}\n")
      (indium-client--handle-data (current-buffer))
      (expect #'indium-client--handle-message :to-have-been-called-with '((foo . 1)))))

  (it "should remove the linefeed char when reading a complete message"
    (spy-on #'indium-client--handle-message)
    (with-temp-buffer
      (insert "{\"foo\": 1}\n")
      (indium-client--handle-data (current-buffer))
      (expect (buffer-string) :to-equal "")))

  (it "should remove all linefeed chars when reading multiple complete messages"
    (spy-on #'indium-client--handle-message)
    (with-temp-buffer
      (insert "{\"foo\": 1}\n{\"bar\": 2}\n")
      (indium-client--handle-data (current-buffer))
      (expect (buffer-string) :to-equal "")))

  (it "should remove all linefeed chars but keep incomplete messages"
    (spy-on #'indium-client--handle-message)
    (with-temp-buffer
      (insert "{\"foo\": 1}\n{\"bar\": 2}\n{\"baz")
      (indium-client--handle-data (current-buffer))
      (expect (buffer-string) :to-equal "{\"baz"))))

(provide 'indium-client-test)
;;; indium-client-test.el ends here
