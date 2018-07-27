;;; indium-structs-test.el --- Unit tests for indium-structs.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2018  Nicolas Petton

;; Author: Nicolas Petton <nicolas@petton.fr>
;; Keywords: test

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

(require 'buttercup)
(require 'assess)
(require 'indium-structs)
(require 'cl-lib)

(describe "Setting current connection slots"
  (it "should be able to set the frames"
    (with-indium-connection (indium-connection-create)
      (setf (indium-current-connection-frames) 'foo)
      (expect (indium-current-connection-frames)
	      :to-be 'foo)))

  (it "should be able to set the current frame"
    (with-indium-connection (indium-connection-create)
      (setf (indium-current-connection-current-frame) 'foo)
      (expect (indium-current-connection-current-frame)
	      :to-be 'foo))))

(describe "Struct creation"
  (it "should be able to make locations from script ids"
    (spy-on 'indium-script-get-file :and-return-value "foo.js")
    (spy-on 'indium-script-find-by-id :and-return-value "id")
    (let ((loc (indium-location-from-script-id
		:script-id "id"
		:line 2
		:column 3)))
      (expect #'indium-script-find-by-id :to-have-been-called-with "id")
      (expect (indium-location-file loc) :to-equal "foo.js")))

  (it "Should be able to make breakpoints"
    (let ((brk (indium-breakpoint-create
		:id 'id
		:original-location (indium-location-create
				    :line 5
				    :column 2
				    :file "foo.js"))))
      (expect (indium-breakpoint-id brk) :to-be 'id)
      (expect (indium-location-file (indium-breakpoint-original-location brk))
	      :to-equal "foo.js")
      (expect (indium-location-line (indium-breakpoint-original-location brk))
	      :to-equal 5)
      (expect (indium-location-column (indium-breakpoint-original-location brk))
	      :to-equal 2))))

(provide 'indium-structs-test)
;;; indium-structs-test.el ends here
