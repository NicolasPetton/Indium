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
    (with-indium-connection (make-indium-connection)
      (setf (indium-current-connection-frames) 'foo)
      (expect (indium-current-connection-frames)
	      :to-be 'foo)))

  (it "should be able to set the current frame"
    (with-indium-connection (make-indium-connection)
      (setf (indium-current-connection-current-frame) 'foo)
      (expect (indium-current-connection-current-frame)
	      :to-be 'foo))))

(describe "Struct creation"
  (it "should be able to make locations from script ids"
    (spy-on 'indium-script-get-file :and-return-value "foo.js")
    (spy-on 'indium-script-find-by-id :and-return-value "id")
    (let ((loc (make-indium-location-from-script-id
		:script-id "id"
		:line 2
		:column 3)))
      (expect #'indium-script-find-by-id :to-have-been-called-with "id")
      (expect (indium-location-file loc) :to-equal "foo.js")))

  (it "Should be able to make breakpoints"
    (let ((brk (make-indium-breakpoint
		:id 'id
		:line 5
		:column 2
		:file "foo.js")))
      (expect (indium-breakpoint-id brk) :to-be 'id)
      (expect (indium-location-file (indium-breakpoint-location brk))
	      :to-equal "foo.js")
      (expect (indium-location-line (indium-breakpoint-location brk))
	      :to-equal 5)
      (expect (indium-location-column (indium-breakpoint-location brk))
	      :to-equal 2))))

(describe "Manipulating breakpoints"
  (it "can register breakpoints"
    (with-indium-connection (make-indium-connection)
      (expect (indium-current-connection-get-breakpoint 'a) :to-be nil)
      (let ((brk (make-indium-breakpoint :id 'a :line 12 :file "foo.js" :condition "cond")))
	(indium-current-connection-add-breakpoint brk)
	(expect (indium-current-connection-get-breakpoint 'a) :to-be brk))))

  (it "can get breakpoints in a file"
    (with-indium-connection (make-indium-connection)
      (assess-with-filesystem '("foo.js" "bar.js")
	(let* ((bufs (seq-map #'find-file-noselect (seq-map #'expand-file-name '("foo.js" "bar.js"))))
	       (brks (seq-map (lambda (buf)
				(with-current-buffer buf
				  (make-indium-breakpoint :id (symbol-name (cl-gensym))
							  :overlay (make-overlay (point) (point)))))
			      bufs)))
	  (seq-map #'indium-current-connection-add-breakpoint brks)
	  (message "%s" bufs)
	  (message "%s" (indium-current-connection-breakpoints))
	  (expect (indium-current-connection-get-breakpoints-in-file (expand-file-name "foo.js"))
		  :to-equal (list (car brks)))))))

  (it "can get breakpoints in a file with line"
    (with-indium-connection (make-indium-connection)
      (assess-with-filesystem '("foo.js")
	(let ((buf (find-file-noselect (expand-file-name "foo.js")))
	      (brks (list (make-indium-breakpoint :id 'a :file "foo.js" :line 5)
			  (make-indium-breakpoint :id 'a :file "foo.js" :line 6))))
	  (seq-do (lambda (brk)
		    (with-current-buffer buf
		      (goto-char (point-min))
		      (forward-line (1- (indium-location-line (indium-breakpoint-location brk))))
		      (indium-breakpoint--add-overlay brk)
		      (indium-current-connection-add-breakpoint brk)))
		  brks)
	  (expect (indium-current-connection-get-breakpoints-in-file (expand-file-name "foo.js") 6)
		  :to-equal (list (cadr brks)))))))

  (it "can get breakpoint from ID"
    (with-indium-connection (make-indium-connection)
      (let ((brk (make-indium-breakpoint :id 'a :line 12 :file "foo.js" :condition "cond")))
	(indium-current-connection-add-breakpoint brk)
	(expect (indium-current-connection-get-breakpoint 'a) :to-be brk))))

  (it "can know if a breakpoint is resolved"
    (expect (indium-breakpoint-unresolved-p (make-indium-breakpoint)) :to-be-truthy)
    (expect (indium-breakpoint-unresolved-p (make-indium-breakpoint :id 'fake)) :not :to-be-truthy))

  (it "gets nil when no breakpoint found for ID"
    (with-indium-connection (make-indium-connection)
      (let ((brk (make-indium-breakpoint :id 'a :line 12 :file "foo.js" :condition "cond")))
	(indium-current-connection-add-breakpoint brk)
	(expect (indium-current-connection-get-breakpoint 'b) :to-equal nil))))

  (it "can unregister breakpoints"
    (with-indium-connection (make-indium-connection)
      (let ((brk (make-indium-breakpoint :id 'a :line 12 :file "foo.js" :condition "cond")))
	(indium-current-connection-add-breakpoint brk)
	(indium-current-connection-remove-breakpoint 'a)
	(expect (map-values (indium-current-connection-breakpoints)) :to-be nil)))))

(provide 'indium-structs-test)
;;; indium-structs-test.el ends here
