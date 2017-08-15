;;; indium-breakpoint-test.el --- Test for indium-breakpoint.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2017  Nicolas Petton

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
(require 'assess)
(require 'seq)
(require 'indium-breakpoint)

(describe "Breakpoint position when editing buffers (GH issue #82)"
  (it "should keep the breakpoint on the original line when adding a line before"
    (with-js2-buffer "let a = true;"
      (let ((ov (indium-breakpoint--ensure-overlay)))
	;; add a line before the current line with the breakpoint overlay
	(goto-char (point-min))
	(open-line 1)
	(forward-line)
	(expect (overlay-start ov) :to-equal (point-at-bol))
	(expect (overlay-end ov) :to-equal (point-at-eol)))))

  (it "should not add a brk on the next line"
    ;; When inserting a line before, deleting a breakpoint resulted in an
    ;; overlay being added on the next line.
    (with-js2-buffer "let a = true;"
      (let ((ov (indium-breakpoint--ensure-overlay)))
	;; add a line before the current line with the breakpoint overlay
	(goto-char (point-min))
	(open-line 1)
	(forward-line)
	(indium-breakpoint-remove-overlay)
	(let ((overlays (seq-find (lambda (ov)
				    (overlay-get ov 'indium-breakpoint-ov))
				  (overlays-in (point-min) (point-max)))))
	  (expect overlays :to-equal nil))))))

(describe "Making markers (tests for issue #79)"
  (it "should put breakpoint overlays on the entire line"
    (with-js2-buffer "let a = true;"
      (let ((ov (indium-breakpoint--ensure-overlay)))
	(expect (overlay-start ov) :to-equal (point-at-bol))
	(expect (overlay-end ov) :to-equal (point-at-eol)))))

  (it "should be able to access breakpoints on empty lines"
    (with-js2-buffer ""
      (let ((ov (indium-breakpoint--ensure-overlay)))
	(expect (indium-breakpoint-overlay-on-current-line)
		:to-be ov))))

  (it "should be able to remove overlays on empty lines"
    (with-js2-buffer ""
      (indium-breakpoint--ensure-overlay)
      (expect (indium-breakpoint-overlay-on-current-line) :not :to-be nil)
      (indium-breakpoint-remove-overlay)
      (expect (indium-breakpoint-overlay-on-current-line) :to-be nil)))

  (it "can get a breakpoint overlays on the current line when it changed"
    (with-js2-buffer "let a = 1;"
      (let ((ov (indium-breakpoint--ensure-overlay)))
	(goto-char (point-min))
	(insert "\n")
	(forward-line 1)
        (expect (indium-breakpoint-overlay-on-current-line) :to-be ov)))))

(describe "Accessing breakpoints"
  (it "can get a breakpoint overlays on the current line"
    (with-js2-buffer "let a = 1;"
      (let ((ov (indium-breakpoint--ensure-overlay)))
        (overlay-put ov 'indium-breakpoint t)
        (expect (indium-breakpoint-overlay-on-current-line) :to-be ov))))

  (it "can put a breakpoint on the current line"
    (with-js2-buffer "let a = 1;"
      (goto-char (point-min))
      (expect (indium-breakpoint-on-current-line-p) :to-be nil)
      (indium-breakpoint-add (make-indium-location))
      (expect (indium-breakpoint-on-current-line-p) :to-be-truthy)))

  (it "can edit a breakpoint on the current line"
    (spy-on #'read-from-minibuffer :and-return-value "new condition")
    (spy-on #'indium-breakpoint-remove :and-call-through)
    (spy-on #'indium-breakpoint-add :and-call-through)
    (with-js2-buffer "let a = 1;"
      (goto-char (point-min))
      (let ((location (make-indium-location :file buffer-file-name
					    :line 0)))
	(indium-breakpoint-add location "old condition")
	(indium-breakpoint-edit-condition)
	(expect #'read-from-minibuffer :to-have-been-called)
	(expect #'indium-breakpoint-remove :to-have-been-called)
	(expect #'indium-breakpoint-add :to-have-been-called-with location "new condition")))))

(describe "Breakpoint duplication handling"
  (it "can add a breakpoint multiple times on the same line without duplicating it"
    (with-temp-buffer
      (indium-breakpoint-add (make-indium-location))
      (let ((number-of-overlays (seq-length (overlays-in (point-min) (point-max)))))
        (indium-breakpoint-add (make-indium-location))
        (expect (seq-length (overlays-in (point-min) (point-max))) :to-equal number-of-overlays)))))

(describe "Restoring breakpoints"
  (it "can restore breakpoints from buffers when opening a new connection"
    (spy-on 'indium-backend-add-breakpoint)
    (spy-on 'indium-breakpoint-add :and-call-through)
    (with-js2-buffer "let a = 2;"
      (goto-char (point-min))
      (let ((loc (make-indium-location))
	    (condition "condition"))
	(indium-breakpoint-add loc condition)
	;; simulate getting the breakpoint ID from a backend
	(setf (indium-breakpoint-id (indium-breakpoint-at-point)) 'id)
	(with-fake-indium-connection
	  (indium-breakpoint-restore-breakpoints)
	  (expect #'indium-backend-add-breakpoint :to-have-been-called)
	  (expect #'indium-breakpoint-add :to-have-been-called-with loc condition))))))

(provide 'indium-breakpoint-test)
;;; indium-breakpoint-test.el ends here
