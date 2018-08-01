;;; indium-breakpoint-test.el --- Test for indium-breakpoint.el  -*- lexical-binding: t; -*-

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
(require 'assess)
(require 'seq)
(require 'map)
(require 'indium-breakpoint)
(require 'indium-client)

(describe "Breakpoint position when editing buffers (GH issue #82)"
  (it "should keep the breakpoint on the original line when adding a line before"
    (with-js2-buffer "let a = true;"
      (let* ((brk (indium-breakpoint-create))
	     (ov (indium-breakpoint--add-overlay brk)))
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
      (let* ((brk (indium-breakpoint-create))
	     (ov (indium-breakpoint--add-overlay brk)))
	;; add a line before the current line with the breakpoint overlay
	(goto-char (point-min))
	(open-line 1)
	(forward-line)
	(indium-breakpoint--remove-overlay)
	(let ((overlays (seq-find (lambda (ov)
				    (overlay-get ov 'indium-breakpoint-ov))
				  (overlays-in (point-min) (point-max)))))
	  (expect overlays :to-equal nil)))))

  (it "should not add a brk on the next line when the line is split "
    ;; When inserting a line before, deleting a breakpoint resulted in an
    ;; overlay being added on the next line when the line where the breakpoint
    ;; was added had been split.
    (with-js2-buffer "let a = true;"
      (let* ((brk (indium-breakpoint-create))
	     (ov (indium-breakpoint--add-overlay brk)))
	;; add a line before the current line with the breakpoint overlay
	(goto-char 5)
	(newline)
	(forward-line -1)
	(indium-breakpoint--remove-overlay)
	(let ((overlays (seq-find (lambda (ov)
				    (overlay-get ov 'indium-breakpoint-ov))
				  (overlays-in (point-min) (point-max)))))
	  (expect overlays :to-equal nil))))))

(describe "Making markers (tests for issue #79)"
  (it "should put breakpoint overlays on the entire line"
    (with-js2-buffer "let a = true;"
      (let* ((brk (indium-breakpoint-create))
	     (ov (indium-breakpoint--add-overlay brk)))
	(expect (overlay-start ov) :to-equal (point-at-bol))
	(expect (overlay-end ov) :to-equal (point-at-eol)))))

  (it "should be able to access breakpoints on empty lines"
    (with-js2-buffer ""
      (let* ((brk (indium-breakpoint-create))
	     (ov (indium-breakpoint--add-overlay brk)))
	(expect (indium-breakpoint--overlay-on-current-line)
		:to-be ov))))

  (it "should be able to remove overlays on empty lines"
    (with-js2-buffer ""
      (let ((brk (indium-breakpoint-create)))
	(indium-breakpoint--add-overlay brk))
      (expect (indium-breakpoint--overlay-on-current-line) :not :to-be nil)
      (indium-breakpoint--remove-overlay)
      (expect (indium-breakpoint--overlay-on-current-line) :to-be nil)))

  (it "can get a breakpoint overlays on the current line when it changed"
    (with-js2-buffer "let a = 1;"
      (let* ((brk (indium-breakpoint-create))
	     (ov (indium-breakpoint--add-overlay brk)))
	(goto-char (point-min))
	(insert "\n")
	(forward-line 1)
        (expect (indium-breakpoint--overlay-on-current-line) :to-be ov)))))

(describe "Accessing breakpoints"
  (it "can get a breakpoint overlay on the current line"
    (with-js2-file
      (let* ((brk (indium-breakpoint-create))
	     (ov (indium-breakpoint--add-overlay brk)))
        (expect (indium-breakpoint--overlay-on-current-line) :to-be ov))))

  (it "can put a breakpoint on the current line"
    (with-js2-file
      (spy-on #'indium-client-add-breakpoint)
      (goto-char (point-min))
      (expect (indium-breakpoint-on-current-line-p) :to-be nil)
      (indium-breakpoint-add)
      (expect (indium-breakpoint-on-current-line-p) :to-be-truthy)))

  (it "can edit a breakpoint on the current line"
    (spy-on #'read-from-minibuffer :and-return-value "new condition")
    (spy-on #'indium-breakpoint-remove :and-call-through)
    (spy-on #'indium-breakpoint-add :and-call-through)
    (spy-on #'indium-client-add-breakpoint)
    (with-js2-file
      (goto-char (point-min))
      (indium-breakpoint-add "old condition")
      (indium-breakpoint-edit-condition)
      (expect #'read-from-minibuffer :to-have-been-called)
      (expect #'indium-breakpoint-remove :to-have-been-called)
      (expect #'indium-breakpoint-add :to-have-been-called-with "new condition"))))

(describe "Breakpoint duplication handling"
  (it "can add a breakpoint multiple times on the same line without duplicating it"
    (spy-on #'indium-client-add-breakpoint)
    (with-js2-file
      (indium-breakpoint-add)
      (let ((number-of-overlays (seq-length (overlays-in (point-min) (point-max)))))
        (indium-breakpoint-add)
        (expect (seq-length (overlays-in (point-min) (point-max))) :to-equal number-of-overlays)))))

(describe "Handling overlays in breakpoints"
  (it "adding overlays should store the overlay in the breakpoint"
    (with-js2-buffer "let a = 2;"
      (let* ((brk (indium-breakpoint-create))
	     (ov (indium-breakpoint--add-overlay brk)))
	(expect (indium-breakpoint-overlay brk) :to-be ov))))

  (it "removing overlays should remove them from breakpoints"
    (with-js2-buffer "let a = 2;"
      (let* ((brk (indium-breakpoint-create)))
	(indium-breakpoint--add-overlay brk)
	(indium-breakpoint--remove-overlay)
	(expect (indium-breakpoint-overlay brk) :to-be nil))))

  (it "should access breakpoint buffers"
    (with-js2-buffer "let a = 2;"
      (let* ((brk (indium-breakpoint-create)))
	(indium-breakpoint--add-overlay brk)
	(expect (indium-breakpoint-buffer brk) :to-be (current-buffer))
	(indium-breakpoint--remove-overlay)
	(expect (indium-breakpoint-buffer brk) :to-be nil)))))

(describe "Keeping track of local breakpoints in buffers"
  (it "should track breakpoints when added"
    (spy-on #'indium-client-add-breakpoint)
    (with-js2-file
      (let ((indium-breakpoint--local-breakpoints (make-hash-table :weakness t)))
	(indium-breakpoint-add)
	(expect (seq-length (map-keys indium-breakpoint--local-breakpoints)) :to-be 1)
	(expect (car (map-values indium-breakpoint--local-breakpoints)) :to-be (current-buffer)))))

  (it "should untrack breakpoints when removed"
    (spy-on #'indium-client-add-breakpoint)
    (with-js2-file
      (let ((indium-breakpoint--local-breakpoints (make-hash-table :weakness t)))
	(indium-breakpoint-add)
	(indium-breakpoint-remove)
	(expect (seq-length (map-keys indium-breakpoint--local-breakpoints)) :to-be 0))))

  (it "should untrack breakpoints when killing a buffer"
    (spy-on #'indium-client-add-breakpoint)
    (with-js2-file
      (let ((indium-breakpoint--local-breakpoints (make-hash-table :weakness t)))
	(indium-breakpoint-add)
	(kill-buffer)
	(expect (seq-length (map-keys indium-breakpoint--local-breakpoints)) :to-be 0))))

  (it "can get breakpoints by id"
    (with-indium-test-fs
      (let ((indium-breakpoint--local-breakpoints (make-hash-table))
	    (brk (indium-breakpoint-create :id 'foo)))
	(map-put indium-breakpoint--local-breakpoints
		 brk
		 'bar)
	(expect (indium-breakpoint-breakpoint-with-id 'foo) :to-be brk)))))

(describe "Breakpoint resolution"
  (it "should be able to unresolve breakpoints"
    (spy-on #'indium-client-add-breakpoint)
    (with-js2-file
      (let ((indium-breakpoint--local-breakpoints (make-hash-table)))
	(indium-breakpoint-add)
	(let ((brk (indium-breakpoint-at-point)))
	  ;; Fake the registration of the breakpoint
	  (setf (indium-breakpoint-resolved (indium-breakpoint-at-point)) t)
	  (indium-breakpoint--unregister-all-breakpoints)
	  (expect (indium-breakpoint-resolved brk) :to-be nil))))))

(provide 'indium-breakpoint-test)
;;; indium-breakpoint-test.el ends here
