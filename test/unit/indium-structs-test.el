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
(require 'indium-structs)
(require 'assess)
(require 'seq)

(describe "Locations"
  (it "Should be able to make locations at point"
    (assess-with-filesystem '(("index.js" "let foo = 1;\nlet bar = 2;"))
      (with-current-buffer (find-file-noselect "index.js")
	(goto-char (point-max))
	(let ((loc (indium-location-at-point)))
	  (expect (indium-location-file loc) :to-equal (expand-file-name "index.js"))
	  (expect (indium-location-line loc) :to-equal 2)))))

  (it "Should be able to make locations from alists"
    (let ((loc (indium-location-from-alist '((file . "index.js")
					     (line . 22)
					     (column  . 0)))))
      (expect (indium-location-file loc) :to-equal "index.js")
      (expect (indium-location-line loc) :to-equal 22)
      (expect (indium-location-column loc) :to-equal 0))))

(describe "Breakpoints"
  (it "Should be able to make breakpoints"
    (let ((brk (indium-breakpoint-create
		:id 'id
		:condition "foo === bar")))
      (expect (indium-breakpoint-id brk) :to-be 'id)
      (expect (indium-breakpoint-condition brk)
	      :to-equal "foo === bar")
      (expect (indium-breakpoint-location brk)
	      :to-be nil)))

  (it "Should be have the location of its overlay"
    (with-temp-buffer
      (insert "let foo = 1;\nlet bar = 2;")
      (goto-char (point-min))
      (let* ((ov (make-overlay (point) (point)))
	     (brk (indium-breakpoint-create :overlay ov)))
	(expect (indium-location-line (indium-breakpoint-location brk))
		:to-equal 1)
	(expect (indium-location-file (indium-breakpoint-location brk))
		:to-equal (buffer-file-name (overlay-buffer ov))))))

  (it "Should follow the location of the overlay when it changes"
    (with-temp-buffer
      (insert "let foo = 1;\nlet bar = 2;")
      (goto-char (point-min))
      (let* ((ov (make-overlay (point) (point)))
  	     (brk (indium-breakpoint-create :overlay ov)))
  	(move-overlay ov (point-at-eol) (point-at-eol))
  	(save-excursion
  	  (goto-char (point-max))
  	  (expect (indium-location-line (indium-breakpoint-location brk))
  		  :to-equal 2))))))

(describe "Scopes"
  (it "Should be able to make scopes from alists"
    (let ((s (indium-scope-from-alist '((type . "closure")
					(name . "this.foo")
					(id . "25")))))
      (expect (indium-scope-id s) :to-equal "25")
      (expect (indium-scope-type s) :to-equal "closure")
      (expect (indium-scope-name s) :to-equal "this.foo"))))

(describe "Frames"
  (it "Should be able to make frames from alists"
    (let ((f (indium-frame-from-alist '((id . "some id")
					(scriptId . "22")
                                        (functionName . "foo")
					(location . ((file . "index.js")
						     (line . 22)
						     (column  . 0)))
					(scopeChain . [((type . "closure")
							(name . "this.foo")
							(id . "25"))
						       ((type . "local")
							(name . "bar")
							(id . "26"))])))))
      (expect (indium-frame-id f) :to-equal "some id")
      (expect (indium-frame-script-id f) :to-equal "22")
      (expect (indium-frame-function-name f) :to-equal "foo")
      (expect (length (indium-frame-scope-chain f)) :to-equal 2)
      (expect (indium-scope-type (seq-elt (indium-frame-scope-chain f) 0))
	      :to-equal "closure")
      (expect (indium-scope-name (seq-elt (indium-frame-scope-chain f) 0))
	      :to-equal "this.foo")
      (expect (indium-scope-id (seq-elt (indium-frame-scope-chain f) 0))
	      :to-equal "25")
      (expect (indium-scope-type (seq-elt (indium-frame-scope-chain f) 1))
	      :to-equal "local")
      (expect (indium-scope-name (seq-elt (indium-frame-scope-chain f) 1))
	      :to-equal "bar")
      (expect (indium-scope-id (seq-elt (indium-frame-scope-chain f) 1))
	      :to-equal "26")
      (expect (indium-location-file (indium-frame-location f))
	      :to-equal "index.js")
      (expect (indium-location-line (indium-frame-location f))
	      :to-equal 22)
      (expect (indium-location-column (indium-frame-location f))
	      :to-equal 0))))

(describe "Native properties"
  :var (native non-native)
  (before-all
    (setq native (indium-property-from-alist
		  '((name . "foo")
		    (value . ((description . "function f() { [native code] }"))))))
    (setq non-native (indium-property-from-alist
		      '((name . "foo")
			(value . ((description . "42")))))))

  (it "can detect native code property"
    (expect (indium-property-native-p native)
            :to-be-truthy))

  (it "can detect non-native code property"
    (expect (indium-property-native-p non-native)
            :to-be nil)))

(provide 'indium-structs-test)
;;; indium-structs-test.el ends here
