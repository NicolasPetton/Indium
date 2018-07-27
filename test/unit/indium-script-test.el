;;; indium-script-test.el --- Unit tests for indium-script.el  -*- lexical-binding: t; -*-

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

;;; Code:

(require 'buttercup)
(require 'assess)
(require 'indium-script)
(require 'indium-workspace)

(defvar indium-script--test-fs
  '((".indium.json" "{\"configurations\": [{}]}")
    ("js" ("foo.js" "foo.js.map" "bar.js")))
  "Fake filesystem used in script tests.")

(describe "Looking up scripts"
  (it "should be able to retrieve parsed scripts url"
    (with-fake-indium-connection
      (indium-script-add-script-parsed "1" "foo")
      (expect (indium-script-url (indium-script-find-by-id "1")) :to-equal "foo")))

  (it "should be able to retrieve parsed scripts sourcemap url"
    (with-fake-indium-connection
      (indium-script-add-script-parsed "1" "foo" "foo-map")
      (expect (indium-script-sourcemap-url (indium-script-find-by-id "1")) :to-equal "foo-map")))

  (it "should be able to retrieve parsed scripts ids"
    (with-fake-indium-connection
      (indium-script-add-script-parsed "1" "foo")
      (expect (indium-script-id (indium-script-find-from-url "foo")) :to-equal "1")))

  (it "should be able to return all scripts with a sourcemap"
    (with-fake-indium-connection
      (indium-script-add-script-parsed "1" "foo")
      (indium-script-add-script-parsed "2" "bar" "bar.map")
      (expect (seq-map #'indium-script-id
		       (indium-script-all-scripts-with-sourcemap))
	      :to-equal '("2"))))

    (it "should be able to find scripts by location with file"
      (spy-on 'indium-script-find-from-file :and-return-value 'script)
      (spy-on 'indium-script-find-from-url)
      (let* ((location (indium-location-create :file "foo"))
	     (script (indium-script-find-from-location location)))
	(expect #'indium-script-find-from-file :to-have-been-called-with "foo")
	(expect #'indium-script-find-from-url :not :to-have-been-called)
	(expect script :to-be 'script)))

    (it "should be able to find scripts by location with url"
      (spy-on 'indium-script-find-from-file)
      (spy-on 'indium-script-find-from-url :and-return-value 'script)
      (let* ((location (indium-location-create :file "foo"))
	     (script (indium-script-find-from-location location)))
	(expect #'indium-script-find-from-url :to-have-been-called-with "foo")
	(expect script :to-be 'script)))

  (it "should be able to find the sourcemap file for a script"
    (assess-with-filesystem indium-script--test-fs
      (with-fake-indium-connection
	(indium-script-add-script-parsed "1" "http://localhost/js/foo.js" "foo.js.map")
	(let ((script (indium-script-find-by-id "1")))
	  (expect (indium-script--sourcemap-file script)
		  :to-equal (expand-file-name "js/foo.js.map"))))))

  (it "should be able to parse a sourcemap data url for a script (base64)"
    (let* ((sourcemap-json '((version . 3)
                             (file . "js/foo.js")
                             (sources . ["foo-1.js" "foo-2.js"])
                             (names . [])
                             (mappings . ";;;;;;kBAAe;AAAA,SAAM,QAAQ,GAAR,CAAY,aAAZ,CAAN;AAAA,C")))
           (sourcemap (indium-sourcemap--decode sourcemap-json)))
      (with-fake-indium-connection
	(indium-script-add-script-parsed
	 "1" "http://localhost/js/foo.js"
	 (concat "data:application/json;charset=utf-8;base64,"
		 (base64-encode-string (json-encode sourcemap-json))))
	(let ((script (indium-script-find-by-id "1")))
	  (expect (indium-script--sourcemap-from-data-url script)
		  :to-equal sourcemap)))))

  (it "should be able to parse a sourcemap data url for a script (url-encoded)"
    (let* ((sourcemap-json '((version . 3)
                             (file . "js/foo.js")
                             (sources . ["foo-1.js" "foo-2.js"])
                             (names . [])
                             (mappings . ";;;;;;kBAAe;AAAA,SAAM,QAAQ,GAAR,CAAY,aAAZ,CAAN;AAAA,C")))
           (sourcemap (indium-sourcemap--decode sourcemap-json)))
      (with-fake-indium-connection
	(indium-script-add-script-parsed
	 "1" "http://localhost/js/foo.js"
	 (concat "data:application/json;charset=utf-8,"
		 (url-hexify-string (json-encode sourcemap-json))))
	(let ((script (indium-script-find-by-id "1")))
	  (expect (indium-script--sourcemap-from-data-url script)
		  :to-equal sourcemap))))))

(describe "Adding scripts"
  (it "should not multiple scripts with the same url"
    (with-fake-indium-connection
      (indium-script-add-script-parsed "1" 'url)
      (indium-script-add-script-parsed "2" 'url)
      (expect (indium-script-id (indium-script-find-from-url 'url))
	      :to-equal "2"))))

(describe "Handling sourcemap files"
  (it "should convert all sourcemap entry paths to absolute paths"
    (spy-on 'indium-workspace-lookup-file :and-return-value "/foo/bar/script.js")
    (let* ((script (indium-script-create :url "/bar/script.js"))
	   (entry (make-indium-source-mapping :source "./baz.js"))
	   (map (make-indium-sourcemap :generated-mappings (make-vector 1 entry))))
      (assess-with-filesystem indium-script--test-fs
	(indium-script--transform-sourcemap-sources map script))
      (expect (indium-source-mapping-source entry)
	      :to-equal "/foo/bar/baz.js")))

  (it "should not convert sourcemap entries paths that are absolute"
    (spy-on 'indium-workspace-lookup-file :and-return-value "/foo/bar/script.js")
    (let* ((script (indium-script-create :url "/bar/script.js"))
	   (entry (make-indium-source-mapping :source "/baz.js"))
	   (map (make-indium-sourcemap :generated-mappings (make-vector 1 entry))))
      (assess-with-filesystem indium-script--test-fs
       (indium-script--transform-sourcemap-sources map script))
      (expect (indium-source-mapping-source entry)
	      :to-equal "/baz.js"))))

(describe "Handling sourcemap path overrides"
  (it "should expand root token in path overrides"
    (spy-on 'indium-workspace-root :and-return-value "/my/project")
    (expect (indium-script--expand-path-override "foo/bar")
	    :to-equal "foo/bar")
    (expect (indium-script--expand-path-override "${root}/foo/bar")
	    :to-equal "/my/project/foo/bar")
    (expect (indium-script--expand-path-override "${webRoot}/foo/bar")
	    :to-equal "/my/project/foo/bar"))

  (it "should apply default sourcemap path overrides"
    (assess-with-filesystem indium-script--test-fs

      (let* ((script (indium-script-create :url "/js/foo.js"))
      	     (entry (make-indium-source-mapping :source "webpack:///./js/foo.js"))
      	     (map (make-indium-sourcemap :generated-mappings (make-vector 1 entry))))
      	(indium-script--transform-sourcemap-sources map script)
      	(expect (indium-source-mapping-source entry)
      		:to-equal (expand-file-name "./js/foo.js")))

      (let* ((script (indium-script-create :url "/js/foo.js"))
      	     (entry (make-indium-source-mapping :source "webpack:///src/js/foo.js"))
      	     (map (make-indium-sourcemap :generated-mappings (make-vector 1 entry))))
      	(indium-script--transform-sourcemap-sources map script)
      	(expect (indium-source-mapping-source entry)
      		:to-equal (expand-file-name "./js/foo.js")))

      (let* ((script (indium-script-create :url "/node_modules/foo/index.js"))
      	     (entry (make-indium-source-mapping :source "webpack:///./~/foo/index.js"))
      	     (map (make-indium-sourcemap :generated-mappings (make-vector 1 entry))))
      	(indium-script--transform-sourcemap-sources map script)
      	(expect (indium-source-mapping-source entry)
      		:to-equal (expand-file-name "./node_modules/foo/index.js")))

      (let* ((script (indium-script-create :url "/foo.js"))
      	     (entry (make-indium-source-mapping :source "webpack:///foo.js"))
      	     (map (make-indium-sourcemap :generated-mappings (make-vector 1 entry))))
      	(indium-script--transform-sourcemap-sources map script)
      	(expect (indium-source-mapping-source entry)
      		:to-equal "/foo.js"))))

  (it "should convert root directories whenapplying sourcemap path overrides"
    (assess-with-filesystem indium-script--test-fs

      (let* ((indium-workspace-configuration '((root . "js")))
	     (script (indium-script-create :url "/js/foo.js"))
      	     (entry (make-indium-source-mapping :source "webpack:///./foo.js"))
      	     (map (make-indium-sourcemap :generated-mappings (make-vector 1 entry))))
      	(indium-script--transform-sourcemap-sources map script)
      	(expect (indium-source-mapping-source entry)
      		:to-equal (expand-file-name "./js/foo.js")))))

  (it "should apply custom sourcemap path overrides"
    (assess-with-filesystem indium-script--test-fs
      (let* ((indium-workspace-configuration
	      '((webRoot . "js")
		(sourceMapPathOverrides . (("foo://" . "${webRoot}/foo")))))
	     (script (indium-script-create :url "/js/foo/bar.js"))
      	     (entry (make-indium-source-mapping :source "foo:///bar.js"))
      	     (map (make-indium-sourcemap :generated-mappings (make-vector 1 entry))))
      	(indium-script--transform-sourcemap-sources map script)
      	(expect (indium-source-mapping-source entry)
      		:to-equal (expand-file-name "./js/foo/bar.js"))))))

(describe "Downloading sourcemap files"
  (it "should return nil when download is not possible"
    (should-not (indium-script--download "foo"))))

(provide 'indium-script-test)
;;; indium-script-test.el ends here
