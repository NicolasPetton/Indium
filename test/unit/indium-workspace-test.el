;;; indium-workspace-test.el --- Tests for indium-workspace.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2018  Nicolas Petton

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

(require 'map)

(require 'buttercup)
(require 'assess)
(require 'indium-workspace)

(defvar indium-workspace--test-fs
  '((".indium.json" "{\"configurations\": [{}]}")
    ("js" ("app.js")))
  "Fake filesystem used in workspace tests.")

(describe "Workspace root"
  (after-each
    (setq indium-workspace-configuration nil))

  (it "Returns the current connection's project root when there is a connection"
    (assess-with-filesystem indium-workspace--test-fs
      (let* ((root (expand-file-name "js"))
	     (indium-current-connection (indium-connection-create :project-root root)))
	(expect (indium-workspace-root) :to-be root))))

  (it "should default to the project directory when no \"root\" is defined"
    (assess-with-filesystem indium-workspace--test-fs
      (expect (directory-file-name (indium-workspace-root)) :to-equal
	      (directory-file-name default-directory))))

  (it "should take the directory set in the \"root\" option"
    (assess-with-filesystem '((".indium.json" "{\"configurations\": [{\"root\": \"foo\"}]}")
			      ("foo" ("index.js")))
      (with-indium-workspace-configuration
	(expect (directory-file-name (indium-workspace-root)) :to-equal
		(directory-file-name (expand-file-name "foo" default-directory))))))

  (it "webRoot should be an alias for root"
    (assess-with-filesystem '((".indium.json" "{\"configurations\": [{\"webRoot\": \"foo\"}]}")
			      ("foo" ("index.js")))
      (with-indium-workspace-configuration
	(expect (directory-file-name (indium-workspace-root)) :to-equal
		(directory-file-name (expand-file-name "foo" default-directory)))))))

(describe "Invalid root directory"
  (after-each
    (setq indium-workspace-configuration nil))

    (it "should signal an error when the root directory does not exist"
    (assess-with-filesystem '((".indium.json" "{\"configurations\": [{\"webRoot\": \"foo\"}]}"))
      (with-indium-workspace-configuration
	(expect (indium-workspace-root) :to-throw)))))

(describe "Choosing a configuration"
  (after-each
    (setq indium-workspace-configuration nil))

    (it "should not prompt for a configuration when there is only one"
      (assess-with-filesystem '((".indium.json" "{\"configurations\": [{}]}"))
	(spy-on #'completing-read)
	(indium-workspace-read-configuration)
	(expect #'completing-read :not :to-have-been-called)))

  (it "should prompt for a configuration when there are many"
    (assess-with-filesystem '((".indium.json" "{\"configurations\": [{\"name\": \"a\"}, {\"name\": \"b\"}]}"))
      (spy-on #'completing-read)
      (indium-workspace-read-configuration)
      (expect #'completing-read :to-have-been-called-with "Choose a configuration: " '("a" "b") nil t))))

(describe "Looking up files"
  (it "cannot lookup file when no workspace it set"
    (expect (indium-workspace-lookup-file "http://localhost:9229/foo/bar")
	    :to-throw))

  (it "can lookup file with an empty .indium.json marker file"
    (assess-with-filesystem indium-workspace--test-fs
      (expect (indium-workspace-lookup-file "http://localhost:9229/js/app.js")
        :to-equal (expand-file-name "js/app.js"))))

  (it "should ignore query strings from urls when looking up files"
    (assess-with-filesystem indium-workspace--test-fs
      (expect (indium-workspace-lookup-file "http://localhost:9229/js/app.js?foo=bar")
        :to-equal (expand-file-name "js/app.js"))))

  (it "cannot find a file that does not exist"
    (assess-with-filesystem indium-workspace--test-fs
      (expect (indium-workspace-lookup-file "http://localhost:9229/non-existant-file-name.js")
        :to-be nil))))

(describe "Looking up files safely"
  (it "should fallback to the url when no file can be found"
    (assess-with-filesystem indium-workspace--test-fs
      (let ((url "http://localhost:9229/non-existant-file-name.js"))
        (expect (indium-workspace-lookup-file-safe url)
          :to-equal url))))

  (it "can lookup files that exist"
    (assess-with-filesystem indium-workspace--test-fs
      (let ((url "http://localhost:9229/js/app.js")
            (file (expand-file-name "js/app.js")))
        (expect (indium-workspace-lookup-file-safe url)
          :to-equal file)))))

(describe "Making workspace urls from file names"
  (after-each
    (setq indium-workspace-configuration nil))

  (it "cannot make a url when no workspace is set"
    (with-indium-connection (indium-connection-create :url "http://localhost:9229")
      (expect (indium-workspace-make-url "js/app.js")
              :to-throw)))

  (it "can make workspace urls"
    (with-indium-connection (indium-connection-create :url "http://localhost:9229")
      (assess-with-filesystem indium-workspace--test-fs
        (expect (indium-workspace-make-url "js/app.js")
          :to-equal "http://localhost:9229/js/app.js"))))

  (it "should strip query strings from computing urls"
    (with-indium-connection (indium-connection-create :url "http://localhost:9229?foo=bar")
      (assess-with-filesystem indium-workspace--test-fs
        (expect (indium-workspace-make-url "js/app.js")
          :to-equal "http://localhost:9229/js/app.js"))))

  (it "should strip paths based on the .indium marker when computing urls"
    (with-indium-connection (indium-connection-create :url "http://localhost:9229/foo/bar")
      (assess-with-filesystem indium-workspace--test-fs
        (expect (indium-workspace-make-url "js/app.js")
		:to-equal "http://localhost:9229/js/app.js"))))

  (it "should use the file path if the connection uses nodejs when computing urls"
    (with-indium-connection (indium-connection-create)
      (map-put (indium-current-connection-props)
	       'nodejs t)
      (assess-with-filesystem indium-workspace--test-fs
	(let ((file (expand-file-name "js/app.js")))
	  (expect (indium-workspace-make-url file)
		  :to-equal (expand-file-name "js/app.js"))))))

  ;; Regression test for GitHub issue #144
  (it "should use Windows file paths file path with nodejs on Windows"
    (with-indium-connection (indium-connection-create)
      (map-put (indium-current-connection-props)
	       'nodejs t)
      (spy-on #'convert-standard-filename :and-call-through)
      (assess-with-filesystem indium-workspace--test-fs
	(let ((file (expand-file-name "js/app.js")))
	  (indium-workspace-make-url file)
	  (expect #'convert-standard-filename :to-have-been-called-with file))))))

(describe "File protocol"
  (after-each
    (setq indium-workspace-configuration nil))

  (it "can lookup files using the file:// protocol"
    (assess-with-filesystem indium-workspace--test-fs
      (with-indium-connection (indium-connection-create :url "file:///foo/bar/index.html")
        (let* ((file (expand-file-name "js/app.js"))
               (url (format "file://%s" file)))
          (expect (indium-workspace-lookup-file url)
            :to-equal file)))))

  (it "can make a url when using the file protocol"
    (assess-with-filesystem indium-workspace--test-fs
      (with-indium-connection (indium-connection-create :url "file:///foo/bar/index.html")
       (let* ((file (expand-file-name "js/app.js")))
         (expect (indium-workspace-make-url file)
           :to-equal (format "file://%s" file)))))))

(provide 'indium-workspace-test)
;;; indium-workspace-test.el ends here
