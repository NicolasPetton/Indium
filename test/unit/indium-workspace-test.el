;;; indium-workspace-test.el --- Tests for indium-workspace.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Nicolas Petton

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
(require 'indium-workspace)

(defvar indium-workspace--test-fs
  '(".indium"
    ("js" ("app.js")))
  "Fake filesystem used in workspace tests.")

(describe "Reading and saving workspace directory list"
  (it "can add a workspace directory"
    (let ((indium-workspaces nil))
      (indium-workspace--add-directory "foobar")
      (expect indium-workspaces :to-equal '("foobar"))))

  (it "can save and read the workspace file"
    (with-temp-workspace-file
      (let ((indium-workspaces nil))
        (indium-workspace--add-directory "foobar")
        (indium-workspace--save-workspaces-file)
        (setq indium-workspaces nil)
        (indium-workspace--read-workspaces-file)
        (expect indium-workspaces :to-equal '("foobar")))))

  (it "should read the workspace file when prompting for a workspace"
    (let ((indium-workspace-use-workspace-file t))
      (spy-on 'indium-workspace--read-workspaces-file)
      (spy-on 'y-or-n-p)
      (spy-on 'completing-read)
      (indium-workspace-read)
      (expect #'indium-workspace--read-workspaces-file
              :to-have-been-called)))

  (it "should write the workspace file when prompting for a workspace"
    (let ((indium-workspace-use-workspace-file t))
      (spy-on 'indium-workspace--read-workspaces-file)
      (spy-on 'indium-workspace--save-workspaces-file)
      (spy-on 'indium-workspace-root :and-return-value 'foo)
      (spy-on 'y-or-n-p)
      (spy-on 'completing-read)
      (indium-workspace-read)
      (expect #'indium-workspace--save-workspaces-file
              :to-have-been-called)))

  (it "should not read or write the workspace file if `indium-workspace-use-workspace-file' is nil"
    (let ((indium-workspace-use-workspace-file nil))
      (spy-on 'indium-workspace--read-workspaces-file)
      (spy-on 'indium-workspace--save-workspaces-file)
      (spy-on 'indium-workspace-root :and-return-value 'foo)
      (spy-on 'y-or-n-p)
      (spy-on 'completing-read)
      (indium-workspace-read)
      (expect #'indium-workspace--save-workspaces-file
              :not :to-have-been-called)
      (expect #'indium-workspace--read-workspaces-file
              :not :to-have-been-called))))

(describe "Looking up files"
  (it "cannot lookup file when no workspace it set"
    (spy-on 'indium-workspace-root :and-return-value nil)
    (expect (indium-workspace-lookup-file "http://localhost:9229/foo/bar")
      :to-be nil))

  (it "can lookup file with .indium marker file"
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
  (it "cannot make a url when no workspace is set"
    (with-indium-connection '((url . "http://localhost:9229"))
      (expect (indium-workspace-make-url "js/app.js")
        :to-be nil)))

  (it "can make workspace urls"
    (with-indium-connection '((url . "http://localhost:9229"))
      (assess-with-filesystem indium-workspace--test-fs
        (expect (indium-workspace-make-url "js/app.js")
          :to-equal "http://localhost:9229/js/app.js"))))

  (it "should strip query strings from computing urls"
    (with-indium-connection '((url . "http://localhost:9229?foo=bar"))
      (assess-with-filesystem indium-workspace--test-fs
        (expect (indium-workspace-make-url "js/app.js")
          :to-equal "http://localhost:9229/js/app.js"))))

  (it "should strip paths based on the .indium marker when computing urls"
    (with-indium-connection  '((url . "http://localhost:9229/foo/bar"))
      (assess-with-filesystem indium-workspace--test-fs
        (expect (indium-workspace-make-url "js/app.js")
          :to-equal "http://localhost:9229/js/app.js")))))

(describe "File protocol"
  (it "can lookup files using the file:// protocol"
    (assess-with-filesystem indium-workspace--test-fs
      (with-indium-connection  '((url . "file:///foo/bar/index.html"))
        (let* ((file (expand-file-name "js/app.js"))
               (url (format "file://%s" file)))
          (expect (indium-workspace-lookup-file url)
            :to-equal file)))))

  (it "can make a url when using the file protocol"
    (assess-with-filesystem indium-workspace--test-fs
      (with-indium-connection  '((url . "file:///foo/bar/index.html"))
       (let* ((file (expand-file-name "js/app.js")))
         (expect (indium-workspace-make-url file)
           :to-equal (format "file://%s" file)))))))

(provide 'indium-workspace-test)
;;; indium-workspace-test.el ends here
