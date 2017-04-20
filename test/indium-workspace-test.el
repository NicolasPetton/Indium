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

(require 'ert)
(require 'el-mock)
(require 'assess)
(require 'indium-workspace)

(defvar indium-workspace--test-fs
  '(".indium"
    ("js" ("app.js")))
  "Fake filesystem used in workspace tests.")

(ert-deftest indium-workspace-lookup-file-with-no-workspace-test ()
  (with-mock (mock (indium-workspace-root) => nil)
             (should (null (indium-workspace-lookup-file "http://localhost:9229/foo/bar")))))

(ert-deftest indium-workspace-lookup-file-that-exists-test ()
  (assess-with-filesystem indium-workspace--test-fs
      (should (equal (expand-file-name "js/app.js")
                     (indium-workspace-lookup-file "http://localhost:9229/js/app.js")))))

(ert-deftest indium-workspace-lookup-file-ignore-query-string-test ()
  (assess-with-filesystem indium-workspace--test-fs
      (should (equal (expand-file-name "js/app.js")
                  (indium-workspace-lookup-file "http://localhost:9229/js/app.js?foo=bar")))))

(ert-deftest indium-workspace-lookup-file-that-does-not-exist-test ()
  (assess-with-filesystem indium-workspace--test-fs
    (should (null (indium-workspace-lookup-file "http://localhost:9229/non-existant-file-name.js")))))

(ert-deftest indium-workspace-lookup-file-safe-fallback-test ()
  (assess-with-filesystem indium-workspace--test-fs
    (let ((url "http://localhost:9229/non-existant-file-name.js"))
      (should (equal url (indium-workspace-lookup-file-safe url))))))

(ert-deftest indium-workspace-lookup-file-safe-test ()
  (assess-with-filesystem indium-workspace--test-fs
    (let ((url "http://localhost:9229/js/app.js")
          (file (expand-file-name "js/app.js")))
      (should (equal file (indium-workspace-lookup-file-safe url))))))

(ert-deftest indium-workspace-make-url-with-no-workspace-test ()
  (let ((indium-connection '((url . "http://localhost:9229"))))
    (should (null (indium-workspace-make-url "js/app.js")))))

(ert-deftest indium-workspace-make-url-test ()
  (let ((indium-connection '((url . "http://localhost:9229"))))
    (assess-with-filesystem indium-workspace--test-fs
      (should (equal (indium-workspace-make-url "js/app.js")
                     "http://localhost:9229/js/app.js")))))

(ert-deftest indium-workspace-make-url-strips-query-string-test ()
  (let ((indium-connection '((url . "http://localhost:9229?foo=bar"))))
    (assess-with-filesystem indium-workspace--test-fs
      (should (equal (indium-workspace-make-url "js/app.js")
                    "http://localhost:9229/js/app.js")))))

(ert-deftest indium-workspace-make-strips-connection-path-test ()
  (let ((indium-connection '((url . "http://localhost:9229/foo/bar"))))
    (assess-with-filesystem indium-workspace--test-fs
     (should (equal (indium-workspace-make-url "js/app.js")
                    "http://localhost:9229/js/app.js")))))

(ert-deftest indium-workspace-lookup-file-protocol-test ()
  (assess-with-filesystem indium-workspace--test-fs
    (let* ((indium-connection '((url . "file:///foo/bar/index.html")))
          (file (expand-file-name "js/app.js"))
          (url (format "file://%s" file)))
     (should (equal (indium-workspace-lookup-file url)
                    file)))))

(ert-deftest indium-workspace-make-url-file-protocol-test ()
  (assess-with-filesystem indium-workspace--test-fs
    (let* ((indium-connection '((url . "file:///foo/bar/index.html")))
           (file (expand-file-name "js/app.js")))
      (should (equal (indium-workspace-make-url file) (format "file://%s" file))))))

(provide 'indium-workspace-test)
;;; indium-workspace-test.el ends here
