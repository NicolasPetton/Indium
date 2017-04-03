;;; jade-workspace-test.el --- Tests for jade-workspace.el  -*- lexical-binding: t; -*-

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
(require 'jade-workspace)

(defvar jade-workspace--test-fs
  '(".jade"
    ("js" ("app.js")))
  "Fake filesystem used in workspace tests.")

(ert-deftest jade-workspace-lookup-file-with-no-workspace-test ()
  (with-mock (mock (jade-workspace-root) => nil)
             (should (null (jade-workspace-lookup-file "http://localhost:9229/foo/bar")))))

(ert-deftest jade-workspace-lookup-file-that-exists-test ()
  (assess-with-filesystem jade-workspace--test-fs
      (should (equal (expand-file-name "js/app.js")
                     (jade-workspace-lookup-file "http://localhost:9229/js/app.js")))))

(ert-deftest jade-workspace-lookup-file-ignore-query-string-test ()
  (assess-with-filesystem jade-workspace--test-fs
      (should (equal (expand-file-name "js/app.js")
                  (jade-workspace-lookup-file "http://localhost:9229/js/app.js?foo=bar")))))

(ert-deftest jade-workspace-lookup-file-that-does-not-exist-test ()
  (assess-with-filesystem jade-workspace--test-fs
    (should (null (jade-workspace-lookup-file "http://localhost:9229/non-existant-file-name.js")))))

(ert-deftest jade-workspace-make-url-with-no-workspace-test ()
  (let ((jade-connection '((url . "http://localhost:9229"))))
    (should (null (jade-workspace-make-url "js/app.js")))))

(ert-deftest jade-workspace-make-url-test ()
  (let ((jade-connection '((url . "http://localhost:9229"))))
    (assess-with-filesystem jade-workspace--test-fs
      (should (equal (jade-workspace-make-url "js/app.js")
                     "http://localhost:9229/js/app.js")))))

(ert-deftest jade-workspace-make-url-strips-query-string-test ()
  (let ((jade-connection '((url . "http://localhost:9229?foo=bar"))))
    (assess-with-filesystem jade-workspace--test-fs
      (should (equal (jade-workspace-make-url "js/app.js")
                    "http://localhost:9229/js/app.js")))))

(ert-deftest jade-workspace-make-strips-connection-path-test ()
  (let ((jade-connection '((url . "http://localhost:9229/foo/bar"))))
    (assess-with-filesystem jade-workspace--test-fs
     (should (equal (jade-workspace-make-url "js/app.js")
                    "http://localhost:9229/js/app.js")))))

(ert-deftest jade-workspace-lookup-file-protocol-test ()
  (assess-with-filesystem jade-workspace--test-fs
    (let* ((jade-connection '((url . "file:///foo/bar/index.html")))
          (file (expand-file-name "js/app.js"))
          (url (format "file://%s" file)))
     (should (equal (jade-workspace-lookup-file url)
                    file)))))

(ert-deftest jade-workspace-make-url-file-protocol-test ()
  (assess-with-filesystem jade-workspace--test-fs
    (let* ((jade-connection '((url . "file:///foo/bar/index.html")))
           (file (expand-file-name "js/app.js")))
      (should (equal (jade-workspace-make-url file) (format "file://%s" file))))))

(provide 'jade-workspace-test)
;;; jade-workspace-test.el ends here
