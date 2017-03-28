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
(require 'jade-workspace)

(ert-deftest jade-workspace-lookup-file-with-no-workspace-test ()
  (with-mock (mock (jade-workspace-root) => nil)
             (should (null (jade-workspace-lookup-file "http://localhost:9229/foo/bar")))))

(ert-deftest jade-workspace-lookup-file-that-exists-test ()
  (with-mock (mock (jade-workspace-root) => default-directory)
             (should (equal (expand-file-name "jade-workspace-test.el")
                            (jade-workspace-lookup-file "http://localhost:9229/jade-workspace-test.el")))))

(ert-deftest jade-workspace-lookup-file-ignore-query-string-test ()
  (with-mock (mock (jade-workspace-root) => default-directory)
             (should (equal (expand-file-name "jade-workspace-test.el")
                            (jade-workspace-lookup-file "http://localhost:9229/jade-workspace-test.el?foo=bar")))))

(ert-deftest jade-workspace-lookup-file-that-does-not-exist-test ()
  (with-mock (mock (jade-workspace-root) => default-directory)
             (should (null (jade-workspace-lookup-file "http://localhost:9229/non-existant-file-name.js")))))

(ert-deftest jade-workspace-make-url-with-no-workspace-test ()
  (with-mock (mock (jade-workspace-root) => nil)
             (let ((connection '((url . "http://localhost:9229"))))
               (should (null (jade-workspace-make-url "./jade-workspace-test.el" connection))))))

(ert-deftest jade-workspace-make-url-test ()
  (with-mock (mock (jade-workspace-root) => default-directory)
             (let ((connection '((url . "http://localhost:9229"))))
               (should (equal (jade-workspace-make-url "./jade-workspace-test.el" connection)
                              "http://localhost:9229/jade-workspace-test.el")))))

(ert-deftest jade-workspace-make-url-strips-query-string-test ()
  (with-mock (mock (jade-workspace-root) => default-directory)
             (let ((connection '((url . "http://localhost:9229?foo=bar"))))
               (should (equal (jade-workspace-make-url "./jade-workspace-test.el" connection)
                              "http://localhost:9229/jade-workspace-test.el")))))

(ert-deftest jade-workspace-make-strips-connection-path-test ()
  (with-mock (mock (jade-workspace-root) => default-directory)
             (let ((connection '((url . "http://localhost:9229/foo/bar"))))
               (should (equal (jade-workspace-make-url "./jade-workspace-test.el" connection)
                              "http://localhost:9229/jade-workspace-test.el")))))

(provide 'jade-workspace-test)
;;; jade-workspace-test.el ends here
