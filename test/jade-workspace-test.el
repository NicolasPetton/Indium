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
(require 'jade-workspace)

(ert-deftest jade-workspace-lookup-file-with-no-workspace-test ()
  (let ((jade-workspace-directories nil))
    (should (null (jade-workspace-lookup-file "http://localhost:9229/foo/bar")))))

(ert-deftest jade-workspace-lookup-file-that-exists-test ()
  (let ((jade-workspace-directories (list (expand-file-name "."))))
    (should (equal (expand-file-name "jade-workspace-test.el")
                   (jade-workspace-lookup-file "http://localhost:9229/jade-workspace-test.el")))))

(ert-deftest jade-workspace-lookup-file-ignore-query-string-test ()
    (let ((jade-workspace-directories (list (expand-file-name "."))))
    (should (equal (expand-file-name "jade-workspace-test.el")
                   (jade-workspace-lookup-file "http://localhost:9229/jade-workspace-test.el?foo=bar")))))

(ert-deftest jade-workspace-lookup-file-that-does-not-exist-test ()
    (let ((jade-workspace-directories (list (expand-file-name "."))))
    (should (null (jade-workspace-lookup-file "http://localhost:9229/non-existant-file-name.js")))))

(provide 'jade-workspace-test)
;;; jade-workspace-test.el ends here
