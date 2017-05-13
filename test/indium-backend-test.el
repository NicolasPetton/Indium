;;; indium-backend-test.el --- Test for indium-backend.el  -*- lexical-binding: t; -*-

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

(require 'ert)
(require 'indium-backend)
(require 'indium-webkit)
(require 'indium-test-helpers)

(ert-deftest indium-generic-connections-active ()
  "Generic connections should always be active."
  (with-indium-connection '((backend . fake))
    (should (indium-backend-active-connection-p nil))))

(ert-deftest indium-webkit-connections-active ()
  "Only Webkit connections with open websocket are active."
  (with-indium-connection '((backend . webkit))
    (should (not (indium-backend-active-connection-p 'webkit)))))

;;
;; Backend breakpoints functions
;;

(ert-deftest indium-backend-register-breakpoint-test ()
  (with-indium-connection '((backend . fake))
    (indium-backend-register-breakpoint 'a 12 "foo.js")
    (should (equal (indium-backend-get-breakpoints)
                   '(((id . a)
                      (file . "foo.js")
                      (line . 12)))))))

(ert-deftest indium-backend-get-breakpoints-in-file-test ()
  (with-indium-connection '((backend . fake))
    (indium-backend-register-breakpoint 'a 12 "foo.js")
    (indium-backend-register-breakpoint 'b 25 "foo.js")
    (indium-backend-register-breakpoint 'c 3 "bar.js")
    (should (equal (indium-backend-get-breakpoints-in-file "foo.js")
                   '(((id . a)
                      (file . "foo.js")
                      (line . 12))
                     ((id . b)
                      (file . "foo.js")
                      (line . 25)))))))

(ert-deftest indium-backend-unregister-breakpoint-test ()
  (with-indium-connection '((backend . fake))
    (indium-backend-register-breakpoint 'a 12 "foo.js")
    (indium-backend-unregister-breakpoint 'a)
    (should (null (indium-backend-get-breakpoints)))))

(provide 'indium-backend-test)
;;; indium-backend-test.el ends here
