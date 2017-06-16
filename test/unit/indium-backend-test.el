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

(require 'buttercup)

(require 'indium-backend)
(require 'indium-webkit)

(describe "Open/Closed connections"
  (it "should be an active connection if generic"
    (with-indium-connection '((backend . fake))
      (expect (indium-backend-active-connection-p 'fake) :to-be-truthy)))

  (it "should not be active unless a websocket is open"
    (with-indium-connection '((backend . webkit))
      (expect (indium-backend-active-connection-p 'webkit) :to-be nil))))

(describe "Backend breakpoints"
  (it "can register breakpoints"
    (with-indium-connection '((backend . fake))
      (indium-backend-register-breakpoint 'a 12 "foo.js" "cond")
      (expect (indium-backend-get-breakpoints) :to-equal
              '(((id . a)
                 (file . "foo.js")
                 (line . 12)
                 (condition . "cond"))))))

  (it "can get breakpoints in a file"
    (with-indium-connection '((backend . fake))
      (indium-backend-register-breakpoint 'a 12 "foo.js" "cond1")
      (indium-backend-register-breakpoint 'b 25 "foo.js" "cond2")
      (indium-backend-register-breakpoint 'c 3 "bar.js" "cond3")
      (expect (indium-backend-get-breakpoints-in-file "foo.js") :to-equal
              '(((id . a)
                 (file . "foo.js")
                 (line . 12)
                 (condition . "cond1"))
                ((id . b)
                 (file . "foo.js")
                 (line . 25)
                 (condition . "cond2"))))))

  (it "can unregister breakpoints"
    (with-indium-connection '((backend . fake))
      (indium-backend-register-breakpoint 'a 12 "foo.js" "cond")
      (indium-backend-unregister-breakpoint 'a)
      (expect (indium-backend-get-breakpoints) :to-be nil))))

(provide 'indium-backend-test)
;;; indium-backend-test.el ends here
