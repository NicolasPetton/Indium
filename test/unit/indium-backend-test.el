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
    (with-indium-connection (make-indium-connection :backend 'webkit)
      (expect (indium-backend-active-connection-p 'webkit) :to-be nil))))

(provide 'indium-backend-test)
;;; indium-backend-test.el ends here
