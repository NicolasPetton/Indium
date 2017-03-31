;;; jade-backend-test.el --- Test for jade-backend.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Nicolas Petton

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
(require 'jade-backend)
(require 'jade-webkit)
(require 'jade-test-helpers)

(ert-deftest jade-generic-connections-active ()
  "Generic connections should always be active."
  (let ((jade-connection '((backend . fake))))
    (should (jade-backend-active-connection-p))))

(ert-deftest jade-webkit-connections-active ()
  "Only Webkit connections with open websocket are active."
  (let ((jade-connection '((backend . webkit))))
    (should (not (jade-backend-active-connection-p 'webkit)))))

(provide 'jade-backend-test)
;;; jade-backend-test.el ends here
