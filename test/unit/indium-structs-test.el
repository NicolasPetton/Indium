;;; indium-structs-test.el --- Unit tests for indium-structs.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Nicolas Petton

;; Author: Nicolas Petton <nicolas@petton.fr>
;; Keywords: test

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

;;

;;; Code:

(require 'buttercup)
(require 'indium-structs)

(describe "Setting current connection slots"
  (it "should be able to set the frames"
    (with-indium-connection (make-indium-connection)
      (setf (indium-current-connection-frames) 'foo)
      (expect (indium-current-connection-frames)
	      :to-be 'foo)))

  (it "should be able to set the current frame"
    (with-indium-connection (make-indium-connection)
      (setf (indium-current-connection-current-frame) 'foo)
      (expect (indium-current-connection-current-frame)
	      :to-be 'foo))))

(provide 'indium-structs-test)
;;; indium-structs-test.el ends here
