;;; indium-debugger-test.el --- Test for indium-debugger.el  -*- lexical-binding: t; -*-

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
(require 'indium-debugger)
(require 'indium-test-helpers)

(ert-deftest indium-debugger-setting-frames ()
  "Debugging frames are correctly set."
  (with-fake-indium-connection
    (let ((frames '(first second))
          (current-frame 'first))
      (indium-debugger-set-frames frames current-frame)
      (should (eq (indium-debugger-frames) frames))
      (should (eq (indium-debugger-current-frame) current-frame)))))

(ert-deftest indium-debugger-unset-frames ()
  "Can unset the debugging frames."
  (with-fake-indium-connection
    (indium-debugger-set-frames '(first second) 'first)
    (indium-debugger-unset-frames)
    (should (null (indium-debugger-frames)))
    (should (null (indium-debugger-current-frame)))))

(provide 'indium-debugger-test)
;;; indium-debugger-test.el ends here
