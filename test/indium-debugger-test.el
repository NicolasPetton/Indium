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
(require 'ert-expectations)
(require 'el-mock)

(require 'indium-debugger)

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

(ert-deftest indium-debugger-seting-current-frame ()
  "Can set the current frame."
  (with-fake-indium-connection
    (with-mock
      ;; We're just interested in the value of the current frame, ignore buffer
      ;; setups.
      (mock (indium-debugger-get-buffer-create))
      (indium-debugger-set-current-frame 'current)
      (should (eq (indium-debugger-current-frame) 'current)))))

(ert-deftest indium-debugger-select-next-frame-error ()
  "Selecting the next frame when there is no next frame should error."
  (with-fake-indium-connection
    (let ((frames '(first second))
          (current-frame 'first))
      (indium-debugger-set-frames frames current-frame)
      (should-error (indium-debugger-next-frame) :type 'user-error))))

(ert-deftest indium-debugger-select-previous-frame-error ()
  "Selecting the previous frame when there is no previous frame
should error."
  (with-fake-indium-connection
    (let ((frames '(first second))
          (current-frame 'second))
      (indium-debugger-set-frames frames current-frame)
      (should-error (indium-debugger-previous-frame) :type 'user-error))))

(expectations
  (desc "Can select the next frame")
  (expect (mock (indium-debugger-select-frame 'first))
    (with-fake-indium-connection
      (let ((frames '(first second))
            (current-frame 'second))
        (indium-debugger-set-frames frames current-frame)
        (indium-debugger-next-frame)))))

(expectations
  (desc "Can select the previous frame")
  (expect (mock (indium-debugger-select-frame 'second))
    (with-fake-indium-connection
      (let ((frames '(first second))
            (current-frame 'first))
        (indium-debugger-set-frames frames current-frame)
        (indium-debugger-previous-frame)))))

(provide 'indium-debugger-test)
;;; indium-debugger-test.el ends here
