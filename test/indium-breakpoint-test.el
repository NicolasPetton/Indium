;;; indium-breakpoint-test.el --- Test for indium-breakpoint.el  -*- lexical-binding: t; -*-

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
(require 'indium-test-helpers)
(require 'indium-breakpoint)

(ert-deftest indium-breakpoint-overlay-on-current-line-test ()
  (with-js2-buffer "let a = 1;"
    (let ((ov (make-overlay (point) (point))))
      (overlay-put ov 'indium-breakpoint t)
      (should (eq (indium-breakpoint-overlay-on-current-line)
                  ov)))))

(ert-deftest indium-breakpoint-on-current-line-p-test ()
  (with-js2-buffer "let a = 1;"
    (goto-char (point-min))
    (should-not (indium-breakpoint-on-current-line-p))
    (indium-breakpoint--put-icon)
    (should (indium-breakpoint-on-current-line-p))))

(ert-deftest indium-adding-breakpoints-multiple-times-not-duplicate-overlays-test ()
  (with-temp-buffer
    (indium-breakpoint--put-icon)
    (let ((number-of-overlays (seq-length (overlays-in (point-min) (point-max)))))
      (indium-breakpoint--put-icon)
      (should (equal number-of-overlays (seq-length (overlays-in (point-min) (point-max))))))))

(provide 'indium-breakpoint-test)
;;; indium-breakpoint-test.el ends here
