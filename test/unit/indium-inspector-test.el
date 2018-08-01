;;; indium-inspector-test.el --- Test for indium-inspector.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2018  Nicolas Petton

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
(require 'indium-inspector)
(require 'indium-structs)

(describe "Inspector should split properties to a better looking form"
  :var (native non-native)
  (before-all
    (setq native (indium-property-from-alist
		  '((name . "foo")
		    (value . ((description . "function f() { [native code] }"))))))
    (setq non-native (indium-property-from-alist
		      '((name . "foo")
			(value . ((description . "42")))))))

  (it "can split empty property list"
    (expect (indium-inspector--split-properties '())
            :to-equal '(nil nil)))

  (it "can split property list with only native property"
    (expect (indium-inspector--split-properties (list native))
            :to-equal `((,native) nil)))

  (it "can split property list with only non-native property"
    (expect (indium-inspector--split-properties (list non-native))
            :to-equal `(nil (,non-native))))

  (it "can split property list with both native and non-native properties"
    (expect (indium-inspector--split-properties (list native non-native))
            :to-equal `((,native) (,non-native)))))

(provide 'indium-inspector-test)
;;; indium-inspector-test.el ends here
