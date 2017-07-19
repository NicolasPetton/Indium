;;; indium-structs.el --- CL structs for Indium backends  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Nicolas Petton

;; Author: Nicolas Petton <nicolas@petton.fr>
;; Keywords:

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

;; This files defines all objects used in Indium as cl-structs.
;;
;; Backends should make instances of the structs defined in this file from data
;; they receive.
;;
;; `indium-script' represents a JavaScript file parsed by the runtime.  Scripts
;; are structs indexed by `id' in the current Indium connection.  A script contain
;; an `url' slot, and an optional `sourcemap-url' slot.
;;
;; `indium-location' represents a location (most often to a file).  A location
;; is a struct with a `line' and `column' slot.  If a location points to a local
;; file, it also contains a `file' slot.  Columns and lines start at 0.
;;
;; `indium-frame' represents a call frame in the context of debugging.

;;; Code:

(declare-function indium-script-get-file "indium-script.el")
(declare-function indium-script-find-by-id "indium-script.el")

(cl-defstruct indium-script
  (id nil :type string :read-only t)
  (url nil :type string :read-only t)
  (sourcemap-url nil :type string :read-only t))

(cl-defstruct
    (indium-location
     (:constructor make-indium-location-from-script-id
		   (&key (script-id "")
			 line
			 column
			 &aux (file (indium-script-get-file (indium-script-find-by-id script-id))))))
  (line 0 :type number :read-only t)
  (column 0 :type number :read-only t)
  (file nil :type string :read-only t))

(cl-defstruct indium-frame
  (id nil :type string :read-only t)
  ;; TODO: make a scope a struct as well.
  (scope-chain nil :type list :read-only t)
  (location nil :type indium-location :read-only t)
  (script nil :type indium-script :read-only t)
  (type nil :type string :read-only t)
  (function-name nil :type string))

(provide 'indium-structs)
;;; indium-structs.el ends here
