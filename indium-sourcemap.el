;;; indium-sourcemap.el --- Indium helpers for source map decoding

;; Copyright (C) 2012 Julian Scheid
;; Copyright (C) 2017-2018 Nicolas Petton

;; Author: Julian Scheid <julians37@gmail.com>, Nicolas Petton <nicolas@petton.fr>
;; Keywords: tools
;; Package: indium

;; This file is not part of GNU Emacs.

;; Indium is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; Indium is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Indium.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file was initially written for kite <https://github.com/jscheid/kite> by
;; Julian Scheid.
;;
;; This package providers helper functions for decoding source maps
;; and looking up mappings in them.
;;
;; It is mostly a transliteration of Mozilla's code found at
;; https://github.com/mozilla/source-map/
;;
;; See also:
;; * http://www.html5rocks.com/en/tutorials/developertools/sourcemaps/
;; * https://github.com/mozilla/source-map/
;; * https://docs.google.com/document/d/1U1RGAehQwRypUTovF1KRlpiOFze0b-_2gc6fAH0KY0k/edit?pli=1#


;;; Code:

(require 'map)
(require 'seq)
(require 'json)
(require 'cl-lib)
(require 'subr-x)

(defconst indium-sourcemap--vlq-base-shift 5)

(defconst indium-sourcemap--vlq-base (lsh 1 indium-sourcemap--vlq-base-shift))

(defconst indium-sourcemap--vlq-base-mask (- indium-sourcemap--vlq-base 1))

(defconst indium-sourcemap--vlq-continuation-bit indium-sourcemap--vlq-base)

(defconst indium--supported-sourcemap-version 3)

(defconst indium--base64-char-to-int-map
  (let* ((index 0)
         (base64-chars "\
ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
         (map (make-hash-table :size 64)))
    (dolist (char
             (string-to-list base64-chars))
      (puthash char index map)
      (cl-incf index))
    map))

(cl-defstruct (indium-source-mapping)
  "Holds the parsed mapping coordinates from the source map's
  `mappings' attribute."
  generated-line
  generated-column
  source
  original-line
  original-column
  name)

(cl-defstruct (indium-sourcemap)
  "Representation of a parsed source map suitable for fast
lookup."
  names
  sources
  generated-mappings)

(defun indium-sourcemap-from-file (file)
  "Return a sourcemap from FILE."
  (indium-sourcemap--decode (json-read-file file)))

(defun indium-sourcemap-from-string (string)
  "Return a sourcemap from STRING."
  (indium-sourcemap--decode (json-read-from-string string)))

(defun indium-sourcemap-original-position-for (sourcemap line column)
  "Given SOURCEMAP, find the original position for LINE and COLUMN.
SOURCEMAP should be an `indium-sourcemap' struct.

Return a plist with `:source', `:line', `:column', and `:name',
or nil if not found."
  (when-let ((match (indium-sourcemap--binary-search sourcemap line column)))
    (list :source (indium-source-mapping-source match)
	  :line (indium-source-mapping-original-line match)
	  :column (indium-source-mapping-original-column match)
	  :name (indium-source-mapping-name match))))

(defun indium-sourcemap-generated-position-for (sourcemap source line column)
  "Given SOURCEMAP, find the generated position for SOURCE at LINE and COLUMN.
SOURCEMAP should be an `indium-sourcemap' struct.

Return a plist with `:source', `:line', `:column', and `:name',
or nil if not found."
  (let ((same-source-map (indium-sourcemap--filter-same-source sourcemap source)))
    (when-let ((match (indium-sourcemap--binary-search same-source-map line column t)))
      (list :source (indium-source-mapping-source match)
	    :line (indium-source-mapping-generated-line match)
	    :column (indium-source-mapping-generated-column match)
	    :name (indium-source-mapping-name match)))))


(defun indium--base64-decode (char)
  "Decode a single base64 CHAR into its corresponding integer value.

Raise an error if the character is invalid."
  (or (gethash char indium--base64-char-to-int-map)
      (error "Invalid base 64 characters: %c" char)))

(defun indium--from-vlq-signed (value)
"Convert to a two-complement value from VALUE.

The sign bit is is placed in the least significant bit.  For
example, as decimals: 2 (10 binary) becomes 1, 3 (11 binary)
becomes -1, 4 (100 binary) becomes 2, 5 (101 binary) becomes -2."
  (let ((shifted (lsh value -1)))
    (if (eq 1 (logand value 1))
        (- shifted)
      shifted)))

(defun indium--base64-vlq-decode (string-as-list)
  "Decode the next base 64 VLQ value from the given STRING-AS-LIST.

Return the value and the rest of the string as values, that is a
list (VALUE STRING-REST)."
  (let ((result 0)
        (continuation t)
        (shift 0))
    (while continuation
      (when (null string-as-list)
        (error "Expected more digits in base 64 VLQ value"))
      (let ((digit (indium--base64-decode (car string-as-list))))
        (setq continuation
              (not (eq 0 (logand digit indium-sourcemap--vlq-continuation-bit))))
        (setq digit (logand digit indium-sourcemap--vlq-base-mask))
        (cl-incf result (lsh digit shift)))
      (cl-incf shift indium-sourcemap--vlq-base-shift)
      (setq string-as-list (cdr string-as-list)))
    (list :value (indium--from-vlq-signed result)
          :rest string-as-list)))

(defun indium-sourcemap--decode (json)
  "Decode JSON object.
Return a `indium-sourcemap' struct."

  (when (not (eq (map-elt json 'version)
                 indium--supported-sourcemap-version))
    (error "Unsupported source map version %s"
           (map-elt json 'version)))

  (let* ((source-root (map-elt json 'sourceRoot))
         (names (map-elt json 'names))
         (sources (map-elt json 'sources))
         (string (string-to-list (map-elt json 'mappings)))
         (result (make-indium-sourcemap
                  :names names
                  :sources sources))
         (generated-mappings-list)
         (generated-line 1)
         (previous-generated-column 0)
         (previous-original-line 0)
         (previous-original-column 0)
         (previous-source 0)
         (previous-name 0))
    (cl-flet
     ((starts-with-mapping-separator (string)
                                     (or (null string)
                                         (eq (car string) ?,)
                                         (eq (car string) ?\;))))
     (while string
       (cond
        ((eq (car string) ?\;)
         (cl-incf generated-line)
         (setq string (cdr string))
         (setq previous-generated-column 0))
        ((eq (car string) ?,)
         (setq string (cdr string)))
        (t
         (let ((mapping (make-indium-source-mapping
                         :generated-line generated-line)))

           ;; Generated column.
           (let ((temp (indium--base64-vlq-decode string)))
             (setf (indium-source-mapping-generated-column mapping)
                   (+ previous-generated-column
                      (plist-get temp :value)))
             (setq previous-generated-column
                   (indium-source-mapping-generated-column mapping))
             (setq string (plist-get temp :rest)))

           (when (not (starts-with-mapping-separator string))

             ;; Original source.
             (let ((temp (indium--base64-vlq-decode string)))
               (setf (indium-source-mapping-source mapping)
                     (concat source-root
                             (elt sources
                                  (+ previous-source
                                     (plist-get temp :value)))))
               (cl-incf previous-source (plist-get temp :value))
               (setq string (plist-get temp :rest)))

             (when (starts-with-mapping-separator string)
               (error "Found a source, but no line and column"))

             ;; Original line.
             (let ((temp (indium--base64-vlq-decode string)))
               (setf (indium-source-mapping-original-line mapping)
                     (+ previous-original-line
                        (plist-get temp :value)))
               (setq previous-original-line
                     (indium-source-mapping-original-line mapping))

               ;; Lines are stored 0-based
               (cl-incf (indium-source-mapping-original-line mapping))

               (setq string (plist-get temp :rest)))

             (when (starts-with-mapping-separator string)
               (error "Found a source and line, but no column"))

             ;; Original column
             (let ((temp (indium--base64-vlq-decode string)))
               (setf (indium-source-mapping-original-column mapping)
                     (+ previous-original-column
                        (plist-get temp :value)))
               (setq previous-original-column
                     (indium-source-mapping-original-column mapping))

               (setq string (plist-get temp :rest)))

             (when (not (starts-with-mapping-separator string))

               ;; Original name
               (let ((temp (indium--base64-vlq-decode string)))
                 (setf (indium-source-mapping-name mapping)
                       (elt names (+ previous-name
                                     (plist-get temp :value))))
                 (cl-incf previous-name (plist-get temp :value))

                 (setq string (plist-get temp :rest)))))

           (push mapping generated-mappings-list)))))

     (setf (indium-sourcemap-generated-mappings result)
           (vconcat (nreverse generated-mappings-list))))
    result))

(defun indium-sourcemap--filter-same-source (sourcemap source)
  "Return a copy of SOURCEMAP with entries filtered for SOURCE only."
  (let ((map (copy-indium-sourcemap sourcemap)))
    (setf (indium-sourcemap-generated-mappings map)
	  (seq-filter (lambda (mapping)
			(string= (indium-source-mapping-source mapping)
				 source))
		      (indium-sourcemap-generated-mappings map)))
    map))

(defun indium-sourcemap--binary-search (sourcemap line column &optional generated)
  "Given SOURCEMAP, find the position for LINE and COLUMN.
If GENERATED is nil, find an original position, otherwise find a
generated position.

Return a plist with `:source', `:line', `:column', and `:name',
or nil if not found.

This is an implementation of binary search which will always try
and return the next lowest value checked if there is no exact
hit.  This is because mappings between original and generated
line/col pairs are single points, and there is an implicit region
between each of them, so a miss just means that you aren't on the
very start of a region."

  (when (<= line 0)
    (error "Line must be greater than or equal to 1"))
  (when (< column 0)
    (error "Column must be greater than or equal to 0"))

  (let* ((haystack (indium-sourcemap-generated-mappings sourcemap))
         (low -1)
         (high (length haystack))
         terminate
         found
	 line-fn
	 column-fn)
    (if generated
	(progn
	  (setq line-fn #'indium-source-mapping-original-line)
	  (setq column-fn #'indium-source-mapping-original-column))
      (progn
	(setq line-fn #'indium-source-mapping-generated-line)
	(setq column-fn #'indium-source-mapping-generated-column)))

    (when (> (length haystack) 0)

      ;; This terminates when one of the following is true:
      ;;
      ;;   1. We find the exact element we are looking for.
      ;;
      ;;   2. We did not find the exact element, but we can return the
      ;;      next closest element that is less than that element.
      ;;
      ;;   3. We did not find the exact element, and there is no
      ;;      next-closest element which is less than the one we are
      ;;      searching for, so we return null.
      (while (not terminate)
        (let* ((mid (floor (+ (/ (- high low) 2) low)))
               (cur (elt haystack mid)))
          (cond
           ((and (eq (funcall line-fn cur) line)
                 (eq (funcall column-fn cur) column))
            ;; Found the element we are looking for.
            (setq found cur)
            (setq terminate t))

           ((or (< (funcall line-fn cur) line)
                (and (eq (funcall line-fn cur) line)
                     (< (funcall column-fn cur)
                        column)))
            ;; haystack[mid] is greater than our needle.
            (if (> (- high mid) 1)
                ;; The element is in the upper half.
                (setq low mid)
              ;; We did not find an exact match, return the next
              ;; closest one (termination case 2).
              (setq found cur)
              (setq terminate t)))

           (t
            ;; haystack[mid] is less than our needle.
            (if (> (- mid low) 1)
                ;; The element is in the lower half.
                (setq high mid)
              ;; The exact needle element was not found in this
              ;; haystack. Determine if we are in termination case (2)
              ;; or (3) and return the appropriate thing.
              (unless (< low 0)
                (setq found (elt haystack low)))
              (setq terminate t))))))
      found)))

(provide 'indium-sourcemap)

;;; indium-sourcemap.el ends here
