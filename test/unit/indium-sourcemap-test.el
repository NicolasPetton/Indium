;;; indium-sourcemap-test.el --- Test for indium-sourcemap.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2018  Nicolas Petton

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
(require 'map)

(require 'indium-sourcemap)

(defconst indium-test-sourcemap-json
  '((version . 3)
    (file . "test.coffee.map")
    (sources . ["test.coffee"])
    (names . ["foo" "bar"])
    (mappings . "\
AAAC;;;EAAA,eAAK,IAAL,GAAa,SAAA,CAAA,QAAA,CAAA;;;MACV,2BAAmB,YAAnB,aAAA,\
CAAA,KAAA,CAAA;QAAI,OAAe;QAAT;oBACR,QAAQ,KAAR,CAAc,IAAd,EAAoB,KAApB;;;;;\
EAED,GAAA,GAAM,CAAA;AAAA,IAAC,CAAD;AAAA,IAAG,CAAH;AAAA,IAAK,CAAL;AAAA,\
EAAA;EACV,MAAA,GAAS,GAAG,IAAH,CAAQ,SAAA,CAAA,IAAA,CAAA;WAAU,IAAA,CAAA,\
CAAA,CAAO;GAAzB;EAET,OAAO,IAAP,CAAY,MAAZ")))

(describe "Decoding sourcemaps"
  (before-each
    (indium-sourcemap--reset-cache))

  (after-each
    (indium-sourcemap--reset-cache))

  (it "should read files as JSON when decoding"
    (spy-on 'indium-sourcemap--decode :and-call-through)
    ;; (spy-on 'json-read-file :and-return-value indium-test-sourcemap-json)
    (spy-on 'insert-file-contents :and-call-fake
	    (lambda (_)
	      (insert (json-encode indium-test-sourcemap-json))))
    (indium-sourcemap-from-file "foo.js")
    (expect #'indium-sourcemap--decode
	    :to-have-been-called-with indium-test-sourcemap-json))

  (it "should read strings as JSON when decoding"
    (spy-on 'indium-sourcemap--decode :and-call-through)
    (spy-on 'json-read-from-string :and-return-value indium-test-sourcemap-json)
    (indium-sourcemap-from-string "test")
    (expect #'indium-sourcemap--decode
	    :to-have-been-called-with indium-test-sourcemap-json))

  (it "should fail if there is no sourcemap version"
    (expect (indium-sourcemap--decode '((sources . ["foo.js"])
					(file . "bar.js")
					(names . [])
					(mappings . "")))
	    :to-throw))

  (it "Should decode Base64"
    (expect (indium--base64-decode ?A) :to-equal 0)
    (expect (indium--base64-decode ?Z) :to-equal 25)
    (expect (indium--base64-decode ?a) :to-equal 26)
    (expect (indium--base64-decode ?z) :to-equal 51)
    (expect (indium--base64-decode ?0) :to-equal 52)
    (expect (indium--base64-decode ?9) :to-equal 61)
    (expect (indium--base64-decode ?+) :to-equal 62)
    (expect (indium--base64-decode ?/) :to-equal 63)
    (expect (indium--base64-decode ?%) :to-throw))

  (it "Should convert VLQ sign"
    (expect (indium--from-vlq-signed 2) :to-equal 1)
    (expect (indium--from-vlq-signed 3) :to-equal -1)
    (expect (indium--from-vlq-signed 4) :to-equal 2)
    (expect (indium--from-vlq-signed 5) :to-equal -2))

  (it "Should decode VLQ"
    (expect (indium--base64-vlq-decode (string-to-list "A"))
	    :to-equal `(:value 0 :rest ,(string-to-list "")))

    (expect (indium--base64-vlq-decode (string-to-list "zA"))
	    :to-equal `(:value -9 :rest ,(string-to-list "")))

    (expect (indium--base64-vlq-decode (string-to-list "zza"))
	    :to-equal `(:value -13625 :rest ,(string-to-list "")))

    (expect (indium--base64-vlq-decode (string-to-list "gzX"))
	    :to-equal `(:value 12080 :rest ,(string-to-list ""))))

  (it "Should decode sourcemaps"
    (let ((sourcemap (indium-sourcemap--decode indium-test-sourcemap-json)))
      (expect (indium-sourcemap-sources sourcemap) :to-equal '["test.coffee"])
      (expect (indium-sourcemap-names sourcemap) :to-equal '["foo" "bar"])
      (expect (length (indium-sourcemap-generated-mappings sourcemap)) :to-equal 62)
      (let ((mapping (elt (indium-sourcemap-generated-mappings sourcemap) 20)))
	(expect (indium-source-mapping-p mapping) :to-be-truthy)
	(expect (indium-source-mapping-generated-column mapping) :to-equal 28)
	(expect (indium-source-mapping-generated-line mapping) :to-equal 10)
	(expect (indium-source-mapping-source mapping) :to-equal "test.coffee")
	(expect (indium-source-mapping-original-column mapping) :to-equal 14)
	(expect (indium-source-mapping-original-line mapping) :to-equal 3)
	(expect (indium-source-mapping-name mapping) :to-be nil)))))

(describe "Sourcemap lookups"
  (it "Should lookup original positions"
    (let ((sourcemap (indium-sourcemap--decode indium-test-sourcemap-json)))
      (expect (indium-sourcemap-original-position-for sourcemap 23 10)
	      :to-equal (list :source "test.coffee"
			      :line 8
			      :column 8
			      :name nil))))

  (it "Should lookup generated positions"
    (let ((sourcemap (indium-sourcemap--decode indium-test-sourcemap-json)))
      (expect (indium-sourcemap-generated-position-for sourcemap "test.coffee" 8 8)
	      :to-equal (list :source "test.coffee"
			      :line 23
			      :column 9
			      :name nil)))))

(provide 'indium-sourcemap-test)
;;; indium-sourcemap-test.el ends here
