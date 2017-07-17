;;; indium-script.el --- Handle scripts for a connection  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Nicolas Petton

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

;; Handle script source registration, script locations (with sourcemap support)
;; for the current indium connection.
;;
;; Scripts are alists indexed by id in the current Indium connection.  A script
;; contain an `url' key, and an optional `sourcemap-url' key.
;;
;; A location is an alist with a `lineNumber' and `columnNumber' key.  If a
;; location points to a local file, it also contains a `file' key.  Columns and
;; lines start at 0.

;;; Code:

(require 'seq)
(require 'indium-backend)
(require 'indium-workspace)
(require 'indium-backend)
(require 'sourcemap)
(require 'memoize)

(defun indium-script-add-script-parsed (id url &optional sourcemap-url)
  "Add a parsed script from the runtime with ID at URL.
If SOURCEMAP-URL is non-nil, add it to the parsed script."
  (unless (map-elt indium-connection 'scripts)
    (map-put indium-connection 'scripts '()))
  (map-put (map-elt indium-connection 'scripts)
           (intern id)
           `((url . ,url)
             (sourcemap-url . ,sourcemap-url))))

(defun indium-script-get (id)
  "Return the location for the script with id ID.
If not such script was parsed, return nil."
  (map-elt (map-elt indium-connection 'scripts) (intern id)))

(defun indium-script-get-url (script)
  "Return the url for SCRIPT."
  (map-elt script 'url))

(defun indium-script-get-file (script)
  "Lookup the local file associated with SCRIPT.
If no local file can be found, return nil."
  (indium-workspace-lookup-file (indium-script-get-url script)))

(defun indium-script-get-id (url)
  "Lookup the parsed script id for URL."
  (seq-find #'identity
            (map-apply (lambda (key script)
                         (when (string= url (map-elt script 'url))
                           (symbol-name key)))
                       (map-elt indium-connection 'scripts))))

(defun indium-script-has-sourcemap-p (script)
  "Return non-nil if SCRIPT has an associated sourcemap."
  (let ((sourcemap-url (map-elt script 'sourcemap-url)))
    (and sourcemap-url (not (seq-empty-p sourcemap-url)))))

(defun indium-script-get-location (frame)
  "Return the location stack FRAME.
The location is an list with `lineNumber' and `columnNumber'
keys.  The location also contains a `file' key if a local file is
associated to the FRAME."
  (let* ((script (indium-backend-get-script (indium-backend) frame))
         (location (map-elt frame 'location))
         (file (indium-workspace-lookup-file (indium-script-get-url script))))
    (if file
	(progn
	  (map-put location 'file file)
	  (indium-script-original-location script location))
      location)))

(defun indium-script-original-location (script location)
  "Use the sourcemap of SCRIPT to lookup its original LOCATION.
If SCRIPT has no sourcemap, return LOCATION.  LOCATION is an
alist with the `lineNumber' and `columnNumber' keys."
  (if (indium-script-has-sourcemap-p script)
      (if-let ((script-file (indium-script-get-file script))
	       (sourcemap-file (indium-workspace-lookup-file-safe
                                (expand-file-name (map-elt script 'sourcemap-url)
						  (file-name-directory script-file)))))
          (let* ((sourcemap (sourcemap-from-file sourcemap-file))
                 (original-location (indium-script--sourcemap-original-position-for
				     sourcemap
				     :line (1+ (map-elt location 'lineNumber))
				     :column (1+ (map-elt location 'columnNumber))
				     :nearest t)))
	    (if original-location
		(let ((file (expand-file-name (plist-get original-location :source)
					      (file-name-directory script-file))))
		  `((file . ,file)
		    (lineNumber . ,(max 0 (1- (plist-get original-location :line))))
		    (columnNumber . ,(max 0 (1- (plist-get original-location :column))))))
	      (progn
		(message "Could not locate original position from sourcemap!")
		location)))
	(progn
	  (message "The sourcemap file does not exist!")
          location))
    location))

;; TODO: wait for https://github.com/syohex/emacs-sourcemap/pull/6 to be merged
(defun indium-script--sourcemap-original-position-for (sourcemap &rest props)
  (let ((here (make-sourcemap-entry :generated-line (plist-get props :line)
                                    :generated-column (plist-get props :column))))
    (let ((ret (sourcemap--binary-search sourcemap here 'generated
					 (plist-get props :nearest))))
      (when ret
        (list :source (sourcemap-entry-source ret)
              :line (sourcemap-entry-original-line ret)
              :column (sourcemap-entry-original-column ret))))))


(memoize 'indium-script-original-location "2 minutes")

(provide 'indium-script)
;;; indium-script.el ends here
