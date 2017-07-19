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

;;; Code:

(require 'seq)
(require 'indium-backend)
(require 'indium-structs)
(require 'indium-workspace)
(require 'indium-backend)
(require 'sourcemap)
(require 'memoize)

(defgroup indium-script nil
  "Indium script and location handling"
  :prefix "indium-script-"
  :group 'indium)

(defcustom indium-script-enable-sourcemaps t
  "When non-nil, use sourcemaps when debugging."
  :type 'boolean)

(defun indium-location-url (location)
  "Lookup the url associated with LOCATION's file."
  (indium-workspace-make-url (indium-location-file location)))

(defun indium-script-add-script-parsed (id url &optional sourcemap-url)
  "Add a parsed script from the runtime with ID at URL.
If SOURCEMAP-URL is non-nil, add it to the parsed script."
  (unless (map-elt indium-connection 'scripts)
    (map-put indium-connection 'scripts '()))
  (map-put (map-elt indium-connection 'scripts)
           (intern id)
	   (make-indium-script :id id
			       :url url
			       :sourcemap-url sourcemap-url)))

(defun indium-script-find-by-id (id)
  "Return the parsed script with id ID in the current connection.
If not such script was parsed, return nil."
  (map-elt (map-elt indium-connection 'scripts) (intern id)))

(defun indium-script-get-file (script)
  "Lookup the local file associated with SCRIPT.
If no local file can be found, return nil."
  (indium-workspace-lookup-file (indium-script-url script)))

(defun indium-script-find-from-url (url)
  "Lookup a script for URL.
Return nil if no script can be found."
  (seq-find #'identity
            (map-apply (lambda (_id script)
                         (when (string= url (indium-script-url script))
                           script))
                       (map-elt indium-connection 'scripts))))

(defun indium-script-find-from-file (file)
  "Lookup a script from a local FILE.
Return nil if no script can be found."
  (indium-script-find-from-url (indium-workspace-make-url file)))

(defun indium-script-has-sourcemap-p (script)
  "Return non-nil if SCRIPT has an associated sourcemap."
  (when-let ((sourcemap-url (indium-script-sourcemap-url script)))
    (not (seq-empty-p sourcemap-url))))

(defun indium-script-all-scripts-with-sourcemap ()
  "Return all parsed scripts that contain a sourcemap."
  (seq-filter #'indium-script-has-sourcemap-p
	      (map-values (map-elt indium-connection 'scripts))))

(defun indium-script-sourcemap-file (script)
  "Return the local sourcemap file associated with SCRIPT.
If no sourcemap file can be found, return nil."
  (when (indium-script-has-sourcemap-p script)
    (when-let ((script-file (indium-script-get-file script)))
      (indium-workspace-lookup-file-safe
       (expand-file-name (indium-script-sourcemap-url script)
			 (file-name-directory script-file))))))

(defun indium-script-get-frame-original-location (frame)
  "Return the location stack FRAME, possibly using sourcemaps."
  (let* ((script (indium-frame-script frame))
         (location (indium-frame-location frame)))
    (if (indium-location-file location)
	(progn
	  (indium-script-original-location script location))
      location)))

(defun indium-script-original-location (script location)
  "Use the sourcemap of SCRIPT to lookup its original LOCATION.
If SCRIPT has no sourcemap, return LOCATION."
  (if indium-script-enable-sourcemaps
      (if-let ((sourcemap-file (indium-script-sourcemap-file script))
	       (sourcemap (sourcemap-from-file sourcemap-file))
	       (original-location (indium-script--sourcemap-original-position-for
				     sourcemap
				     :line (1+ (indium-location-line location))
				     :column (1+ (indium-location-column location))
				     :nearest t))
	       (file (expand-file-name (plist-get original-location :source)
				       (file-name-directory (indium-script-get-file script)))))
          (make-indium-location :file file
				:line (max 0 (1- (plist-get original-location :line)))
				:column (plist-get original-location :column))
	(progn
	  (message "The sourcemap file does not exist!")
          location))
    location))

(defun indium-script-generated-location (location)
  "Return a generated location from the original LOCATION.

If there is a parsed script for LOCATION's file, return LOCATION.
Otherwise, if a sourcemap exists, generate a location using that
sourcemap."
  (let ((file (indium-location-file location)))
    (if (indium-script-find-from-file file)
	location
      (if indium-script-enable-sourcemaps
	  (or (seq-some (lambda (script)
			  (if-let ((sourcemap-file (indium-script-sourcemap-file script))
				   (script-file (indium-script-get-file script))
				   (sourcemap (sourcemap-from-file sourcemap-file))
				   (generated-location (sourcemap-generated-position-for
							sourcemap
							:source (file-relative-name
								 (indium-location-file location)
								 (file-name-directory script-file))
							:line (1+ (indium-location-line location))
							:column 0
							:nearest t)))
			      (make-indium-location :file script-file
						    :line (max 0 (1- (plist-get generated-location :line)))
						    :column (plist-get generated-location :column))))
			(indium-script-all-scripts-with-sourcemap))
	      location)
	location))))

(defun indium-script-generated-location-at-point ()
  "Return a location for the position of POINT.
If no location can be found, return nil."
  (indium-script-generated-location
   (make-indium-location :file buffer-file-name
			 :line (1- (line-number-at-pos)))))

;; TODO: wait for https://github.com/syohex/emacs-sourcemap/pull/6 to be merged
(defun indium-script--sourcemap-original-position-for (sourcemap &rest props)
  "Lookup a position in SOURCEMAP based on PROPS.
PROPS should be a plist with a `:line' and `:column' key."
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
