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
(require 'url)
(require 'subr-x)

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
If SOURCEMAP-URL is non-nil, add it to the parsed script.

If an existing script has the same URL, remove that script first,
so that the new script overrides it, as we cannot have multiple
parsed scripts with the same URL."
  (when-let ((script (indium-script-find-from-url url)))
    (map-delete (indium-current-connection-scripts)
  		(intern (indium-script-id script))))
  (map-put (indium-current-connection-scripts)
           (intern id)
	   (make-indium-script :id id
			       :url url
			       :sourcemap-url sourcemap-url)))

(defun indium-script-find-by-id (id)
  "Return the parsed script with id ID in the current connection.
If not such script was parsed, return nil."
  (map-elt (indium-current-connection-scripts) (intern id)))

(defun indium-script-get-file (script &optional ignore-existence)
  "Lookup the local file associated with SCRIPT.
If no local file can be found and IGNORE-EXISTENCE is nil, return nil."
  (indium-workspace-lookup-file (indium-script-url script) ignore-existence))

(defun indium-script-find-from-url (url)
  "Lookup a script for URL.
Return nil if no script can be found."
  (seq-find #'identity
            (map-apply (lambda (_id script)
                         (when (string= url (indium-script-url script))
                           script))
                       (indium-current-connection-scripts))))

(defun indium-script-find-from-file (file)
  "Lookup a script from a local FILE.
Return nil if no script can be found."
  (indium-script-find-from-url (indium-workspace-make-url file)))

(defun indium-script-has-sourcemap-p (script)
  "Return non-nil if SCRIPT has an associated sourcemap."
  (when-let ((sourcemap-url (indium-script-sourcemap-url script)))
    (not (seq-empty-p sourcemap-url))))

(defun indium-script-all-scripts-with-sourcemap ()
  "Return all parsed scripts that contain a sourcemap.
The scripts are sorted by parsed time, to ensure the newest
script is picked up first when using sourcemaps."
  (seq-sort (lambda (a b)
	      (not (time-less-p (indium-script-parsed-time a)
				(indium-script-parsed-time b))))
	    (seq-filter #'indium-script-has-sourcemap-p
			(map-values (indium-current-connection-scripts)))))

(defun indium-script-get-frame-original-location (frame)
  "Return the location stack FRAME, possibly using sourcemaps."
  (let* ((script (indium-frame-script frame))
         (location (indium-frame-location frame)))
    (if (indium-script-has-sourcemap-p script)
	(indium-script-original-location script location)
      location)))

(defun indium-script-original-location (script location)
  "Use the sourcemap of SCRIPT to lookup its original LOCATION.
If SCRIPT has no sourcemap, return LOCATION."
  (if indium-script-enable-sourcemaps
      (if-let ((sourcemap (indium-script-sourcemap script))
	       (original-location (indium-script--sourcemap-original-position-for
				   sourcemap
				   :line (1+ (indium-location-line location))
				   :column (1+ (indium-location-column location))
				   :nearest t))
	       (file (plist-get original-location :source)))
          (make-indium-location :file file
				:line (max 0 (1- (plist-get original-location :line)))
				:column (plist-get original-location :column))
	location)
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
			  (if-let ((sourcemap (indium-script-sourcemap script))
				   (script-file (indium-script-get-file script t))
				   (generated-location (sourcemap-generated-position-for
							sourcemap
							:source (indium-location-file location)
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
  (when-let ((file (buffer-file-name))
             (path (file-truename file)))
    (indium-script-generated-location
     (make-indium-location :file path
                           :line (1- (line-number-at-pos))))))

(defun indium-script-sourcemap (script)
  "Return the sourcemap object associated with SCRIPT.
The sourcemap object is cached in SCRIPT.

If no local sourcemap file can be found, try to download it.
If the sourcemap file cannot be downloaded either, return nil."
  (when (indium-script-has-sourcemap-p script)
    (unless (indium-script-sourcemap-cache script)
      (setf (indium-script-sourcemap-cache script)
	    (if-let ((file (indium-script--sourcemap-file script)))
		(sourcemap-from-file file)
	      (when-let ((str (indium-script--download
			       (indium-script--absolute-sourcemap-url script))))
		(sourcemap-from-string str))))
      (when-let (sourcemap (indium-script-sourcemap-cache script))
	(indium-script--absolute-sourcemap-sources sourcemap script)))
    (indium-script-sourcemap-cache script)))

(defconst indium-script--webpack-regexp "^webpack:///\\([^?]*\\)\\([?].*\\)?$")

(defun indium-script--webpack-path-p (path)
  (string-match-p indium-script--webpack-regexp path))

(defun indium-script--webpack-path (path)
  (replace-regexp-in-string indium-script--webpack-regexp
                            "\\1"
                            path))

(defun indium-script--sourcemap-path (path sourcemap-root webpack-root)
  (when path
    (cond ((file-name-absolute-p path) path)
          ((indium-script--webpack-path-p path)
           (let ((path (indium-script--webpack-path path)))
             (expand-file-name path webpack-root)))
          (t (expand-file-name path sourcemap-root)))))

(defun indium-script-root (script)
  (or (when-let ((path (indium-script-get-file script t)))
        (file-name-directory path))
      (indium-workspace-root)))

(defun indium-script--absolute-sourcemap-sources (sourcemap script)
  "Convert all relative source paths in SOURCEMAP to absolute ones.

Paths might be either absolute, or relative to the SCRIPT's
directory.  To make things simpler with sourcemaps manipulation,
make all source paths absolute."
  (let ((sourcemap-root (file-truename (indium-script-root script)))
        (webpack-root (file-truename (indium-workspace-root))))
    (seq-do (lambda (entry)
              (when-let ((path (sourcemap-entry-source entry))
                         (expansion (indium-script--sourcemap-path path sourcemap-root webpack-root)))
                (unless (string= path expansion)
                  (setf (sourcemap-entry-source entry) expansion))))
            sourcemap)))

(defun indium-script--sourcemap-file (script)
  "Return the local sourcemap file associated with SCRIPT.
If no sourcemap file can be found, return nil."
  (when-let ((script-file (indium-script-get-file script)))
    (indium-workspace-lookup-file-safe
     (expand-file-name (indium-script-sourcemap-url script)
		       (file-name-directory script-file)))))

(defun indium-script--download (url &optional fix-address)
  "Download and return the content of URL.
If the request fails or has no data, return nil.

Because of debbugs#17976 in Emacs <= 25.3, when the first call
fails, the function is called again with FIX-ADDRESS, in which
case 'localhost' is replaced with '127.0.0.1' in URL."
  (message "Downloading sourcemap file...")
  (let ((buf (url-retrieve-synchronously url t)))
    (with-current-buffer buf
      (message "Downloading sourcemap file...done")
      (goto-char (point-min))
      (if (re-search-forward "^HTTP/.+ 200 OK$" nil (line-end-position))
	  (when (search-forward "\n\n" nil t)
	    (buffer-substring (point) (point-max)))
	;; Fix for bug#17976
	(unless fix-address
	  (let ((url (replace-regexp-in-string "localhost" "127.0.0.1" url)))
	    (indium-script--download url t)))))))

(defun indium-script--absolute-sourcemap-url (script)
  "Return the absolute URL for the sourcemap associated with SCRIPT.

For instance, for a script located at
\"http://localhost/foo/bar.js\" with a sourcmap located at
\"bar.js.map\", return \"http://localhost/foo/bar.js.ap\"."
  (let* ((url (indium-script-url script))
	 (sourcemap-url (indium-script-sourcemap-url script)))
    (url-expand-file-name sourcemap-url url)))

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

(provide 'indium-script)
;;; indium-script.el ends here
