;;; indium-script.el --- Handle scripts for a connection  -*- lexical-binding: t; -*-

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

;; Handle script source registration, script locations (with sourcemap support)
;; for the current indium connection.

;;; Code:

(require 'seq)
(require 'map)
(require 'rx)
(require 'indium-backend)
(require 'indium-structs)
(require 'indium-workspace)
(require 'indium-backend)
(require 'indium-sourcemap)
(require 'url)
(require 'url-http)
(require 'url-handlers)
(require 'subr-x)

(defgroup indium-script nil
  "Indium script and location handling"
  :prefix "indium-script-"
  :group 'indium)

(defcustom indium-script-enable-sourcemaps t
  "When non-nil, use sourcemaps when debugging."
  :type 'boolean)

(defvar indium-script-default-sourcemap-path-overrides
  '(("webpack:///./~/" . "${root}/node_modules/")
    ("webpack:///src/" . "${root}/")
    ("webpack:///./"   . "${root}/")
    ("webpack:///"     . "/"))
  "Sourcemap mappings that are included by default in Indium.
Any override in the workspace configuration will override this
defaults.")

(defun indium-location-url (location)
  "Lookup the url associated with LOCATION's file."
  (indium-workspace-make-url (indium-location-file location)))

(defun indium-script-add-script-parsed (id url &optional sourcemap-url)
  "Add a parsed script from the runtime with ID at URL.
If SOURCEMAP-URL is non-nil, add it to the parsed script.

Return the new parsed script.

If an existing script has the same URL, remove that script first,
so that the new script overrides it, as we cannot have multiple
parsed scripts with the same URL."
  (when-let ((script (indium-script-find-from-url url)))
    (map-delete (indium-current-connection-scripts)
  		(intern (indium-script-id script))))
  (let ((script (make-indium-script :id id
				    :url url
				    :sourcemap-url sourcemap-url)))
    ;; TODO Should use `indum-current-connection-scripts' but I get a
    ;; compilation warning.
    (map-put (indium-connection-scripts indium-current-connection)
    	     (intern id)
    	     script)
    script))

(defun indium-script-find-by-id (id)
  "Return the parsed script with id ID in the current connection.
If not such script was parsed, return nil."
  (map-elt (indium-current-connection-scripts) (intern id)))

(defun indium-script-get-file (script &optional ignore-existence)
  "Lookup the local file associated with SCRIPT.
If no local file can be found and IGNORE-EXISTENCE is nil, return nil."
  (indium-workspace-lookup-file (indium-script-url script) ignore-existence))

(defun indium-script-find-from-location (location)
  "Return the script associated to LOCATION.

LOCATION can either be a buffer location or a
generated (sourcemap) script location."
  (let ((file (indium-location-file location)))
    (or (indium-script-find-from-file file)
	(indium-script-find-from-url file))))

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
	       (original-location (indium-sourcemap-original-position-for
				   sourcemap
				   (1+ (indium-location-line location))
				   (1+ (indium-location-column location))))
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
				   (generated-location (indium-sourcemap-generated-position-for
							sourcemap
							(indium-location-file location)
							(1+ (indium-location-line location))
							0)))
			      (make-indium-location :file (indium-script-url script)
						    :line (max 0 (1- (plist-get generated-location :line)))
						    :column (plist-get generated-location :column))))
			(indium-script-all-scripts-with-sourcemap))
	      location)
	location))))

(defun indium-script-generated-location-at-point ()
  "Return a location for the position of point.
If no location can be found, return nil."
  (indium-script-generated-location
   (indium-location-at-point)))

(defun indium-script-sourcemap (script)
  "Return the sourcemap object associated with SCRIPT.
The sourcemap object is cached in SCRIPT.

If no local sourcemap file can be found, try to download it.
If the sourcemap file cannot be downloaded either, return nil."
  (when (indium-script-has-sourcemap-p script)
    (unless (indium-script-sourcemap-cache script)
      (setf (indium-script-sourcemap-cache script)
	    (or (indium-script--sourcemap-from-data-url script)
                (if-let ((file (indium-script--sourcemap-file script)))
		    (indium-sourcemap-from-file file)
                  (when-let ((str (indium-script--download
                                   (indium-script--absolute-sourcemap-url script))))
                    (indium-sourcemap-from-string str)))))
      (when-let (sourcemap (indium-script-sourcemap-cache script))
	(indium-script--transform-sourcemap-sources sourcemap script)))
    (indium-script-sourcemap-cache script)))

(defun indium-script--sourcemap-from-data-url (script)
  "Return the sourcemap for SCRIPT if it's specified by a data url.
If the sourcemap url is not a data url, return nil."
  (let ((url (indium-script-sourcemap-url script)) buf)
    (when (and url (string-prefix-p "data:" url))
      (setq buf (url-data (url-generic-parse-url url)))
      (with-current-buffer buf
        ;; `url-insert' does not handle Content-Transfer-Encoding;
        ;; this is adapted from
        ;; `url-handle-content-transfer-encoding', which handles gzip
        (when-let (cte (mail-fetch-field "content-transfer-encoding"))
          (cond
           ((string= cte "base64")
            (save-restriction
              (widen)
              (goto-char (point-min))
              (when (search-forward "\n\n")
                (base64-decode-region (point) (point-max)))))
           ((string= cte "8bit"))
           (t (error "Unknown Content-Transfer-Encoding %s" cte)))))
      (with-temp-buffer
        (url-insert buf)
        (goto-char (point-min))
        (indium-sourcemap--decode (json-read))))))

(defun indium-script--sourcemap-path-overrides ()
  "Return the sourcemap path overrides from the workspace settings.
If no overrides are defined, return the default ones.

Override paths are expanded."
  (let ((overrides (map-elt indium-workspace-configuration 'sourceMapPathOverrides
			    indium-script-default-sourcemap-path-overrides)))
    (map-apply (lambda (regexp transformation)
		 (cons regexp (indium-script--expand-path-override transformation)))
	       overrides)))

(defun indium-script--expand-path-override (path)
  "Return PATH expanded.

Occurrences of ${root} (alias ${webRoot}) are replaced with the
absolute path of the root directory of the project as returned
by `indium-workspace-root'."
  (save-match-data
    (if (string-match (rx (or "${root}" "${webRoot}")) path)
	(expand-file-name (replace-match (indium-workspace-root) nil t path))
      path)))

(defun indium-script--transform-sourcemap-sources (sourcemap script)
  "Transform source mappings in SOURCEMAP to locations on disk.

Some source mappings might not be usable as is an need
transformation to map to source paths on disk.

The transformation map is read from
`indium-script--sourcemap-path-overrides'.

Paths relative to SCRIPT are also converted to absolute paths
based on the directory path of SCRIPT."
  (let ((dir (file-name-directory (indium-script-get-file script t)))
	(overrides (indium-script--sourcemap-path-overrides)))
    (seq-do (lambda (mapping)
	      (when (indium-source-mapping-source mapping)
		(indium-script--apply-sourcemap-path-overrides mapping overrides)
		(indium-script--apply-absolute-sourcemap-path mapping dir)))
	    (indium-sourcemap-generated-mappings sourcemap))))

(defun indium-script--apply-sourcemap-path-overrides (mapping overrides)
  "Mutate MAPPING by applying sourcemap path overrides on its source.
OVERRIDES is an alist of '(REGEXP . OVERRIDE) transformation rules."
  (when (indium-source-mapping-source mapping)
    (map-apply (lambda (regexp transformation)
		 (let ((source (indium-source-mapping-source mapping)))
		   (unless (file-name-absolute-p source)
		     (save-match-data
		       (when (string-match regexp source)
			 (setf (indium-source-mapping-source mapping)
			       (replace-match
				transformation
				nil t source)))))))
	       overrides)))

(defun indium-script--apply-absolute-sourcemap-path (mapping dir)
  "Mutate MAPPING by setting its source to an absolute path based on DIR.
Do nothing if MAPPING's source is already an absolute path.

Mapping paths can be either absolute, or relative to a SCRIPT's
directory.  To make things simpler with sourcemaps manipulation,
make all source paths absolute."
  (when-let ((source (indium-source-mapping-source mapping)))
    (unless (file-name-absolute-p source)
      (setf (indium-source-mapping-source mapping)
	    (expand-file-name source dir)))))

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
  (message "Downloading sourcemap file %s..." url)
  (when-let ((buf (condition-case nil
		      (url-retrieve-synchronously url t)
		    (error nil))))
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
    (unless (string-empty-p url)
      (url-expand-file-name sourcemap-url url))))

(provide 'indium-script)
;;; indium-script.el ends here
