;;; indium-chrome-test.el --- tests for indium-chrome.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Nicolas Petton

;; Author: Nicolas Petton <nicolas@petton.fr>
;; Keywords: test

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

;;

;;; Code:

(require 'buttercup)
(require 'indium-chrome)

(describe "Selecting a workspace"
  (it "should ask for a workspace when using the HTTP protocol"
    (spy-on 'websocket-url)
    (spy-on 'indium-v8--open-ws-connection)
    (spy-on 'indium-workspace-read)
    (indium-chrome--connect-to-tab-with-url "http://foo.com" '())
    (expect #'indium-workspace-read :to-have-been-called))

  (it "should not ask for a workspace when using the file:// protocol"
    (spy-on 'websocket-url)
    (spy-on 'indium-v8--open-ws-connection)
    (spy-on 'indium-workspace-read)
    (indium-chrome--connect-to-tab-with-url "file:///home/foo" '())
    (expect #'indium-workspace-read :not :to-have-been-called)))

(describe "Regression test for GH issue #97"
  (it "should not create multiple REPL buffers"
    (let ((buf (indium-repl-get-buffer-create)))
      (expect (indium-repl-get-buffer-create) :to-be buf))))

(provide 'indium-chrome-test)
;;; indium-chrome-test.el ends here
