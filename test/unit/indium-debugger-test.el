;;; indium-debugger-test.el --- Test for indium-debugger.el  -*- lexical-binding: t; -*-

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
(require 'indium-debugger)

(describe "Debugging frames are correctly set"
  (it "can set debugger frames and current frame"
    (with-fake-indium-connection
      (let ((frames '(first second))
            (current-frame 'first))
        (indium-debugger-set-frames frames)
        (expect (indium-current-connection-frames) :to-be frames)
        (expect (indium-current-connection-current-frame) :to-be current-frame))))

  (it "can set the current frame"
    ;; We're not interested in buffer setups
    (spy-on 'indium-debugger-get-buffer-create)

    (with-fake-indium-connection
      (indium-debugger-set-current-frame 'current)
      (expect (indium-current-connection-current-frame) :to-be 'current)))

  (it "can unset the debugging frames"
    (with-fake-indium-connection
      (indium-debugger-set-frames '(first second))
      (indium-debugger-unset-frames)
      (expect (indium-current-connection-frames) :to-be nil)
      (expect (indium-current-connection-current-frame) :to-be nil))))

(describe "Jumping to the next/previous frame"
  (before-each
    (spy-on 'indium-debugger-select-frame))

  (it "can select the next frame"
    (with-fake-indium-connection
      (let ((frames '(first second)))
        (indium-debugger-set-frames frames)
        (indium-debugger-set-current-frame 'second)
        (indium-debugger-next-frame)
        (expect 'indium-debugger-select-frame :to-have-been-called-with 'first))))

  (it "can select the previous frame"
    (with-fake-indium-connection
      (let ((frames '(first second)))
        (indium-debugger-set-frames frames)
        (indium-debugger-previous-frame)
        (expect 'indium-debugger-select-frame :to-have-been-called-with 'second))))

  (it "should throw when selecting the next frame if it does not exist"
    (with-fake-indium-connection
      (let ((frames '(first second)))
        (indium-debugger-set-frames frames)
        (expect (indium-debugger-next-frame) :to-throw 'user-error))))

  (it "should throw when selecting the previous frame if it does not exist"
    (with-fake-indium-connection
      (let ((frames '(first second)))
        (indium-debugger-set-frames frames)
        (indium-debugger-set-current-frame 'second)
        (expect (indium-debugger-previous-frame) :to-throw 'user-error)))))

(describe "Regression test for GitHub issue 53"
  (before-each
    (let ((debugger-buffer (get-buffer-create (indium-debugger--buffer-name-no-file))))
      (with-current-buffer debugger-buffer
        (indium-debugger-mode))))
  (after-each
    (ignore-errors
      (kill-buffer (indium-debugger--buffer-name-no-file))))

  ;; Regression test for https://github.com/NicolasPetton/Indium/issues/53
  ;; Killing the debugger buffer when stepping results in a visual
  ;; flickering. Since stepping over or into cause the execution to be resumed
  ;; and paused, the debugger buffer should not be killed.
  (it "should not killing the debugger buffer when execution is resumed"
    (spy-on 'indium-backend-resume)
    (expect (get-buffer (indium-debugger--buffer-name-no-file)) :not :to-be nil)
    (indium-debugger-resume)
    (expect (get-buffer (indium-debugger--buffer-name-no-file)) :not :to-be nil)))

(describe "Debugger stepping"
  ;; Cleaning up the debugger state should only be done when the execution is
  ;; resumed, which happens between each step over/into/out.
  (it "should not unset the debugger buffer when stepping"
    (spy-on 'indium-debugger-unset-current-buffer)
    (spy-on 'indium-backend-step-into)
    (spy-on 'indium-backend-step-out)
    (spy-on 'indium-backend-step-over)

    (indium-debugger-step-into)
    (expect #'indium-debugger-unset-current-buffer :not :to-have-been-called)
    (indium-debugger-step-over)
    (expect #'indium-debugger-unset-current-buffer :not :to-have-been-called)
    (indium-debugger-step-out)
    (expect #'indium-debugger-unset-current-buffer :not :to-have-been-called))

  (it "should call the backend when stepping into"
    (with-fake-indium-connection
      (spy-on 'indium-backend-step-into)
      (indium-debugger-step-into)
      (expect #'indium-backend-step-into :to-have-been-called-with 'fake)))

  (it "should call the backend when stepping over"
    (with-fake-indium-connection
      (spy-on 'indium-backend-step-over)
      (indium-debugger-step-over)
      (expect #'indium-backend-step-over :to-have-been-called-with 'fake)))

  (it "should call the backend when stepping out"
    (with-fake-indium-connection
      (spy-on 'indium-backend-step-out)
      (indium-debugger-step-out)
      (expect #'indium-backend-step-out :to-have-been-called-with 'fake)))

  (it "should call the backend when resuming execution"
    (with-fake-indium-connection
      (spy-on 'indium-backend-resume)
      (indium-debugger-resume)
      (expect #'indium-backend-resume :to-have-been-called-with 'fake)))

  (it "should call the backend when jumping to a location"
    (with-fake-indium-connection
      (spy-on 'indium-backend-continue-to-location)
      (spy-on 'indium-script-generated-location-at-point :and-return-value 'location)
      (indium-debugger-here)
      (expect #'indium-backend-continue-to-location :to-have-been-called-with 'fake 'location))))

(provide 'indium-debugger-test)
;;; indium-debugger-test.el ends here
