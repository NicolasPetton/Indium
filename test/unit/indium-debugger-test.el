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
    (let ((frames '(first second))
          (current-frame 'first))
      (indium-debugger-set-frames frames)
      (expect indium-debugger-frames :to-be frames)
      (expect indium-debugger-current-frame :to-be current-frame)))

  (it "can unset the debugging frames"
    (indium-debugger-set-frames '(first second))
    (indium-debugger-unset-frames)
    (expect indium-debugger-frames :to-be nil)
    (expect indium-debugger-current-frame :to-be nil)))

(describe "Jumping to the next/previous frame"
  (before-each
    (spy-on 'indium-debugger-select-frame))

  (it "can select the next frame"
    (let ((frames '(first second)))
      (indium-debugger-set-frames frames)
      (setq indium-debugger-current-frame 'second)
      (indium-debugger-next-frame)
      (expect 'indium-debugger-select-frame :to-have-been-called-with 'first)))

  (it "can select the previous frame"
    (let ((frames '(first second)))
      (indium-debugger-set-frames frames)
      (indium-debugger-previous-frame)
      (expect 'indium-debugger-select-frame :to-have-been-called-with 'second)))

  (it "should throw when selecting the next frame if it does not exist"
    (let ((frames '(first second)))
      (indium-debugger-set-frames frames)
      (expect (indium-debugger-next-frame) :to-throw 'user-error)))

  (it "should throw when selecting the previous frame if it does not exist"
    (let ((frames '(first second)))
      (indium-debugger-set-frames frames)
      (setq indium-debugger-current-frame 'second)
      (expect (indium-debugger-previous-frame) :to-throw 'user-error))))

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
    (spy-on 'indium-client-resume)
    (expect (get-buffer (indium-debugger--buffer-name-no-file)) :not :to-be nil)
    (indium-client-resume)
    (expect (get-buffer (indium-debugger--buffer-name-no-file)) :not :to-be nil)))

(describe "Debugger stepping"
  ;; Cleaning up the debugger state should only be done when the execution is
  ;; resumed, which happens between each step over/into/out.
  (it "should not unset the debugger buffer when stepping"
    (spy-on 'indium-debugger-unset-current-buffer)
    (spy-on 'indium-client-step-into)
    (spy-on 'indium-client-step-out)
    (spy-on 'indium-client-step-over)

    (indium-debugger-step-into)
    (expect #'indium-debugger-unset-current-buffer :not :to-have-been-called)
    (indium-client-step-over)
    (expect #'indium-debugger-unset-current-buffer :not :to-have-been-called)
    (indium-client-step-out)
    (expect #'indium-debugger-unset-current-buffer :not :to-have-been-called))

  (it "should call the client when stepping into"
    (spy-on 'indium-client-step-into)
    (indium-debugger-step-into)
    (expect #'indium-client-step-into :to-have-been-called))

  (it "should call the client when stepping over"
    (spy-on 'indium-client-step-over)
    (indium-debugger-step-over)
    (expect #'indium-client-step-over :to-have-been-called))

  (it "should call the client when stepping out"
    (spy-on 'indium-client-step-out)
    (indium-debugger-step-out)
    (expect #'indium-client-step-out :to-have-been-called))

  (it "should call the client when resuming execution"
    (spy-on 'indium-client-resume)
    (indium-debugger-resume)
    (expect #'indium-client-resume :to-have-been-called))

  (it "should call the client when jumping to a location"
    (spy-on 'indium-client-continue-to-location)
    (indium-debugger-here)
    (expect #'indium-client-continue-to-location
	    :to-have-been-called-with (indium-location-at-point))))

(provide 'indium-debugger-test)
;;; indium-debugger-test.el ends here
