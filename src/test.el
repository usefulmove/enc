;;; test.el --- Unit tests for enc encryption package -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Duane Edmonds
;;
;; Author: Duane Edmonds <duane.edmonds@gmail.com>
;; Maintainer: Duane Edmonds <duane.edmonds@gmail.com>
;; Created: August 26, 2023
;; Modified: August 26, 2023
;; Version: 0.0.1
;; Keywords: extensions files data processes tools
;; Homepage: https://github.com/usefulmove/enc
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(load-file (concat (file-name-directory load-file-name)
                   "enc.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test definitions

(defun test-enc-negate-string (error-prelude)
  (when (not (equal "8" (enc-string-negate "-8")))
    (error (concat error-prelude "error: negate string test(s) failed"))))


(defun test-enc-encrypt-char-with-key (error-prelude)
  (when (not (equal ?- (funcall (enc-encrypt-char-with-key 0) ?-)))
    (error (concat error-prelude "error: encrypt char test(s) failed")))
  (when (not (equal ?` (funcall (enc-encrypt-char-with-key -3) ?c)))
    (error (concat error-prelude "error: encrypt char test(s) failed"))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test execution

(defun test-run-tests ()
  (let ((error-prelude "enc-test ... "))
    (message (concat error-prelude "running tests..."))
    (test-enc-encrypt-char-with-key error-prelude) ; encrypt char
    (test-enc-negate-string error-prelude) ; string negation
    (message (concat error-prelude "passed all tests"))))

(test-run-tests)



(provide 'test)
;;; test.el ends here
