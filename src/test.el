;;; test.el --- Unit tests for enc encryption package -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Duane Edmonds
;;
;; Author: Duane Edmonds <duane.edmonds@gmail.com>
;; Maintainer: Duane Edmonds <duane.edmonds@gmail.com>
;; Created: August 26, 2023
;; Modified: August 27, 2023
;; Version: 0.0.4
;; Keywords: extensions files data processes tools
;; Homepage: https://github.com/usefulmove/enc
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description: enc encryption unit tests
;;
;; Code:

(load-file "~/repos/epic/src/epic.el") ; load epic functional library
(load-file (concat (file-name-directory load-file-name) ; load enc.el (from same directory)
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


(defun test-enc-round-trip (error-prelude)
  ((lambda (f s)
     (when (not (equal s (funcall f s)))
       (error (concat
                error-prelude
                "error: round-trip encryption test(s) failed"))))

   (lambda (s) ; lambda :: string -> string
     (let ((key 313))
       (_thread s
         'string-to-list
         (lambda (chars)
           (enc-encrypt-chars key chars))
         (lambda (chars)
           (enc-encrypt-chars (- key) chars))
         'join-chars)))

   "lorem ipsum dolor sit amet, consectetur adipiscing elit"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test execution

(defun test-run-tests ()
  (let* ((prelude "enc-test ... "))
    (message (concat prelude "running tests..."))
    (test-enc-encrypt-char-with-key prelude) ; encrypt char
    (test-enc-negate-string prelude) ; string negation
    (test-enc-round-trip prelude) ; round-trip test
    (message (concat prelude "passed all tests"))))

(test-run-tests)




(provide 'test)
;;; test.el ends here
