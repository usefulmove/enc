;;; enc-test.el --- Unit tests for enc encryption package -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Duane Edmonds
;;
;; Author: Duane Edmonds <duane.edmonds@gmail.com>
;; Maintainer: Duane Edmonds <duane.edmonds@gmail.com>
;; Created: August 26, 2023
;; Modified: September 14, 2023
;; Version: 0.0.18
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


; load Cora language
(add-to-list 'load-path "~/repos/cora/src/")
(require 'cora)


; load enc.el from same directory as (this) test file
(load-file (concat (file-name-directory load-file-name)
                   "enc.el"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test definitions

(defun enc-test-negate-string (error-prelude)
  (when (not (equal "8" (enc-string-negate "-8")))
    (error (concat error-prelude "error: negate string test(s) failed"))))


(defun enc-test-encrypt-char-with-key (error-prelude)
  (when (not (equal ?-
                    (call (enc-encrypt-char 0) ?-)))
    (error (concat error-prelude "error: encrypt char test(s) failed")))
  (when (not (equal ?`
                    (call (enc-encrypt-char -3) ?c)))
    (error (concat error-prelude "error: encrypt char test(s) failed"))))


(defun enc-test-string-encryption (error-prelude)
  (assert-equal
    (enc-encrypt-string 313 "lorem ipsum dolor sit amet, consectetur adipiscing elit")
    "1&)\"<$+& 0&-&!}</21\"1 \"0+, <H1\"*}<1&0</,),!<*20-&<*\"/,)"
    (error (concat error-prelude "error: string encryption test(s) failed"))))


(defun enc-test-round-trip (error-prelude)
  (let ((s "lorem ipsum dolor sit amet, consectetur adipiscing elit")
        (key 313))
    (assert-equal
      (thread s
        (lambda (str)
          (enc-encrypt-string key str))
        (lambda (str)
          (enc-encrypt-string (- key) str)))
      s
      (error (concat error-prelude "error: round-trip test(s) failed"))))
  (let ((s "lorem ipsum dolor sit amet, consectetur adipiscing elit"))
    (assert-equal
      (enc-decrypt-string 512 (enc-encrypt-string 512 s))
      s
      (error (concat
               error-prelude
               "error: round-trip test(s) failed")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test execution


(defun enc-test-run-tests (&rest tests)
  (letrec ((prelude "enc-test ... ")
           (execute-tests (lambda (fns)
                            (if (null fns) nil
                                (do
                                  (call (car fns) prelude)
                                  (call execute-tests (cdr fns)))))))
    (message (concat prelude "running tests..."))
    (call execute-tests tests)
    (message (concat prelude "passed all tests"))))


(enc-test-run-tests
  'enc-test-encrypt-char-with-key
  'enc-test-negate-string
  'enc-test-string-encryption
  'enc-test-round-trip)



(provide 'enc-test)
;;; enc-test.el ends here
