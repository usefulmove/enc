;;; enc-test.el --- Unit tests for enc encryption package -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Duane Edmonds
;;
;; Author: Duane Edmonds <duane.edmonds@gmail.com>
;; Maintainer: Duane Edmonds <duane.edmonds@gmail.com>
;; Created: August 26, 2023
;; Modified: September 3, 2023
;; Version: 0.0.9
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

(load-file "~/repos/cora/src/cora.el") ; load Cora language
(load-file (concat (file-name-directory load-file-name) ; load enc.el (from same directory)
                   "enc.el"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test definitions

(defun enc-test-negate-string (error-prelude)
  (when (not (equal "8" (enc-string-negate "-8")))
    (error (concat error-prelude "error: negate string test(s) failed"))))


(defun enc-test-encrypt-char-with-key (error-prelude)
  (when (not (equal ?-
                    (call (enc-encrypt-char-with-key 0) ?-)))
    (error (concat error-prelude "error: encrypt char test(s) failed")))
  (when (not (equal ?`
                    (call (enc-encrypt-char-with-key -3) ?c)))
    (error (concat error-prelude "error: encrypt char test(s) failed"))))


(defun enc-test-string-encryption (error-prelude)
  ((lambda (f s s-enc)
     (when (not (equal s-enc
                       (call f s)))
       (error (concat
                error-prelude
                "error: string encryption test(s) failed"))))

   (lambda (s) ; _ :: string -> string
     (let ((key 313))
       (thread s
         (lambda (chars) (enc-encrypt-chars key chars))
         'enc-join-chars)))

   "lorem ipsum dolor sit amet, consectetur adipiscing elit"
   "1&)\"<$+& 0&-&!}</21\"1 \"0+, <H1\"*}<1&0</,),!<*20-&<*\"/,)"))


(defun enc-test-round-trip (error-prelude)
  ((lambda (f s)
     (when (not (equal s (call f s)))
       (error (concat
                error-prelude
                "error: round-trip encryption test(s) failed"))))

   (lambda (s) ; _ :: string -> string
     (let ((key 313))
       (thread s
         (lambda (chars) (enc-encrypt-chars key chars))
         (lambda (chars) (enc-encrypt-chars (- key) chars))
         'enc-join-chars)))

   "lorem ipsum dolor sit amet, consectetur adipiscing elit"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test execution

(defun test-run-tests (&rest tests)
  (letrec ((prelude "enc-test ... ")
           (execute-tests (lambda (fns)
                            (cond ((null fns) nil)
                                  (t (call (car fns) prelude)
                                     (call execute-tests (cdr fns)))))))
    (message (concat prelude "running tests..."))
    (call execute-tests tests)
    (message (concat prelude "passed all tests"))))


(test-run-tests
 'enc-test-encrypt-char-with-key
 'enc-test-negate-string
 'enc-test-string-encryption
 'enc-test-round-trip)




(provide 'enc-test)
;;; enc-test.el ends here
