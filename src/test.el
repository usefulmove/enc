;;; test.el --- Unit tests for enc encryption package -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Duane Edmonds
;;
;; Author: Duane Edmonds <duane.edmonds@gmail.com>
;; Maintainer: Duane Edmonds <duane.edmonds@gmail.com>
;; Created: August 26, 2023
;; Modified: August 27, 2023
;; Version: 0.0.3
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

(load-file (concat (file-name-directory load-file-name) ; load enc.el (from same directory)
                   "enc.el"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helper functions

;; fold :: (U -> T -> U) -> U -> [T] -> U
(defun fold (f acc lst)
  (cond ((null lst) acc)
        (t (fold f (funcall f acc (car lst)) (cdr lst)))))


;; partial :: (... -> T -> U) -> [...] -> (T -> U)
(defun partial (&rest args)
  "Return unary function when passed an nary function and (- n 1) arguments."
  (let ((f (car args))
        (fargs (cdr args)))
    (lambda (a)
      (apply f (append fargs (list a))))))


;; thread :: T -> [(T -> T)] -> T
(defun thread (&rest args)
  ;(message (concat (car args) (apply 'concat (cdr args)))))
  (let ((seed (car args))
        (fns (cdr args)))
    (fold
      (lambda (acc f)
        (funcall f acc))
      seed
      fns)))



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
       (thread s
         'string-to-list
         (partial 'enc-encrypt-chars key)
         (partial 'enc-encrypt-chars (- key))
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
