;;; enc.el --- Buffer and region encryption -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Robert Duane Edmonds
;;
;; Author: Duane Edmonds <duane.edmonds@gmail.com>
;; Maintainer: Duane Edmonds <duane.edmonds@gmail.com>
;; Created: August 23, 2023
;; Modified: April 7, 2024
;; Version: 0.0.20
;; Keywords: extensions files data processes tools
;; Homepage: https://github.com/usefulmove/enc
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description: buffer and region encryption
;;
;;; Warning:
;;
;; The encryption method used is based on a basic algorithm, which means:
;;
;; 1. It offers basic obfuscation rather than robust security. It is not suitable
;;    for encrypting sensitive or confidential data.
;;
;; 2. Knowledgeable attackers who are aware of this method can reverse-engineer
;;    the original content, especially if they have multiple encrypted samples.
;;
;; 3. Do NOT rely on this for ensuring data privacy or for compliance with data
;;    protection regulations.
;;
;; This encryption is intended for casual use-cases where the primary goal is to
;; prevent immediate readability, not to provide strong cryptographic protection.
;;
;; If you need a secure encryption method, consider using established
;; cryptographic libraries or packages that implement proven more robust
;; encryption algorithms.
;;
;;; Code:


;; load Othello library
(add-to-list 'load-path "~/repos/othello/src/")
(require 'othello)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; core functions

;; enc-read-buffer :: nil -> string (IMPURE)
(defun enc-read-buffer ()
  "Read the contents of the current buffer."
  (buffer-substring-no-properties
    (point-min)
    (point-max)))


;; enc-read-region :: nil -> string (IMPURE)
(defun enc-read-region ()
  "Read the contents of the current region."
  (buffer-substring
    (region-beginning)
    (region-end)))


;; enc-encrypt-char :: encryption-key -> (char -> char)
(defun enc-encrypt-char (encryption-key)
  "Encrypt character using ENCRYPTION-KEY function decorator. Return key-specific
encryption function."
  (o-memoize
   (lambda (ord)
     (let ((base 32)
           (cap 127))
       (if (or (< ord base) ; ignore characters lower than base or
               (> ord (inc cap))) ord ; higher than cap
           (+ base
              (mod (+ (- ord base)
                      encryption-key)
                   (- cap base))))))))


;; enc-encrypt-string :: encryption-key -> string -> string
(defun enc-encrypt-string (encryption-key s)
  "Encrypt string (S) using ENCRYPTION-KEY."
  (o-thread s
    (lambda (c) (o-map (enc-encrypt-char encryption-key) c))
    'reverse
    'o-join-chars))


;; enc-decrypt-string :: encryption-key -> string -> string
(defun enc-decrypt-string (encryption-key s)
  "Decrypt string (S) using ENCRYPTION-KEY."
  (enc-encrypt-string (- encryption-key) s))


;; enc-string-negate :: string -> string
(defun enc-string-negate (s)
  "Negate value stored as string (S)."
  (let* ((chars (string-to-list s)))
    (o-join-chars (if (equal ?- (car chars))
                      (cdr chars)
                    (cons ?- chars)))))


;; enc-update-buffer :: string -> nil (IMPURE)
(defun enc-update-buffer (s)
  "Replace the contents of the current buffer with S."
  (erase-buffer)
  (insert s))


;; enc-update-region :: string -> nil (IMPURE)
(defun enc-update-region (s)
  "Replace the contents of the current region with S."
  (delete-region
    (region-beginning)
    (region-end))
  (insert s))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; interactive commands

;; enc-encrypt-buffer :: string -> nil (IMPURE)
(defun enc-encrypt-buffer (encryption-key-string)
  "Encrypt contents of current buffer using encryption key (ENCRYPTION-KEY-STRING)."
  (interactive "sEnter encryption key: ")
  (let ((encryption-key (string-to-number encryption-key-string)))
    (if (= 0 encryption-key) (message "error: invalid key (enc)")
        (enc-update-buffer (enc-encrypt-string
                             encryption-key
                             (enc-read-buffer))))))


;; enc-decrypt-buffer :: string -> nil (IMPURE)
(defun enc-decrypt-buffer (encryption-key-string)
  "Decrypt contents of current buffer using encryption key (ENCRYPTION-KEY-STRING)."
  (interactive "sEnter decryption key: ")
  (enc-encrypt-buffer (enc-string-negate encryption-key-string)))


;; enc-encrypt-region :: string -> nil (IMPURE)
(defun enc-encrypt-region (encryption-key-string)
  "Encrypt contents of selected region using encryption key
(ENCRYPTION-KEY-STRING)."
  (interactive "sEnter encryption key: ")
  (let ((encryption-key (string-to-number encryption-key-string)))
    (if (= 0 encryption-key) (message "error: invalid key (enc)")
        (enc-update-region (enc-encrypt-string
                             encryption-key
                             (enc-read-region))))))


;; enc-decrypt-region :: string -> nil (IMPURE)
(defun enc-decrypt-region (encryption-key-string)
  "Decrypt contents of selected region using encryption key
(ENCRYPTION-KEY-STRING)."
  (interactive "sEnter decryption key: ")
  (enc-encrypt-region (enc-string-negate encryption-key-string)))



(provide 'enc)
;;; enc.el ends here
