;;; enc.el --- Buffer and region encryption -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Duane Edmonds
;;
;; Author: Duane Edmonds <duane.edmonds@gmail.com>
;; Maintainer: Duane Edmonds <duane.edmonds@gmail.com>
;; Created: August 23, 2023
;; Modified: September 3, 2023
;; Version: 0.0.8
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
;;; Code:

(load-file "~/repos/cora/src/cora.el") ; load Cora language



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; core functions

;; enc-read-buffer-contents :: nil -> string
(defun enc-read-buffer-contents ()
  "Read the contents of the current buffer."
  (buffer-substring-no-properties (point-min) (point-max)))


;; enc-encrypt-char-with-key :: encryption-key -> (char -> char)
(defun enc-encrypt-char-with-key (encryption-key)
  "Encrypt character using ENCRYPTION-KEY function decorator. Return key-specific
encryption function."
  (lambda (ord)
    (let ((base 32)
          (cap 127))
      (cond ((or (< ord base)
                 (> ord (inc cap))) ord) ; only modify characters in range
            (t (+ base
                  (mod (+ (- ord base) encryption-key)
                            (- cap base))))))))


;; enc-encrypt-chars :: encryption-key -> [char] -> [char]
(defun enc-encrypt-chars (encryption-key chars)
  "Encrypt character stream (CHARS) using ENCRYPTION-KEY."
  (let ((encrypt-char (enc-encrypt-char-with-key encryption-key)))
    (reverse (mapcar encrypt-char chars))))


;; enc-join-chars :: [char] -> string
(defun enc-join-chars (chars)
  "Join character list (CHARS) into a string."
  (apply 'string chars))


;; enc-string-negate :: string -> string
(defun enc-string-negate (s)
  "Negate value stored as string (S)."
  (let* ((chars (string-to-list s)))
    (enc-join-chars (cond ((equal ?- (car chars)) (cdr chars))
                          (t (cons ?- chars))))))


;; enc-update-buffer :: string -> nil (IMPURE)
(defun enc-update-buffer (s)
  "Replace the contents of the current buffer with S."
  (delete-region (point-min) (point-max))
  (insert s))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; interactive commands

;; enc-encrypt-buffer :: string -> nil (IMPURE)
(defun enc-encrypt-buffer (encryption-key-string)
  "Encrypt contents of current buffer using encryption key (ENCRYPTION-KEY-STRING)."
  (interactive "sEnter encryption key: ")
  (let ((encryption-key (string-to-number encryption-key-string)))
    (cond ((= 0 encryption-key) (message "error: invalid key (enc)"))
          (t (let ((encrypted-string (thread (enc-read-buffer-contents)
                                       (lambda (chars)
                                          (enc-encrypt-chars encryption-key chars))
                                       'enc-join-chars)))

               (enc-update-buffer encrypted-string) ; overwrite buffer
               (message "enc: buffer encrypted/decrypted"))))))



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
    (cond ((= 0 encryption-key) (message "error: invalid key (enc)"))
          (t (let ((encrypted-string (thread (buffer-substring
                                               (region-beginning)
                                               (region-end))
                                       (lambda (chars)
                                         (enc-encrypt-chars encryption-key chars))
                                       'enc-join-chars)))

                 (delete-region (region-beginning) (region-end)) ; delete region
                 (insert encrypted-string) ; insert encrypted string
                 (message "enc: region encrypted/decrypted"))))))



;; enc-decrypt-region :: string -> nil (IMPURE)
(defun enc-decrypt-region (encryption-key-string)
  "Decrypt contents of selected region using encryption key
(ENCRYPTION-KEY-STRING)."
  (interactive "sEnter decryption key: ")
  (enc-encrypt-region (enc-string-negate encryption-key-string)))



(provide 'enc)
;;; enc.el ends here
