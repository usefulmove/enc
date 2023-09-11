;;; enc.el --- Buffer and region encryption -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Duane Edmonds
;;
;; Author: Duane Edmonds <duane.edmonds@gmail.com>
;; Maintainer: Duane Edmonds <duane.edmonds@gmail.com>
;; Created: August 23, 2023
;; Modified: September 10, 2023
;; Version: 0.0.11
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


; load Cora language
(add-to-list 'load-path "~/repos/cora/src/")
(require 'cora)


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
      (if (or (< ord base) ; ignore characters lower than base or
              (> ord (inc cap))) ord ; higher than cap
          (+ base
             (mod (+ (- ord base)
                     encryption-key)
                  (- cap base)))))))


;; enc-encrypt-chars :: encryption-key -> [char] -> [char]
(defun enc-encrypt-chars (encryption-key chars)
  "Encrypt character stream (CHARS) using ENCRYPTION-KEY."
  (reverse (map (enc-encrypt-char-with-key encryption-key)
                chars)))


;; enc-join-chars :: [char] -> string
(defun enc-join-chars (chars)
  "Join character list (CHARS) into a string."
  (apply 'string chars))


;; enc-string-negate :: string -> string
(defun enc-string-negate (s)
  "Negate value stored as string (S)."
  (let* ((chars (string-to-list s)))
    (enc-join-chars (if (equal ?- (car chars))
                        (cdr chars)
                        (cons ?- chars)))))


;; enc-update-buffer :: string -> nil (IMPURE)
(defun enc-update-buffer (s)
  "Replace the contents of the current buffer with S."
  (delete-region
    (point-min)
    (point-max))
  (insert s))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; interactive commands

;; enc-encrypt-buffer :: string -> nil (IMPURE)
(defun enc-encrypt-buffer (encryption-key-string)
  "Encrypt contents of current buffer using encryption key (ENCRYPTION-KEY-STRING)."
  (interactive "sEnter encryption key: ")
  (let ((encryption-key (string-to-number encryption-key-string)))
    (if (= 0 encryption-key) (message "error: invalid key (enc)")
        ; overwrite buffer with encrypted string
        (enc-update-buffer (thread (enc-read-buffer-contents)
                             (lambda (chars)
                               (enc-encrypt-chars encryption-key chars))
                             'enc-join-chars)))))


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
        (let ((region-contents  (buffer-substring
                                  (region-beginning)
                                  (region-end))))
          ; delete current region and insert encrypted string
          (delete-region
            (region-beginning)
            (region-end))
          (insert (thread region-contents
                    (lambda (chars)
                      (enc-encrypt-chars encryption-key chars))
                    'enc-join-chars))))))


;; enc-decrypt-region :: string -> nil (IMPURE)
(defun enc-decrypt-region (encryption-key-string)
  "Decrypt contents of selected region using encryption key
(ENCRYPTION-KEY-STRING)."
  (interactive "sEnter decryption key: ")
  (enc-encrypt-region (enc-string-negate encryption-key-string)))


(provide 'enc)
;;; enc.el ends here
