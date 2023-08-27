;;; enc.el --- Buffer and region encryption -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Duane Edmonds
;;
;; Author: Duane Edmonds <duane.edmonds@gmail.com>
;; Maintainer: Duane Edmonds <duane.edmonds@gmail.com>
;; Created: August 23, 2023
;; Modified: August 26, 2023
;; Version: 0.0.3
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; core functions

;; enc-read-buffer-contents :: nil -> string
(defun enc-read-buffer-contents ()
  "Read the contents of the current buffer."
  (buffer-substring-no-properties (point-min) (point-max)))


;; enc-encrypt-char-with-key :: encryption-key -> (char -> char)
(defun enc-encrypt-char-with-key (encryption-key)
  "Key-specific single-character encryption - function decorator."
  (lambda (ord)
    ((lambda (base cap)
      (cond ((or (< ord base)
                 (> ord (- cap 1))) ord) ; only modify characters in range
            (t (+ base (mod (+ (- ord base) encryption-key)
                            (- cap base)))))) 32 127)))


;; enc-encrypt-chars :: encryption-key -> [char] -> [char]
(defun enc-encrypt-chars (encryption-key chars)
  "Encrypt character stream."
  (let ((encrypt-char (enc-encrypt-char-with-key encryption-key)))
    (reverse (mapcar encrypt-char chars))))


;; enc-update-buffer :: string -> nil (impure)
(defun enc-update-buffer (s)
  "Replace the contents of the current buffer with character stream."
  (delete-region (point-min) (point-max))
  (insert s))


;; enc-join-chars :: [char] -> string
(defun enc-join-chars (chars)
  (apply 'string chars))


;; enc-string-negate :: string -> string
(defun enc-string-negate (s)
  "Negate value stored as string."
  (let* ((chars (string-to-list s)))
    (enc-join-chars (cond ((equal ?- (car chars)) (cdr chars))
                          (t (cons ?- chars))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; interactive commands

;; enc-encrypt-buffer :: string -> nil (impure)
(defun enc-encrypt-buffer (encryption-key-string)
  "Encrypt contents of current buffer."
  (interactive "sEnter encryption key: ")
  (let* ((chars (string-to-list (enc-read-buffer-contents)))
         (encryption-key (string-to-number encryption-key-string))
         (encrypted-chars (enc-encrypt-chars encryption-key chars)))
    ; replace buffer contents with encrypted character stream
    (cond ((= 0 encryption-key) (message "error: invalid key (enc)"))
          (t (enc-update-buffer (enc-join-chars encrypted-chars))))))


;; enc-decrypt-buffer :: string -> nil (impure)
(defun enc-decrypt-buffer (encryption-key-string)
  "Decrypt contents of current buffer."
  (interactive "sEnter decryption key: ")
  (enc-encrypt-buffer (enc-string-negate encryption-key-string)))



;; enc-encrypt-region :: string -> nil (impure)
(defun enc-encrypt-region (encryption-key-string)
  "Encrypt contents of selected region."
  (interactive "sEnter encryption key: ")
    (let* ((chars (string-to-list (buffer-substring
                                    (region-beginning)
                                    (region-end))))
           (encryption-key (string-to-number encryption-key-string))
           (encrypted-chars (enc-encrypt-chars encryption-key chars)))
      (cond ((= 0 encryption-key) (message "error: invalid key (enc)"))
            (t (progn
                 (delete-region (region-beginning) (region-end))
                 (insert (enc-join-chars encrypted-chars)))))))


;; enc-decrypt-region :: string -> nil (impure)
(defun enc-decrypt-region (encryption-key-string)
  "Decrypt contents of selected region."
  (interactive "sEnter decryption key: ")
  (enc-encrypt-region (enc-string-negate encryption-key-string)))



(provide 'enc)
;;; enc.el ends here
