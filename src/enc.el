;;; enc.el --- Buffer encryption -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2023 Robert Duane Edmonds
;;
;; Author: Duane Edmonds <dedmonds@gmail.com>
;; Maintainer: Duane Edmonds <dedmonds@gmail.com>
;; Created: August 23, 2023
;; Modified: August 24, 2023
;; Version: 0.0.1
;; Keywords: extensions files data processes tools
;; Homepage: https://github.com/dedmonds/enc
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description: buffer encryption
;;
;;; Code:


; enc-read-buffer-contents :: nil -> string
(defun enc-read-buffer-contents ()
  "read the contents of the current buffer"
  (buffer-substring-no-properties (point-min) (point-max)))

; enc-encrypt-char-with-key :: encryption-key -> (char -> char)
(defun enc-encrypt-char-with-key (encryption-key)
  "key-specific character encryption function decorator"
  (lambda (ord)
    (let* ((base 32)
           (cap 127)
           (range (- cap base)))
      (cond ((or (< ord base) (> ord (- cap 1))) ord) ; only modify characters in range
            (t (+ base (mod (+ (- ord base) encryption-key) range)))))))

; enc-encrypt-cstream :: encryption-key -> [char] -> [char]
(defun enc-encrypt-cstream (encryption-key cstream)
  "encrypt character stream"
  (let ((encrypt-char (enc-encrypt-char-with-key encryption-key)))
    (reverse (mapcar encrypt-char cstream))))

; enc-update-buffer :: string -> nil (impure)
(defun enc-update-buffer (s)
  "replace the contents of the current buffer with character stream"
  (delete-region (point-min) (point-max))
  (insert s))

; enc-list-to-string :: [char] -> string
(defun enc-list-to-string (lst)
  "join characters in list into a concatenated string"
  (apply 'concat (mapcar 'char-to-string lst)))



; (interactive command)
; enc-encrypt :: string -> nil (impure)
(defun enc-encrypt (encryption-key)
  "Encrypt buffer contents."
  (interactive "sEnter encryption key: ")
  (let* ((cstream (string-to-list (enc-read-buffer-contents)))
         (encrypted (enc-encrypt-cstream (string-to-number encryption-key) cstream)))
    ; replace buffer contents with encrypted stream
    (enc-update-buffer (enc-list-to-string encrypted))))

; (interactive command)
; enc-decrypt :: string -> nil (impure)
(defun enc-decrypt (encryption-key)
  "Decrypt buffer contents."
  (interactive "sEnter encryption key: ")
  (enc-encrypt (number-to-string (- (string-to-number encryption-key)))))



(provide 'enc)
;;; enc.el ends here
