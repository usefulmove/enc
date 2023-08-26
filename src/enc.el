;;; enc.el --- Buffer encryption -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Duane Edmonds
;;
;; Author: Duane Edmonds <duane.edmonds@gmail.com>
;; Maintainer: Duane Edmonds <duane.edmonds@gmail.com>
;; Created: August 23, 2023
;; Modified: August 26, 2023
;; Version: 0.0.2
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; core functions

;; enc-read-buffer-contents :: nil -> string
(defun enc-read-buffer-contents ()
  "Read the contents of the current buffer."
  (buffer-substring-no-properties (point-min) (point-max)))

;; enc-encrypt-char-with-key :: encryption-key -> (char -> char)
(defun enc-encrypt-char-with-key (encryption-key)
  "Key-specific character encryption function decorator."
  (lambda (ord)
    (let* ((base 32)
           (cap 127)
           (range (- cap base)))
      (cond ((or (< ord base)
                 (> ord (- cap 1))) ord) ; only modify characters in range
            (t (+ base (mod (+ (- ord base) encryption-key)
                            range)))))))

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


;; enc-list-to-string :: [char] -> string
(defun enc-list-to-string (lst)
  "Join characters in list into a concatenated string."
  (apply 'concat (mapcar 'char-to-string lst)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; interactive commands

;; enc-encrypt-buffer :: string -> nil (impure)
(defun enc-encrypt-buffer (encryption-key-string)
  "Encrypt contents of current buffer."
  (interactive "sEnter encryption key: ")
  (let* ((chars (string-to-list (enc-read-buffer-contents)))
         (encrypted-chars (enc-encrypt-chars (string-to-number encryption-key-string)
                                             chars)))
    ; replace buffer contents with encrypted character stream
    (enc-update-buffer (enc-list-to-string encrypted-chars))))

;;
;; enc-decrypt-buffer :: string -> nil (impure)
(defun enc-decrypt-buffer (encryption-key-string)
  "Decrypt contents of current buffer."
  (interactive "sEnter encryption key: ")
  (enc-encrypt-buffer (number-to-string (- (string-to-number encryption-key-string)))))



;; enc-encrypt-region :: string -> nil (impure)
(defun enc-encrypt-region (encryption-key-string)
  "Encrypt contents of selected region."
  (interactive "sEnter encryption key: ")
    (let* ((chars (string-to-list (buffer-substring (region-beginning) (region-end))))
           (encrypted-chars (enc-encrypt-chars (string-to-number encryption-key-string)
                                               chars)))
      (delete-region (region-beginning) (region-end))
      (insert (enc-list-to-string encrypted-chars))))


;; enc-decrypt-region :: string -> nil (impure)
(defun enc-decrypt-region (encryption-key-string)
  "Decrypt contents of selected region."
  (interactive "sEnter encryption key: ")
  (enc-encrypt-region (number-to-string (- (string-to-number encryption-key-string)))))



(provide 'enc)
;;; enc.el ends here
