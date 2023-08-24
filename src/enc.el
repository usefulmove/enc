;;; enc.el --- emacs buffer encryption -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2023 Robert Duane Edmonds
;;
;; Author: Duane Edmonds <dedmonds@gmail.com>
;; Maintainer: Duane Edmonds <dedmonds@gmail.com>
;; Created: August 23, 2023
;; Modified: August 24, 2023
;; Version: 0.0.1
;; Keywords: buffer encryption decryption
;; Homepage: https://github.com/dedmonds/enc
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description: emacs buffer encryption
;;
;;; Code:

; read-buffer-contents :: nil -> string
(defun read-buffer-contents ()
  "read the contents of the current buffer"
  (buffer-substring-no-properties (point-min) (point-max)))

; encrypt :: encryption-key -> [char] -> [char]
(defun encrypt (encryption-key cstream) ; here
  "encrypt character stream"
  (reverse cstream)) ; TODO - replace with full implementation

; update-buffer :: string -> nil (impure)
(defun update-buffer (s)
  "replace the contents of the current buffer with character stream"
  s) ; TODO - implement me

; list-to-string :: [char] -> string
(defun list-to-string (lst)
  "join characters in list into a concatenated string"
  (apply 'concat (mapcar 'char-to-string lst)))



; enc-encrypt :: interactive command
(defun enc-encrypt (encryption-key)
  "encrypt buffer contents"
  (interactive)
  (let* ((cstream (string-to-list (read-buffer-contents))) ; read current buffer as character stream
         (encrypted (encrypt encryption-key cstream)))
    ; replace buffer contents with encrypted stream
    (update-buffer (list-to-string encrypted))))

; enc-decrypt :: interactive command
(defun enc-decrypt (encryption-key)
  "decrypt buffer contents"
  (interactive)
  (enc-encrypt (- encryption-key)))



(provide 'enc)
;;; enc.el ends here
