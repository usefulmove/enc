;;; enc.el --- emacs buffer encryption -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2023 Robert Duane Edmonds
;;
;; Author: Duane Edmonds <dedmonds@gmail.com>
;; Maintainer: Duane Edmonds <dedmonds@gmail.com>
;; Created: August 23, 2023
;; Modified: August 23, 2023
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


; update-buffer
(defun update-buffer (cstream)
  "replace the contents of the current buffer with character stream"
  (TODO))


; encrypt :: encryption-key -> [char] -> [char]
(defun encrypt (encryption-key cstream) ; here
  "encrypt character stream"
  (reverse cstream))

(encrypt 8 '(3 1 2))



; enc :: interactive command
(defun enc (encryption-key)
  "encrypt buffer contents"
  (interactive)
  (let* ((cstream (read-buffer-contents)) ; read current buffer as character stream
         (encrypted (encrypt encryption-key cstream)))
        ; replace buffer contents with encrypted stream
        (update-buffer encrypted)))

;
; enc-d :: interactive command
(defun enc-d (encryption-key)
  "decrypt buffer contents"
  (interactive)
  (enc (- encryption-key)))


(provide 'enc)
;;; enc.el ends here
