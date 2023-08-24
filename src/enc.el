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
  (todo))


; encrypt :: [char] -> encryption-key -> [char]
(defun encrypt (cstream encryption-key)
  )


; enc - encrypt buffer contents
(defun enc (encryption-key)
  (interactive)
  (let* ((cstream (read-buffer)) ; read current buffer as character stream
         (encrypted (encrypt cstream encryption-key)))
        ; replace buffer contents with encrypted stream
        (update-buffer encrypted)))

;
; enc-d - decrypt buffer contents
(defun enc-d (encryption-key)
  (interactive)
  (enc (- encryption-key)))


(provide 'enc)
;;; enc.el ends here
