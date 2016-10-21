#|
 This file is a part of lichat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.lichat.protocol)

(defun print-sexpr-list (list stream)
  (write-char #\( stream)
  (loop for (item . rest) on list
        do (print-sexpr item stream)
           (when rest (write-char #\  stream)))
  (write-char #\) stream))

(defun print-sexpr-string (string stream)
  (write-char #\" stream)
  (loop for char across string
        do (when (char= char #\")
             (write-char #\\ stream))
           (write-char char stream))
  (write-char #\" stream))

(defun print-sexpr-number (number stream)
  (etypecase number
    (integer (format stream "~d" number))
    (float   (format stream "~f" number))))

(defun print-sexpr-token (token stream)
  (loop for char across token
        do (when (find char "\"():0123456789. #")
             (write-char #\\ stream))
           (write-char (char-downcase char) stream)))

(defun print-sexpr-symbol (sexpr stream)
  (case (symbol-package sexpr)
    (#.(find-package :keyword)
     (write-char #\: stream)
     (print-sexpr-token (symbol-name sexpr) stream))
    (#.*package*
     (print-sexpr-token (symbol-name sexpr) stream))
    ((NIL)
     (write-char #\# stream)
     (write-char #\: stream)
     (print-sexpr-token (symbol-name sexpr) stream))
    (T
     (print-sexpr-token (package-name (symbol-package sexpr)) stream)
     (write-char #\:)
     (print-sexpr-token (symbol-name sexpr) stream))))

(defun print-sexpr (sexpr stream)
  (typecase sexpr
    (list (print-sexpr-list sexpr stream))
    (string (print-sexpr-string sexpr stream))
    (real (print-sexpr-number sexpr stream))
    (symbol (print-sexpr-symbol sexpr stream))
    (T (error 'unprintable-object :object sexpr)))
  sexpr)
