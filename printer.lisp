#|
 This file is a part of lichat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.lichat.protocol)

(defun print-sexpr-list (list stream)
  (write-char #\( stream)
  (unwind-protect
       (loop for (item . rest) on list
             do (print-sexpr item stream)
                (when rest (write-char #\  stream)))
    (write-char #\) stream)))

(defun print-sexpr-string (string stream)
  (write-char #\" stream)
  (unwind-protect
       (loop for char across string
             unless (char= char (code-char 0))
             do (when (or (char= char #\") (char= char #\\))
                  (write-char #\\ stream))
                (write-char char stream))
    (write-char #\" stream)))

(defun print-sexpr-number (number stream)
  (etypecase number
    (integer (format stream "~d" number))
    (float   (format stream "~f" number))))

(defun print-sexpr-token (token stream symbol)
  (loop for char across token
        do (when (char= char (code-char 0))
             (error 'null-in-symbol-designator :symbol-designator (cons (package-name (symbol-package symbol))
                                                                        (symbol-name symbol))))
           (when (find char "\\\"():0123456789. #")
             (write-char #\\ stream))
           (write-char (char-downcase char) stream)))

(defun print-sexpr-symbol (sexpr stream)
  (case (symbol-package sexpr)
    (#.(find-package :keyword)
     (write-char #\: stream))
    (#.*package*)
    ((NIL)
     (write-char #\# stream)
     (write-char #\: stream))
    (T
     (print-sexpr-token (package-name (symbol-package sexpr)) stream sexpr)
     (write-char #\: stream)))
  (print-sexpr-token (symbol-name sexpr) stream sexpr))

(defun print-sexpr (sexpr stream)
  (typecase sexpr
    (null (write-string "NIL" stream))
    (cons (print-sexpr-list sexpr stream))
    (string (print-sexpr-string sexpr stream))
    (real (print-sexpr-number sexpr stream))
    (symbol (print-sexpr-symbol sexpr stream))
    (T (error 'unprintable-object :object sexpr)))
  sexpr)
