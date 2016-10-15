#|
 This file is a part of lichat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.lichat.protocol)

(defun read-sexpr-list (stream)
  (prog1 (loop until (eql #\) (peek-char T stream))
               collect (read-sexpr stream))
    (read-char stream)))

(defun read-sexpr-string (stream)
  (with-output-to-string (out)
    (loop for char = (read-char stream)
          do (case char
               (#\\ (write-char (read-char stream) out))
               (#\" (return))
               (T   (write-char char out))))))

(defun read-sexpr-keyword (stream)
  (find-symbol (read-sexpr-token stream) :keyword))

(defun read-sexpr-number (stream)
  (let ((out (make-string-output-stream))
        (point NIL))
    (loop for i from 0
          for char = (read-char stream NIL)
          do (case char
               ((NIL) (return))
               (#\. (cond (point (unread-char char stream) (return))
                          (T (setf point i))))
               ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
                (write-char char out))
               (T (unread-char char stream) (return))))
    (let* ((number-string (get-output-stream-string out))
           (number (if (string= number-string "") 0 (parse-integer number-string))))
      (if point
          (let ((decimal (- (length number-string) point)))
            (if (= 0 decimal)
                (coerce number 'double-float)
                (coerce (/ number (expt 10 decimal)) 'double-float)))
          number))))

(defun read-sexpr-token (stream)
  (with-output-to-string (out)
    (loop for char = (char-upcase (read-char stream NIL))
          do (case char
               (#\\ (write-char (read-char stream) out))
               ((#\" #\( #\) #\: #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\. #\ ) (unread-char char stream) (return))
               ((NIL) (return))
               (T (write-char char out))))))

(defun read-sexpr-symbol (stream)
  (let ((token (read-sexpr-token stream)))
    (cond ((eql #\: (peek-char NIL stream NIL))
           (read-char stream)
           (if (string= token "#")
               (make-symbol (read-sexpr-token stream))
               (find-symbol (read-sexpr-token stream) (find-package token))))
          (T
           (find-symbol token #.*package*)))))

(defun read-sexpr (stream)
  (let ((char (read-char stream)))
    (case char
      (#\( (read-sexpr-list stream))
      (#\" (read-sexpr-string stream))
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\.)
       (unread-char char stream)
       (read-sexpr-number stream))
      (#\: (read-sexpr-keyword stream))
      (T (unread-char char stream)
       (read-sexpr-symbol stream)))))
