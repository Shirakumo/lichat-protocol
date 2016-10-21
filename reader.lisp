#|
 This file is a part of lichat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.lichat.protocol)

(defvar *whitespace*
  #+asdf-unicode (map 'vector #'code-char '(#x0009 #x000A #x000B #x000C #x000D #x0020
                                            #x0085 #x00A0 #x1680 #x2000 #x2001 #x2002
                                            #x2003 #x2004 #x2005 #x2006 #x2008 #x2009
                                            #x200A #x2028 #x2029 #x202F #x205F #x3000
                                            #x180E #x200B #x200C #x200D #x2060 #xFEFF))
  #-asdf-unicode (map 'vector #'code-char '(#x0009 #x000A #x000B #x000C #x000D #x0020)))

(defun whitespace-p (char)
  (find char *whitespace*))

(defun skip-whitespace (stream)
  (loop for char = (read-char stream)
        while (find char *whitespace*)
        finally (unread-char char stream)))

(defun safe-find-symbol (name package)
  (let ((package (find-package package)))
    (or (find-symbol name package)
        (error 'unknown-symbol :symbol-designator (cons (package-name package) name)))))

(defun read-sexpr-list (stream)
  (prog1 (loop do (skip-whitespace stream)
               until (eql #\) (peek-char T stream))
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
  (safe-find-symbol (read-sexpr-token stream) :keyword))

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
  (peek-char NIL stream)
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
               (safe-find-symbol (read-sexpr-token stream) (find-package token))))
          (T
           (safe-find-symbol token #.*package*)))))

(defun read-sexpr (stream)
  (skip-whitespace stream)
  (let ((char (read-char stream)))
    (handler-bind
        ((end-of-file (lambda (err)
                        (declare (ignore err))
                        (error 'incomplete-token))))
      (case char
        (#\( (read-sexpr-list stream))
        (#\" (read-sexpr-string stream))
        ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\.)
         (unread-char char stream)
         (read-sexpr-number stream))
        (#\: (read-sexpr-keyword stream))
        (T (unread-char char stream)
         (read-sexpr-symbol stream))))))
