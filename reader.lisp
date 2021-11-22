#|
 This file is a part of lichat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.lichat.protocol)

(defvar *whitespace* (map 'vector #'code-char '(#x0009 #x000A #x000B #x000C #x000D #x0020)))
(defvar *errors* NIL)
(defvar *invalid-symbol* (make-symbol "INVALID-SYMBOL"))
(defvar *read-limit* NIL)
(defvar *read-counter*)

(defun lread (stream &optional eof)
  (when *read-limit*
    (when (<= *read-limit* *read-counter*)
      (error 'read-limit-hit))
    (incf *read-counter*))
  (read-char stream (not eof) eof))

(defun lpeek (stream)
  (when *read-limit*
    (when (<= *read-limit* *read-counter*)
      (error 'read-limit-hit)))
  (peek-char NIL stream))

(defun lunread (char stream)
  (when *read-limit*
    (decf *read-counter*))
  (unread-char char stream))

(defun whitespace-p (char)
  (find char *whitespace*))

(defun skip-whitespace (stream)
  (loop for char = (lread stream)
        while (find char *whitespace*)
        finally (lunread char stream)))

(defun safe-find-symbol (name package)
  (let ((package (find-package package)))
    (if (string= name "NIL")
        NIL
        (or (find-symbol name package)
            (progn (push (make-condition 'unknown-symbol :symbol-designator (cons (package-name package) name)) *errors*)
                   *invalid-symbol*)))))

(defun read-sexpr-list (stream)
  (prog1 (loop do (skip-whitespace stream)
               until (eql #\) (lpeek stream))
               collect (read-sexpr stream))
    (lread stream)))

(defun read-sexpr-string (stream)
  (with-output-to-string (out)
    (loop for char = (lread stream)
          do (case char
               (#\Nul (error 'stray-null-found))
               (#\\ (write-char (lread stream) out))
               (#\" (return))
               (T   (write-char char out))))))

(defun read-sexpr-keyword (stream)
  (safe-find-symbol (read-sexpr-token stream) :keyword))

(defun read-sexpr-number (stream)
  (let ((out (make-string-output-stream))
        (point NIL))
    (loop for i from 0
          for char = (lread stream NIL)
          do (case char
               ((NIL) (return))
               (#\. (cond (point (lunread char stream) (return))
                          (T (setf point i))))
               ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
                (write-char char out))
               (T (lunread char stream) (return))))
    (let* ((number-string (get-output-stream-string out))
           (number (if (string= number-string "") 0 (parse-integer number-string))))
      (if point
          (let ((decimal (- (length number-string) point)))
            (if (= 0 decimal)
                (coerce number 'double-float)
                (coerce (/ number (expt 10 decimal)) 'double-float)))
          number))))

(defun read-sexpr-token (stream)
  (lpeek stream)
  (with-output-to-string (out)
    (loop for char = (lread stream NIL)
          do (case char
               (#\\ (write-char (lread stream) out))
               ((#\" #\( #\) #\: #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\. #\  #\Nul) (lunread char stream) (return))
               ((NIL) (return))
               (T (write-char (char-upcase char) out))))))

(defun read-sexpr-symbol (stream)
  (let ((token (read-sexpr-token stream)))
    (cond ((eql #\: (lpeek stream))
           (lread stream)
           (if (string= token "#")
               (make-symbol (read-sexpr-token stream))
               (safe-find-symbol (read-sexpr-token stream) (find-package token))))
          (T
           (safe-find-symbol token #.*package*)))))

(defun skip-to-null (stream)
  (loop until (char= #\Null (read-char stream NIL #\Null))))

(defun read-sexpr (stream)
  (let* ((char (lread stream))
         (*errors* NIL)
         (sexpr (handler-bind
                    ((end-of-file (lambda (err)
                                    (declare (ignore err))
                                    (error 'incomplete-token))))
                  (case char
                    (#\Nul (error 'stray-null-found))
                    (#\( (read-sexpr-list stream))
                    (#\) (error 'incomplete-token))
                    (#\" (read-sexpr-string stream))
                    ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\.)
                     (lunread char stream)
                     (read-sexpr-number stream))
                    (#\: (read-sexpr-keyword stream))
                    (T (lunread char stream)
                     (read-sexpr-symbol stream))))))
    (when *errors*
      (dolist (err *errors*)
        (cerror "Ignore reader error." err)))
    sexpr))
