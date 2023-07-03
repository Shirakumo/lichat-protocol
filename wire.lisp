(in-package #:org.shirakumo.lichat.protocol)

(defun to-wire (wireable stream)
  (etypecase wireable
    (object
     (print-sexpr `(,(class-name (class-of wireable))
                     ,@(loop for slot in (c2mop:class-slots (class-of wireable))
                             for initarg = (first (c2mop:slot-definition-initargs slot))
                             when initarg collect initarg
                             when initarg collect (slot-value wireable (c2mop:slot-definition-name slot))))
                  stream))
    (wireable
     (print-sexpr wireable stream)))
  (write-char #\Nul stream)
  (force-output stream))

(defun check-update-options (sexpr)
  (let (id-found clock-found)
    (loop for (key val) on (rest sexpr) by #'cddr
          do (unless (eq key *invalid-symbol*)
               (unless (typep key 'keyword)
                 (error 'malformed-wire-object :update sexpr))
               (case key
                 (:id (setf id-found T))
                 (:clock (setf clock-found T)))))
    (unless id-found
      (error 'missing-id :update sexpr))
    (unless clock-found
      (error 'missing-clock :update sexpr))))

(defun from-wire (stream &optional limit)
  (let* ((*read-counter* 0)
         (*read-limit* limit)
         (sexpr (read-sexpr stream)))
    (unwind-protect
         (typecase sexpr
           (cons
            (unless (typep (first sexpr) 'symbol)
              (error 'malformed-wire-object :update sexpr))
            (let ((class (find-class (first sexpr) NIL)))
              (unless class (error 'unknown-wire-object :update sexpr))
              (cond ((c2mop:subclassp class (find-class 'update))
                     (check-update-options sexpr)
                     (apply #'make-instance (first sexpr) :allow-other-keys T (rest sexpr)))
                    ((c2mop:subclassp class (find-class 'object))
                     (apply #'make-instance (first sexpr) :allow-other-keys T (rest sexpr)))
                    (T
                     (error 'unknown-wire-object :update sexpr)))))
           (T
            sexpr))
      (loop for char = (read-char stream NIL)
            until (or (not char) (char= #\Nul char))))))

(defun from-wire* (stream &optional limit)
  (handler-case
      (handler-bind ((unknown-symbol #'continue))
        (from-wire stream limit))
    (stray-null-found ()
      NIL)
    (reader-condition ()
      (skip-to-null stream)
      NIL)))
