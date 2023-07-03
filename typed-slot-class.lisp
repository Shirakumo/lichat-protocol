(in-package #:org.shirakumo.lichat.protocol)

(defvar *unbound-value* (make-symbol "UNBOUND"))

(defun %check-slot-value (value type)
  (labels ((rec (value type)
             (typecase type
               ((cons (eql list))
                (let ((element-type (second type)))
                  (and (listp value)
                       (every (lambda (element)
                                (rec element element-type))
                              value))))
               ((cons (eql or))
                (some (lambda (type) (rec value type)) (rest type)))
               (t
                (typep value type)))))
    (rec value type)))

(defun check-compatible-slot-value (value object slot)
  (cond ((eq value *unbound-value*)
         (unless (eql T (slot-type slot))     
           (cerror "Unbind the slot anyway." 'incompatible-value-type-for-slot
                   :object object :slot (c2mop:slot-definition-name slot) :value *unbound-value* :type (slot-type slot))))
        ((not (%check-slot-value value (slot-type slot)))
         (cerror "Write to the slot anyway." 'incompatible-value-type-for-slot
                 :object object :slot (c2mop:slot-definition-name slot) :value value :type (slot-type slot)))))

(defclass typed-slot (c2mop:standard-slot-definition)
  ((slot-type :initarg :slot-type :initform NIL :accessor slot-type)))

(defmethod print-object ((slot typed-slot) stream)
  (print-unreadable-object (slot stream :type T :identity T)
    (format stream "~s ~s ~s" (c2mop:slot-definition-name slot) :type (slot-type slot))))

(defclass typed-direct-slot-definition (typed-slot c2mop:standard-direct-slot-definition)
  ())

(defclass typed-effective-slot-definition (typed-slot c2mop:standard-effective-slot-definition)
  ())

(defclass typed-slot-class (standard-class)
  ())

(defmethod c2mop:validate-superclass ((class standard-class) (superclass typed-slot-class))
  T)

(defmethod c2mop:validate-superclass ((class typed-slot-class) (superclass standard-class))
  T)

(defmethod c2mop:direct-slot-definition-class ((class typed-slot-class) &key)
  (find-class 'typed-direct-slot-definition))

(defmethod c2mop:effective-slot-definition-class ((class typed-slot-class) &key)
  (find-class 'typed-effective-slot-definition))

(defmethod c2mop:compute-effective-slot-definition ((class typed-slot-class) name direct-slots)
  (declare (ignore name))
  (let ((effective-slot (call-next-method)))
    (loop for direct-slot in direct-slots
          do (when (and (typep direct-slot 'typed-direct-slot-definition)
                        (eql (c2mop:slot-definition-name direct-slot)
                             (c2mop:slot-definition-name effective-slot)))
               (setf (slot-type effective-slot) (slot-type direct-slot))
               (return)))
    effective-slot))

(defmethod (setf c2mop:slot-value-using-class) :before (value (class typed-slot-class) object (slot typed-slot))
  (check-compatible-slot-value value object slot))

(defmethod c2mop:slot-makunbound-using-class :before ((class typed-slot-class) object (slot typed-slot))
  (unless (slot-boundp object (c2mop:slot-definition-name slot))
    (check-compatible-slot-value *unbound-value* object slot)))

(defclass typed-object ()
  ()
  (:metaclass typed-slot-class))

(defmethod shared-initialize :after ((object typed-object) slot-names &key)
  (let ((slots (c2mop:class-slots (class-of object))))
    (flet ((process-slot (slot)
             (when (typep slot 'typed-slot)
               (unless (slot-boundp object (c2mop:slot-definition-name slot))
                 (check-compatible-slot-value *unbound-value* object slot)))))
      (etypecase slot-names
        (list (loop for name in slot-names
                    for slot = (find name slots :key #'c2mop:slot-definition-name)
                    do (process-slot slot)))
        (T    (loop for slot in slots
                    do (process-slot slot)))))))

(defmacro define-typed-class (name direct-superclasses direct-slots &body options)
  `(defclass ,name (,@direct-superclasses typed-object)
     ,direct-slots
     ,@(unless (find :metaclass options :key #'first)
         `((:metaclass typed-slot-class)))
     ,@options))
