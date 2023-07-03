(in-package #:org.shirakumo.lichat.protocol)

(define-from-protocol-file "spec/lichat.sexpr")
(define-from-protocol-file "spec/shirakumo.sexpr")

;; Define base symbols of the channel/user--info extension
:news :topic :rules :contact :url
:birthday :contact :location :public-key :real-name :status

(defmethod print-object ((update update) stream)
  (print-unreadable-object (update stream :type T)
    (format stream "~s ~a ~s ~a" :from (maybe-sval update 'from)
                                 :id (maybe-sval update 'id))))

(defmethod print-object ((update channel-update) stream)
  (print-unreadable-object (update stream :type T)
    (format stream "~s ~a ~s ~a ~s ~a"
            :from (maybe-sval update 'from)
            :channel (maybe-sval update 'channel)
            :id (maybe-sval update 'id))))

(defmethod print-object ((update target-update) stream)
  (print-unreadable-object (update stream :type T)
    (format stream "~s ~a ~s ~a ~s ~a"
            :from (maybe-sval update 'from)
            :target (maybe-sval update 'target)
            :id (maybe-sval update 'id))))

(defmethod print-object ((update text-update) stream)
  (print-unreadable-object (update stream :type T)
    (format stream "~s ~a ~s ~a ~s ~s" :from (maybe-sval update 'from)
                                       :id (maybe-sval update 'id)
                                       :text (maybe-sval update 'text))))

(defmethod print-object ((update update-failure) stream)
  (print-unreadable-object (update stream :type T)
    (format stream "~s ~a ~s ~a ~s ~a" :from (maybe-sval update 'from)
                                       :id (maybe-sval update 'id)
                                       :update-id (maybe-sval update 'update-id))))
