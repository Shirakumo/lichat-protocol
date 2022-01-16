#|
sbcl --noinform --load "$0" --eval '(generate-all)' --quit && exit
|#

#+quicklisp (ql:quickload '(cl-markless-plump lass lquery) :silent T)

(defvar *here* #.(or *compile-file-pathname*
                     *load-pathname*
                     (error "LOAD this file.")))

(defun file (name type)
  (make-pathname :name name :type type :defaults *here*))

(defun style ()
  (lass:compile-and-write
   '(article
     :max-width 1000px
     :font-size 14pt
     :font-family sans-serif
     :margin 3em auto
     (h1
      :text-align center
      :font-size 2em)
     (img
      :margin 0 auto
      :max-width 100%)
     (blockquote
      :border-left 0.2em solid gray
      :margin-left 1em
      :padding-left 1em)
     (figcaption
      :padding 0.2em 1em
      :background (hex E0E0E0))
     (code
      :background (hex F0F0F0)
      :padding 0 0.1em)
     (.code-block
      :padding 0.1em 0.5em
      :overflow-x auto))))

(defun suffix-p (suffix string)
  (and (<= (length suffix) (length string))
       (string= string suffix :start1 (- (length string) (length suffix)))))

(defun fixup-href (node)
  (let ((href (plump:attribute node "href")))
    (when (suffix-p ".mess" href)
      (setf (plump:attribute node "href") (format NIL "~a.html" (subseq href 0 (- (length href) (length ".mess"))))))
    node))

(defun generate-documentation (file)
  (let ((dom (plump:make-root)))
    (cl-markless:output (cl-markless:parse file (make-instance 'cl-markless:parser))
                        :target dom
                        :format (make-instance 'org.shirakumo.markless.plump:plump
                                               :css (style)))
    (lquery:$ dom "a[href]" (each #'fixup-href))
    (with-open-file (stream (make-pathname :type "html" :defaults file)
                            :direction :output
                            :if-exists :supersede)
      (plump:serialize dom stream))))

(defun generate-all ()
  (dolist (file (directory (file :wild "mess")))
    (with-simple-restart (continue "Ignore ~a" file)
      (generate-documentation file))))
