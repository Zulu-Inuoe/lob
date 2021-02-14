(defpackage #:com.inuoe.lob/things
  (:use #:cl)
  (:export
   #:lisp-file
   #:path
   #:file-pathname

   #:asd-file
   #:path

   #:system-name
   #:name

   #:resolve-thing))
(in-package #:com.inuoe.lob/things)

(defclass file ()
  ((path
    :type pathname
    :initarg :path)
   (relative-to
    :type pathname
    :initarg :relative-to
    :initform (uiop:pathname-directory-pathname *default-pathname-defaults*))))

(defun file-pathname (file)
  (uiop:merge-pathnames* (slot-value file 'path) (slot-value file 'relative-to)))

(defclass lisp-file (file)
  ())

(defclass asd-file (file)
  ())

(defclass system-name ()
  ((name
    :initarg :name)))

(defun resolve-thing (thing)
  (etypecase thing
    (string (resolve-thing (uiop:parse-native-namestring thing)))
    (pathname
     (cond
       ((string-equal (pathname-type thing) "lisp")
        (make-instance 'lisp-file :path thing))
       ((string-equal (pathname-type thing) "asd")
        (make-instance 'asd-file :path thing))))
    (symbol
     (make-instance 'system-name :name thing))
    ((or lisp-file asd-file system-name) thing)))
