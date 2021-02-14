(defpackage #:com.inuoe.lob/spawn
  (:use #:cl)
  (:import-from #:uiop)
  (:local-nicknames
   (#:things #:com.inuoe.lob/things))
  (:export #:spawn
           #:write-load-forms))
(in-package #:com.inuoe.lob/spawn)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun read-file-into-string (pathname)
    (with-output-to-string (stream)
      (with-open-file (file pathname)
        (loop :for line := (read-line file nil)
              :while line
              :do (write-line line stream))))))

(defparameter +uiop-src+ #.(read-file-into-string
                            (uiop:merge-pathnames* (make-pathname :name "uiop" :type "lisp")
                                                   (uiop:pathname-directory-pathname (or *compile-file-truename* *load-truename* *default-pathname-defaults*)))))

(defparameter +asdf-src+ #.(read-file-into-string
                            (uiop:merge-pathnames* (make-pathname :name "asdf" :type "lisp")
                                                   (uiop:pathname-directory-pathname (or *compile-file-truename* *load-truename* *default-pathname-defaults*)))))

(defgeneric thing-preloader (thing)
  (:method ((thing things:lisp-file))
    nil)
  (:method ((thing things:asd-file))
    (list
     (format nil "(asdf:load-asd ~S)" (things:file-pathname thing))))
  (:method ((thing things:system-name))
    nil))

(defgeneric thing-loader (thing)
  (:method ((thing things:lisp-file))
    (let ((path (things:file-pathname thing)))
      (list
       (format nil "(progn
      (mapc #'asdf:load-system (ignore-errors (asdf/package-inferred-system::package-inferred-system-file-dependencies ~S)))
      (let ((*features* (cons :lob *features*)))
        (load ~S)))"
               path
               path))))
  (:method ((thing things:asd-file))
    (let ((name (pathname-name (slot-value thing 'things:path))))
      (list
       (format nil "(progn
      (mapc #'asdf:load-system (asdf:component-sideway-dependencies (asdf:find-system ~S)))
      (let ((*features* (cons :lob *features*)))
        (asdf:load-system ~S)))"
               name
               name))))
  (:method ((thing things:system-name))
    (list
     (format nil "(asdf:load-system ~S)"
             (let ((name (slot-value thing 'things:name)))
               (etypecase name
                   (string name)
                   (symbol (string-downcase name))))))))

(defun write-load-forms (stream &key loaded-things additional-source-registry)
  (setf loaded-things (mapcar #'things:resolve-thing (if (listp loaded-things) loaded-things (list loaded-things)))
        additional-source-registry (mapcar #'truename (mapcar #'uiop:ensure-directory-pathname additional-source-registry)))

  ;; Load UIOP & ASDF
  (write-string +uiop-src+ stream)
  (terpri stream)

  (write-string +asdf-src+ stream)
  (terpri stream)

  ;; Set up the source registry
  (when additional-source-registry
    (format stream "
(asdf:initialize-source-registry
  '(:source-registry
    :inherit-configuration~{
    (:tree ~S)~}))
"
            additional-source-registry))

  ;; Pre-load any deps
  (let ((pre-load-forms (mapcan #'thing-preloader loaded-things)))
    (when pre-load-forms
      (format stream "
(handler-case
  (progn~{
    ~A~})
  (error (error)
    (format *error-output* \"lob: error while pre-loading:~~%  ~~A~~%\" error)
    (sb-ext:exit :code 1)))
"
              pre-load-forms)))

  ;; Load each thing
  (let ((load-forms (mapcan #'thing-loader loaded-things)))
    (when load-forms
      (format stream "
(handler-case
  (progn~{
    ~A~})
  (error (error)
    (format *error-output* \"lob: error while loading:~~%  ~~A~~%\" error)
    (sb-ext:exit :code 1)))
"
              load-forms)))
  (finish-output stream))


(defun spawn (&key image core)
  (setf image (etypecase image
                (string (uiop:parse-native-namestring image))
                (pathname image)
                (null sb-ext:*runtime-pathname*))
        core (etypecase core
               (string (uiop:parse-native-namestring core))
               (pathname core)
               (null nil)))

  (sb-ext:run-program image
                      (delete nil (list
                                   (when core "--core")
                                   (when core (namestring core))
                                   "--noinform"
                                   "--disable-ldb"
                                   "--lose-on-corruption"
                                   "--end-runtime-options"
                                   "--no-sysinit"
                                   "--no-userinit"
                                   "--noprint"
                                   "--disable-debugger"))
                      :search t
                      :input :stream
                      :output t
                      :error t
                      :wait nil))
