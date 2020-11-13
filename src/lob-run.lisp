(defpackage #:com.inuoe.lob-run
  (:use #:cl)
  (:export
   #:*lob-stdout*
   #:*lob-stderr*

   #:lisp-file
   #:path

   #:asd-file
   #:path

   #:system-name
   #:name

   #:run))

(in-package #:com.inuoe.lob-run)

(defvar *lob-stdout* (make-synonym-stream '*standard-output*))
(defvar *lob-stderr* (make-synonym-stream '*error-output*))

(defclass lisp-file ()
  ((path
    :initarg :path)))

(defclass asd-file ()
  ((path
    :initarg :path)))

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

(defgeneric prettify-thing (thing)
  (:method ((thing lisp-file))
    (format nil "~A" (slot-value thing 'path)))
  (:method ((thing asd-file))
    (format nil "~A" (slot-value thing 'path)))
  (:method ((thing system-name))
    (format nil "~A" (slot-value thing 'name))))

(defgeneric thing-preloader (thing)
  (:method ((thing lisp-file))
    nil)
  (:method ((thing asd-file))
    (list
     (format nil "(asdf:load-asd ~S)" (slot-value thing 'path))))
  (:method ((thing system-name))
    nil))

(defgeneric thing-loader (thing)
  (:method ((thing lisp-file))
    (list
     (format nil "(progn
      (mapc #'asdf:load-system (asdf/package-inferred-system::package-inferred-system-file-dependencies ~S))
      (let ((*features* (cons :lob *features*)))
        (load ~S)))
"
             (slot-value thing 'path)
             (slot-value thing 'path))))
  (:method ((thing asd-file))
    (list
     (format nil "(progn
      (mapc #'asdf:load-system (asdf:component-sideway-dependencies (asdf:find-system ~S)))
      (let ((*features* (cons :lob *features*)))
        (asdf:load-system ~S)))"
             (pathname-name (slot-value thing 'path))
             (pathname-name (slot-value thing 'path)))))
  (:method ((thing system-name))
    (list
     (format nil "(asdf:load-system ~S)"
             (etypecase (slot-value thing 'name)
               (string (slot-value thing 'name))
               (symbol (string-downcase (slot-value thing 'name))))))))

(defgeneric thing-implied-package-name (thing)
  (:method ((thing lisp-file))
    (or (ignore-errors (let* ((form (asdf/package-inferred-system::file-defpackage-form (slot-value thing 'path))))
                         (string (second form))))
        (string-upcase (pathname-name (slot-value thing 'path)))))
  (:method ((thing asd-file))
    (string-upcase (pathname-name (slot-value thing 'path))))
  (:method ((thing system-name))
    (string-upcase (slot-value thing 'name))))

(defun run (&key
              image core loaded-things toplevel-symbol-name toplevel-package-name
              argv debug additional-source-registry
              format-error)
  (unless loaded-things
    (error "Must specify at least one thing to load"))

  (setf image (or image (first sb-ext:*posix-argv*))
        core (etypecase core
               (string (uiop:parse-native-namestring core))
               (pathname core)
               (null nil))
        loaded-things (mapcar #'resolve-thing (if (consp loaded-things) loaded-things (list loaded-things)))
        toplevel-symbol-name (or toplevel-symbol-name "MAIN")
        toplevel-symbol-name (string toplevel-symbol-name)
        toplevel-package-name (or toplevel-package-name (thing-implied-package-name (car (last loaded-things))))
        toplevel-package-name (string toplevel-package-name)
        additional-source-registry (mapcar #'truename (mapcar #'uiop:ensure-directory-pathname additional-source-registry))
        format-error (cond
                       ((stringp format-error)
                        format-error)
                       (format-error
                        "Error: ~A~%")))

  ;; Create the output directory so we can resolve it via truename as well

  ;;Diagnostics

  (let ((expressions ()))
    (flet ((expr (str &rest args)
             (push (apply #'format nil str args) expressions)))
      (expr "(in-package #:cl-user)~%")

      ;; Load ASDF
      (expr "~%(require \"ASDF\")~%")

      (when additional-source-registry
        (expr "~%(asdf:initialize-source-registry
  '(:source-registry
    :inherit-configuration~{
    (:tree ~S)~}))~%" additional-source-registry))

      ;; Pre-load any deps
      (let ((pre-load-forms (mapcan #'thing-preloader loaded-things)))
        (when pre-load-forms
          (expr "~%(handler-case
  (progn~{
    ~A~})
  (error (error)
    (format *error-output* \"error while pre-loading:~~%  ~~A~~%\" error)
    (sb-ext:exit :code 1)))~%" pre-load-forms)))

      ;; Get loaded systems
      (let ((load-forms (mapcan #'thing-loader loaded-things)))
        (when load-forms
          (expr "~%(handler-case
  (progn~{
    ~A~})
  (error (error)
    (format *error-output* \"error while loading:~~%  ~~A~~%\" error)
    (sb-ext:exit :code 1)))~%" load-forms)))

      (push
       (with-output-to-string (f)
         ;; Try and find the toplevel symbol
         (format f "~%(let ((#1=#:toplevel-sym (find-symbol ~S ~S)))" toplevel-symbol-name toplevel-package-name)

         ;;Verify toplevel symbol exist
         (format f "
  (unless #1#
    (format *error-output* \"cannot find toplevel symbol '~A' in package '~A'~~%\")
    (finish-output *error-output*)
    (sb-ext:exit :code 1))
"
                 toplevel-symbol-name toplevel-package-name)

         ;;Verify toplevel symbol is fboundp
         (format f "
  (unless (fboundp #1#)
    (format *error-output* \"toplevel symbol '~~A' is not fboundp~~%\" #1#)
    (finish-output *error-output*)
    (sb-ext:exit :code 2))
")

         ;; Run
         (if debug
             ;; If it's a debug build, don't catch toplevel errors since
             ;; that'll prevent the debugger from triggering
             (format f "
  (let ((#2=#:result (apply #1# (cdr *posix-argv*))))
    (sb-ext:exit :code (if (integerp #2#) #2# (if #2# 0 1)) :abort nil)))
")

             ;;Otherwise, catch toplevel errors and exit 1
             (format f "
  (handler-case
      (let ((#2=#:result (apply #1# (cdr *posix-argv*))))
        (sb-ext:exit :code (if (integerp #2#) #2# (if #2# 0 1)) :abort nil))
    (sb-sys:interactive-interrupt ()
      (sb-ext:exit :code #x-3FFFFEC6 :abort t))
    (error (~:[~;error~]~:*)~@[
      (format *error-output* ~S error)~]
      (sb-ext:exit :code 1 :abort t))))
"
                     format-error)))
       expressions))

    (let ((sbcl-args (concatenate 'list
                                  (delete nil (list
                                               (when core "--core")
                                               (when core (namestring core))
                                               "--noinform"
                                               "--end-runtime-options"
                                               "--no-sysinit"
                                               "--no-userinit"))
                                  (loop :for exp :in (nreverse expressions)
                                        :collect "--eval"
                                        :collect exp)
                                  (list*
                                   "--eval" "(exit :code 1)"
                                   "--end-toplevel-options"
                                   argv))))
      ;; (format t "lob: sbcl args:~%~{  ~A~%~}" sbcl-args)
      (sb-ext:process-exit-code
       (sb-ext:run-program
        image sbcl-args
        :search t
        :input t
        :output t
        :error t)))))
