(defpackage #:com.inuoe.lob-build
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

   #:build))

(in-package #:com.inuoe.lob-build)

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
        (load ~S)))"
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

(defun build (&key
                image core loaded-things gui toplevel-symbol-name toplevel-package-name
                output-path debug-build compression additional-source-registry
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
        output-path (or output-path "a.exe")
        additional-source-registry (mapcar #'truename (mapcar #'uiop:ensure-directory-pathname additional-source-registry))
        format-error (cond
                       ((stringp format-error)
                        format-error)
                       (format-error
                        "Error: ~A~%")))

  ;; Create the output directory so we can resolve it via truename as well

  (ensure-directories-exist output-path)
  (setf output-path (make-pathname
                     :name (pathname-name output-path)
                     :type (pathname-type output-path)
                     :defaults (truename (uiop:pathname-directory-pathname output-path))))

  ;;Diagnostics
  (format *lob-stdout* "lob: image: ~A~%" image)
  (when core
    (format *lob-stdout* "lob: using core ~A~2%" core))

  (format *lob-stdout* "lob: loading:~%~{~T~A~%~}~%" (mapcar #'prettify-thing loaded-things))

  (format *lob-stdout* "lob: outputting executable as ~A~%" output-path)
  (format *lob-stdout* "lob: application is ~:[CONSOLE~;GUI~]~%" gui)
  (format *lob-stdout* "lob: toplevel is ~A::~A~%" toplevel-package-name toplevel-symbol-name)

  (uiop:with-temporary-file (:stream f :pathname p :direction :output)
    (format *lob-stdout* "~%lob: creating bootstrap at ~A~%" (namestring p))
    (format *lob-stdout* "~%==BEGIN BOOTSTRAP CODE==~2%")
    (let ((*lob-stdout* (make-broadcast-stream *lob-stdout* f)))
      (format *lob-stdout* "(in-package #:cl-user)~%")

      ;; Load ASDF
      (format *lob-stdout* "~%(require \"ASDF\")~%")

      (when additional-source-registry
        (format *lob-stdout* "~%(asdf:initialize-source-registry
  '(:source-registry
    :inherit-configuration~{
    (:tree ~S)~}))~%" additional-source-registry))

      ;; Pre-load any deps
      (let ((pre-load-forms (mapcan #'thing-preloader loaded-things)))
        (when pre-load-forms
          (format *lob-stdout* "~%(handler-case
  (progn~{
    ~A~})
  (error (error)
    (format *error-output* \"lob: error while pre-loading:~~%  ~~A~~%\" error)
    (sb-ext:exit :code 1)))~%" pre-load-forms)))

      ;; Get loaded systems
      (let ((load-forms (mapcan #'thing-loader loaded-things)))
        (when load-forms
          (format *lob-stdout* "~%(handler-case
  (progn~{
    ~A~})
  (error (error)
    (format *error-output* \"lob: error while loading:~~%  ~~A~~%\" error)
    (sb-ext:exit :code 1)))~%" load-forms)))

      ;; Clear out asdf source registry
      (format *lob-stdout* "~%(asdf:clear-source-registry)~%")

      ;; Make sure output directory exists
      (format *lob-stdout* "~%(ensure-directories-exist ~S)~%" (uiop:pathname-directory-pathname output-path))

      ;; Try and find the toplevel symbol
      (format *lob-stdout* "~%(let ((#1=#:toplevel-sym (find-symbol ~S ~S)))" toplevel-symbol-name toplevel-package-name)

      ;;Verify toplevel symbol exist
      (format *lob-stdout* "
  (unless #1#
    (format *error-output* \"lob: cannot find toplevel symbol '~A' in package '~A'~~%\")
    (finish-output *error-output*)
    (sb-ext:exit :code 1))
"
              toplevel-symbol-name toplevel-package-name)

      ;;Verify toplevel symbol is fboundp
      (format *lob-stdout* "
  (unless (fboundp #1#)
    (format *error-output* \"lob: toplevel symbol '~~A' is not fboundp~~%\" #1#)
    (finish-output *error-output*)
    (sb-ext:exit :code 2))
")

      ;; Flush output streams
      (format *lob-stdout* "
  (finish-output *standard-output*)
  (finish-output *error-output*)
  (sb-ext:gc :full t)
")

      ;;Dump exe
      (format *lob-stdout* "
  (sb-ext:save-lisp-and-die
    ~S
    :executable t
    :save-runtime-options t
    :application-type :~:[console~;gui~]
    :toplevel"
              output-path
              gui)

      (if debug-build
          ;; If it's a debug build, don't catch toplevel errors since
          ;; that'll prevent the debugger from triggering
          (format *lob-stdout* "
    (lambda ()
      (sb-ext:enable-debugger)
      (let* ((#2=#:result (apply #1# *posix-argv*))
             (#3=#:result-code (if (integerp #2#) #2# (if #2# 0 1))))
        (sb-ext:exit :code #3# :abort nil)))")

          ;;Otherwise, catch toplevel errors and exit 1
          (format *lob-stdout* "
    (lambda ()
      (handler-case
          (let ((#2=#:result (apply #1# *posix-argv*)))
            (sb-ext:exit :code (if (integerp #2#) #2# (if #2# 0 1)) :abort nil))
        (sb-sys:interactive-interrupt ()
          (sb-ext:exit :code #x-3FFFFEC6 :abort t))
        (error (~:[~;error~]~:*)~@[
          (format *error-output* ~S error)~]
          (sb-ext:exit :code 1 :abort t))))"
                  format-error))

      (when compression
        (format *lob-stdout* "
    :compression ~A" compression))

      (format *lob-stdout* "))
")

      ;;Finish output to the file
      (finish-output *lob-stdout*))
    (format *lob-stdout* "~%==END BOOTSTRAP CODE==~2%")
    (let ((sbcl-args (delete nil (list
                                  (when core "--core")
                                  (when core (namestring core))
                                  "--noinform"
                                  "--end-runtime-options"
                                  "--no-sysinit"
                                  "--no-userinit"
                                  "--disable-debugger"
                                  "--load" (namestring p)
                                  "--eval" "(exit :code 1)"))))
      (format *lob-stdout* "lob: sbcl args:~%~{  ~A~%~}" sbcl-args)

      (format *lob-stdout* "~%==BEGIN BOOTSTRAP==~2%")

      (finish-output *lob-stdout*)
      (finish-output *lob-stderr*)

      (let* ((code (sb-ext:process-exit-code
                    (sb-ext:run-program
                     image sbcl-args
                     :search t
                     :input nil
                     :output *lob-stdout*
                     :error *lob-stderr*)))
             (success (zerop code)))

        (format *lob-stdout* "~%==END BOOTSTRAP==~2%")

        (if success
            (format *lob-stdout* "lob: compilation finished~%")
            (format *lob-stderr* "lob: bootstrapping failed, exited with code ~A~%" code))

        (finish-output *lob-stdout*)
        (finish-output *lob-stderr*)

        (values success code)))))
