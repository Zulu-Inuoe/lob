(defpackage #:lob
  (:use #:cl)
  (:export
   #:*lob-stdout*
   #:*lob-stderr*
   #:build))

(in-package #:lob)

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
    (string (resolve-thing (parse-namestring thing)))
    (pathname
     (cond
       ((string-equal (pathname-type thing) "lisp")
        (make-instance 'lisp-file :path (truename thing)))
       ((string-equal (pathname-type thing) "asd")
        (make-instance 'asd-file :path (truename thing)))))
    (symbol
     (make-instance 'system-name :name thing))
    ((or lisp-file asd-file system-name) thing)))

(defgeneric need-asdf-p (thing)
  (:method ((thing lisp-file))
    nil)
  (:method ((thing asd-file))
    t)
  (:method ((thing system-name))
    t))

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
     (format nil "(let ((*features* (cons :lob *features*))) (load ~S))" (slot-value thing 'path))))
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

(defun build (&key
                image core loaded-things gui toplevel-symbol-name toplevel-package-name
                output-path debug-build compression additional-source-registry asdf-p)
  ;; truename-ize all the paths
  (setf image (or image (first sb-ext:*posix-argv*))
        core (and core (truename core))
        loaded-things (mapcar #'resolve-thing (if (consp loaded-things) loaded-things (list loaded-things) ))
        toplevel-symbol-name (or toplevel-symbol-name "MAIN")
        toplevel-symbol-name (string toplevel-symbol-name)
        toplevel-package-name (or toplevel-package-name  "COMMON-LISP-USER")
        toplevel-package-name (string toplevel-package-name)
        output-path (or output-path "a.exe")
        additional-source-registry (mapcar #'uiop:ensure-directory-pathname additional-source-registry)
        asdf-p (or asdf-p (not (null additional-source-registry)) (some #'need-asdf-p loaded-things)))

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

  (when loaded-things
    (format *lob-stdout* "~2&lob: loading:~{~&~T~A~}~%" loaded-things))

  (format *lob-stdout* "~2&lob: outputting executable as ~A~%" output-path)
  (format *lob-stdout* "lob: application is ~:[CONSOLE~;GUI~]~%" gui)
  (format *lob-stdout* "lob: toplevel is ~A::~A~%" toplevel-package-name toplevel-symbol-name)

  (uiop:with-temporary-file (:stream f :pathname p :direction :output)
    (format *lob-stdout* "~%lob: creating bootstrap at ~A~%" (namestring p))
    (format *lob-stdout* "~2&==BEGIN BOOTSTRAP CODE==~2%")
    (let ((*lob-stdout* (make-broadcast-stream *lob-stdout* f)))
      (format *lob-stdout* "(in-package #:cl-user)~%")

      ;; Load ASDF
      (when asdf-p
        (format *lob-stdout* "~%(require \"ASDF\")~%"))

      (when additional-source-registry
        (format *lob-stdout* "(asdf:initialize-source-registry
  '(:source-registry
    :inherit-configuration~{
    (:tree ~S)~}))" additional-source-registry))

      ;; Pre-load any deps
      (let ((pre-load-forms (mapcan #'thing-preloader loaded-things)))
        (when pre-load-forms
          (format *lob-stdout* "
(handler-case
  (progn~{
    ~A~})
  (error (#1=#:error)
    (format *error-output* \"lob: error while pre-loading:
  ~~A~%\" #1#)
    (sb-ext:exit :code 1)))~%" pre-load-forms)))

      ;; Get loaded systems
      (let ((load-forms (mapcan #'thing-loader loaded-things)))
        (when load-forms
          (format *lob-stdout* "
(handler-case
  (progn~{
    ~A~})
  (error (#1=#:error)
    (format *error-output* \"lob: error while loading:
  ~~A~%\" #1#)
    (sb-ext:exit :code 1)))~%" load-forms)))

      ;; Clear out asdf source registry
      (when asdf-p
        (format *lob-stdout* "~%(asdf:clear-source-registry)~%"))

      ;; Make sure output directory exists
      (format *lob-stdout* "~%(ensure-directories-exist ~S)~%" output-path)

      ;; Try and find the toplevel symbol
      (format *lob-stdout* "
(let ((#1=#:toplevel-sym (find-symbol ~S ~S)))" toplevel-symbol-name toplevel-package-name)

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

      (when debug-build
        (format *lob-stdout* "
  (sb-ext:enable-debugger)
"))

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
      (let* ((#2=#:result (funcall #1# *posix-argv*))
             (#3=#:result-code (if (integerp #2#) #2# (if #2# 0 1))))
        (sb-ext:exit :code #3# :abort nil)))")

          ;;Otherwise, catch toplevel errors and exit 1
          (format *lob-stdout* "
    (lambda ()
      (handler-case
        (let* ((#2=#:result (funcall #1# *posix-argv*))
               (#3=#:result-code (if (integerp #2#) #2# (if #2# 0 1))))
          (sb-ext:exit :code #3# :abort nil))
        (sb-sys:interactive-interrupt (#4=#:error)
         (declare (ignore #4#))
         (sb-ext:exit :code 1 :abort t))
        (error ()
          (sb-ext:exit :code 1 :abort t))))"))

      (when compression
        (format *lob-stdout* "
    :compression ~A" compression))

      (format *lob-stdout* "))~%")

      ;;Finish output to the file
      (finish-output *lob-stdout*))
    (format *lob-stdout* "~2&==END BOOTSTRAP CODE==~2%")
    (let ((sbcl-args (list
                      (when core "--core")
                      (when core (namestring core))
                      "--noinform"
                      "--end-runtime-options"
                      "--no-sysinit"
                      "--no-userinit"
                      "--disable-debugger"
                      "--load" (namestring p)
                      "--eval" "(exit :code 1)")))
      (setf sbcl-args (delete nil sbcl-args))
      (format *lob-stdout* "lob: sbcl args:~{~&  ~A~}~%" sbcl-args)

      (let (code)
        (format *lob-stdout* "~2&==BEGIN BOOTSTRAP==~2%")

        (finish-output *lob-stdout*)
        (finish-output *lob-stderr*)
        (setf code
              (sb-ext:process-exit-code
               (sb-ext:run-program
                image sbcl-args
                :search t
                :input nil
                :output *lob-stdout*
                :error *lob-stderr*)))

        (format *lob-stdout* "~2&==END BOOTSTRAP==~2%")

        (unless (zerop code)
          (format *lob-stderr* "lob: bootstrapping failed, exited with code ~A~%" code)
          (finish-output *lob-stdout*)
          (finish-output *lob-stderr*)
          (return-from build nil))))

    (format *lob-stdout* "lob: compilation finished~%"))
  (finish-output *lob-stdout*)
  (finish-output *lob-stderr*)
  t)
