(defpackage #:com.inuoe.lob/commands/build
  (:use #:cl)
  (:local-nicknames
   (#:things #:com.inuoe.lob/things)
   (#:spawn #:com.inuoe.lob/spawn))
  (:export #:build))
(in-package #:com.inuoe.lob/commands/build)

(defgeneric thing-implied-package-name (thing)
  (:method ((thing things:lisp-file))
    (or (ignore-errors (let ((form (asdf/package-inferred-system::file-defpackage-form (slot-value thing 'things:path))))
                         (and form
                              (string (second form)))))
        "CL-USER"))
  (:method ((thing things:asd-file))
    (string-upcase (pathname-name (slot-value thing 'things:path))))
  (:method ((thing things:system-name))
    (string-upcase (slot-value thing 'things:name))))

(defgeneric prettify-thing (thing)
  (:method ((thing things:lisp-file))
    (format nil "~A" (slot-value thing 'things:path)))
  (:method ((thing things:asd-file))
    (format nil "~A" (slot-value thing 'things:path)))
  (:method ((thing things:system-name))
    (format nil "~A" (slot-value thing 'things:name))))

(defun write-build-forms (stream &key output-path toplevel-symbol-name toplevel-package-name gui debug-build compression format-error)
  (format stream "
(ensure-directories-exist ~S)
"
          (uiop:pathname-directory-pathname output-path))



  (format stream "
;; Try and find the toplevel symbol
(let ((symbol (find-symbol ~S ~S)))
  ;;Verify toplevel symbol exist
  (unless symbol
    (format *error-output* \"lob: cannot find toplevel symbol '~A' in package '~A'~~%\")
    (finish-output *error-output*)
    (sb-ext:exit :code 1))

   ;;Verify toplevel symbol is fboundp
  (unless (fboundp symbol)
    (format *error-output* \"lob: toplevel symbol '~~A' is not fboundp~~%\" symbol)
    (finish-output *error-output*)
    (sb-ext:exit :code 2))

   ;; Flush output streams
  (finish-output *standard-output*)
  (finish-output *error-output*)
  (sb-ext:gc :full t)

  ;;Dump exe
  (sb-ext:save-lisp-and-die
    ~S
    :executable t
    :save-runtime-options t
    :application-type :~:[console~;gui~]
    :toplevel"
          toplevel-symbol-name
          toplevel-package-name
          toplevel-symbol-name
          toplevel-package-name
          output-path
          gui)

  (if debug-build
      ;; If it's a debug build, don't catch toplevel errors since
      ;; that'll prevent the debugger from triggering
      (format stream "
    (lambda ()
      (sb-ext:enable-debugger)
      (let ((return (apply symbol sb-ext:*posix-argv*)))
        (sb-ext:exit :code (if (integerp return) return (if return 0 1)) :abort nil)))")

      ;;Otherwise, catch toplevel errors and exit 1
      (format stream "
    (lambda ()
      (handler-case
          (let ((return (apply symbol sb-ext:*posix-argv*)))
            (sb-ext:exit :code (if (integerp return) return (if return 0 1)) :abort nil))
        (sb-sys:interactive-interrupt ()
          (sb-ext:exit :code -1073741510 :abort t))
        (error (~:[~;error~]~:*)~@[
          (format *error-output* ~S error)~]
          (sb-ext:exit :code 1 :abort t))))"
              format-error))

  (when compression
    (format stream "
    :compression ~A" compression))

  (format stream "))
")

  ;; Exit with error if we fall through
  (format stream "
;; fallback exit if save-lisp-or-die passes
(sb-ext:exit :code 1)
"))

(defun build (&key
                image core loaded-things gui toplevel-symbol-name toplevel-package-name
                output-path debug-build compression additional-source-registry
                format-error
                verbose
              &aux
                (stdout (if verbose *standard-output* (make-broadcast-stream))))
  (unless loaded-things
    (error "Must specify at least one thing to load"))

  (setf image (etypecase image
                (string (uiop:parse-native-namestring image))
                (pathname image)
                (null sb-ext:*runtime-pathname*))
        core (etypecase core
               (string (uiop:parse-native-namestring core))
               (pathname core)
               (null nil))
        loaded-things (mapcar #'things:resolve-thing (if (listp loaded-things) loaded-things (list loaded-things)))
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
  (format stdout "lob: image: ~A~%" image)
  (when core
    (format stdout "lob: using core ~A~2%" core))

  (format stdout "lob: loading:~%~{~T~A~%~}~%" (mapcar #'prettify-thing loaded-things))

  (format stdout "lob: outputting executable as ~A~%" output-path)
  (format stdout "lob: application is ~:[CONSOLE~;GUI~]~%" gui)
  (format stdout "lob: toplevel is ~A::~A~%" toplevel-package-name toplevel-symbol-name)


  (format stdout "~%==BEGIN BOOTSTRAP==~2%")
  (finish-output stdout)
  (finish-output *error-output*)
  (let ((lisp (spawn:spawn :image image :core core))
        (success nil))
    (unwind-protect
         (let ((stdout (make-broadcast-stream stdout (sb-ext:process-input lisp))))
           (spawn:write-load-forms stdout :loaded-things loaded-things :additional-source-registry additional-source-registry)
           (write-build-forms stdout :output-path output-path :debug-build debug-build
                                     :toplevel-symbol-name toplevel-symbol-name :toplevel-package-name toplevel-package-name
                                     :gui gui :compression compression
                                     :format-error format-error)

           (finish-output stdout)
           (sb-ext:with-timeout 10
             (sb-ext:process-wait lisp t))
           (setf success t))
      (unless success
        (when (sb-ext:process-alive-p lisp)
          (sb-ext:process-kill lisp #+win32 0 #-win32 sb-unix:sigkill)
          (sb-ext:with-timeout 10
            (sb-ext:process-wait t)))))
    (finish-output stdout)
    (finish-output *error-output*)
    (format stdout "~%==END BOOTSTRAP==~2%")
    (let* ((code (sb-ext:process-exit-code lisp))
           (success (zerop code)))
      (if success
          (format stdout "lob: compilation finished~%")
          (format *error-output* "lob: bootstrapping failed, exited with code ~A~%" code))
      (finish-output stdout)
      (finish-output *error-output*)
      (values success code))))
