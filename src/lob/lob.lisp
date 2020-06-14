(defpackage #:com.inuoe.lob
  (:use #:cl)
  (:export
   #:*lob-stdout*
   #:*lob-stderr*
   #:build)
  (:import-from
   #:alexandria
   #:assoc-value
   #:required-argument
   #:eswitch)
  (:import-from #:uiop))

(in-package #:com.inuoe.lob)

(defun lob-dir ()
  (uiop:merge-pathnames* (make-pathname :directory '(:relative ".lob"))
                         (user-homedir-pathname)))

(defun package-directory (name version)
  (uiop:merge-pathnames*
   (make-pathname :directory (list :relative "packages" (concatenate 'string name "_" version)))
   (lob-dir)))

(defgeneric source-name (source))

(defclass source ()
  ((name
    :reader source-name
    :initarg :name)
   (options
    :reader source-options
    :initarg :options))
  (:default-initargs
   :name (required-argument :name)
   :options (required-argument :options)))

(defclass dist-source (source)
  ((dist
    :reader source-dist
    :initarg :dist))
  (:default-initargs
   :dist (required-argument :dist)))

(defclass uri-source (source)
  ((version
    :initarg :version
    :reader source-version))
  (:default-initargs
   :version (required-argument :version)))

(defclass http-source (uri-source)
  ())

(defclass git-source (uri-source)
  ())

(defclass file-source (uri-source)
  ())

(defun dist-versions-from-url (url)
  (let ((temp (qmerge "tmp/dist-versions.txt"))
        (versions '())
        (url (ql::available-versions-url "org.borodust.bodge")))
    (when url
      (ensure-directories-exist temp)
      (delete-file-if-exists temp)
      (handler-case
          (fetch url temp)
        (unexpected-http-status ()
          (return-from available-versions nil)))
      (with-open-file (stream temp)
        (loop for line = (read-line stream nil)
              while line do
                (destructuring-bind (version url)
                    (split-spaces line)
                  (setf versions (acons version url versions)))))
      versions)))

(defmethod available-versions ((dist dist))
  )

(defun available-dist-versions (dist)
  (etypecase dist
    (string )))

(defun parse-uri (value)
  (puri:parse-uri value))

(defun parse-version (value)
  (uiop:parse-version value 'error))

(defun parse-dist (value)
  value)

(defun coerce-action (name value)
  (eswitch (name :test #'string=)
    ("url" (cons :url (parse-uri value)))
    ("uri" (cons :url (parse-uri value)))
    ("version" (cons :version (parse-version value)))
    ("dist" (cons :dist (parse-dist value)))))

;; TODO - Warn or error on lines containing junk
(defun collect-options (options-str)
  "Parse `options-str' into a list of options"
  (let ((options ()))
    (cl-ppcre:do-register-groups (opt-name opt-value)
        ("\\s*(.*?):\\s+\"(.*?)\"" options-str (nreverse options) :sharedp t)
      (push (coerce-action opt-name opt-value) options))))

(defun make-source (name options)
  (let ((url (assoc-value options :url)))
    (if url
        (let ((version (assoc-value options :version)))
          (unless version
            (error "Using url ~A requires specifying a version" url))

          (make-instance (ecase (puri:uri-scheme url)
                           ((:http :https) 'http-source)
                           ((:git) 'git-source)
                           ((:file) 'file-source))
                         :name name
                         :version version
                         :options options))
        (make-instance 'dist-source :name name :options options
                                    :dist (assoc-value options :dist)))))

(defun parse-lob-line (line)
  (or
   (cl-ppcre:register-groups-bind (directive name options)
       ("^(.+?)\\s+\"(.*?)\"\\s*(.*)$" line :sharedp nil)
     (unless (string= directive "lob")
       (error "Unknown directive ~A" directive))
     (make-source name (collect-options options)))
   (error "invalid line '~A'" line)))

(defun parse-lobfile (path)
  "Parse a lobfile"
  (with-open-file (stream path :external-format :utf-8)
    (loop
      :for line := (read-line stream nil nil)
      :while line
      :unless (or (zerop (length line))
                  (char= (char line 0) #\;))
        :collect (parse-lob-line line))))

(defgeneric install-source (source directory))

(defmethod install-source ((source source) directory)
  (let ((dist-versions (ql:available-dist-versions (source-dist source)))))
  (error "unimplemented"))

(defmethod install-source ((source http-source) directory)
  (error "unimplemented"))

(defmethod install-source ((source git-source) directory)
  (error "unimplemented"))

(defmethod install-source ((source file-source) directory)
  (error "unimplemented"))

(defun resolve-version (source)
  (let ((version (assoc-value (source-options source) :version)))
    (or version
        (error "TBD - get latest version number from QL"))))

(defun ensure-source-installed (source)
  (let ((dir (package-directory (source-name source) (resolve-version source))))
    (unless (uiop:directory-exists-p dir)
      (install-source source dir))))

(defclass registry-entry ()
  ((name
    :initarg :name
    :reader entry-name)
   (version-constraint
    :initarg :version
    :reader entry-version-constraint))
  (:default-initargs
   :name (required-argument :name)))

(defun get-fetch-fn (uri)
  (assoc-value ql-http:*fetch-scheme-functions* (puri:uri-scheme uri)
               :test #'string-equal))

(defclass dist (ql-dist:dist)
  ())

(defun dist-from-url (dist-url)
  (setf dist-url (puri:uri dist-url))
  (uiop:with-temporary-file (:pathname file :external-format :utf-8)
    (let ((fetch-fn (get-fetch-fn dist-url)))
      (unless fetch-fn
        (error "Unknown scheme ~S" dist-url))
      (funcall fetch-fn (puri:render-uri dist-url nil) file))
    ;; TODO Internal fn
    (ql-dist::make-dist-from-file file :class 'ql-dist:dist)))

(defun install-lobfile (path)

  (let ((registry (make-hash-table)))
    )
  (mapc #'install-source (parse-lobfile path)))

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
    t)
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
    (string-upcase (pathname-name (slot-value thing 'path))))
  (:method ((thing asd-file))
    (string-upcase (pathname-name (slot-value thing 'path))))
  (:method ((thing system-name))
    (string-upcase (slot-value thing 'name))))

(defun build (&key
                image core loaded-things gui toplevel-symbol-name toplevel-package-name
                output-path debug-build compression additional-source-registry asdf-p
                format-error)
  (unless loaded-things
    (error "Must specify at least one thing to load"))

  ;; truename-ize all the paths
  (setf image (or image (first sb-ext:*posix-argv*))
        core (and core (truename core))
        loaded-things (mapcar #'resolve-thing (if (consp loaded-things) loaded-things (list loaded-things) ))
        toplevel-symbol-name (or toplevel-symbol-name "MAIN")
        toplevel-symbol-name (string toplevel-symbol-name)
        toplevel-package-name (or toplevel-package-name (thing-implied-package-name (car (last loaded-things))))
        toplevel-package-name (string toplevel-package-name)
        output-path (or output-path "a.exe")
        additional-source-registry (mapcar #'uiop:ensure-directory-pathname additional-source-registry)
        asdf-p (or asdf-p (not (null additional-source-registry)) (some #'need-asdf-p loaded-things))
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

  (format *lob-stdout* "lob: loading:~%~{~T~A~%~}~%" loaded-things)

  (format *lob-stdout* "lob: outputting executable as ~A~%" output-path)
  (format *lob-stdout* "lob: application is ~:[CONSOLE~;GUI~]~%" gui)
  (format *lob-stdout* "lob: toplevel is ~A::~A~%" toplevel-package-name toplevel-symbol-name)

  (uiop:with-temporary-file (:stream f :pathname p :direction :output)
    (format *lob-stdout* "~%lob: creating bootstrap at ~A~%" (namestring p))
    (format *lob-stdout* "~%==BEGIN BOOTSTRAP CODE==~2%")
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
          (format *lob-stdout* "~%(handler-case
  (progn~{
    ~A~})
  (error (#1=#:error)
    (format *error-output* \"lob: error while pre-loading:
  ~~A~%\" #1#)
    (sb-ext:exit :code 1)))~%" pre-load-forms)))

      ;; Get loaded systems
      (let ((load-forms (mapcan #'thing-loader loaded-things)))
        (when load-forms
          (format *lob-stdout* "~%(handler-case
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
      (format *lob-stdout* "~%  (unless (fboundp #1#)
    (format *error-output* \"lob: toplevel symbol '~~A' is not fboundp~~%\" #1#)
    (finish-output *error-output*)
    (sb-ext:exit :code 2))~%")

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
        (error (#4#)
          ~:[(declare (ignore #4#))~;~:*(format *error-output* ~S #4#)~]
          (sb-ext:exit :code 1 :abort t))))"
                  format-error))

      (when compression
        (format *lob-stdout* "
    :compression ~A" compression))

      (format *lob-stdout* "))~%")

      ;;Finish output to the file
      (finish-output *lob-stdout*))
    (format *lob-stdout* "~%==END BOOTSTRAP CODE==~2%")
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
      (format *lob-stdout* "lob: sbcl args:~%~{  ~A~%~}" sbcl-args)

      (let (code)
        (format *lob-stdout* "~%==BEGIN BOOTSTRAP==~2%")

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

        (format *lob-stdout* "~%==END BOOTSTRAP==~2%")

        (unless (zerop code)
          (format *lob-stderr* "lob: bootstrapping failed, exited with code ~A~%" code)
          (finish-output *lob-stdout*)
          (finish-output *lob-stderr*)
          (return-from build nil))))

    (format *lob-stdout* "lob: compilation finished~%"))
  (finish-output *lob-stdout*)
  (finish-output *lob-stderr*)
  t)
