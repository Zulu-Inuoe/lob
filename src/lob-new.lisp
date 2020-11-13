(defpackage #:com.inuoe.lob-new
  (:use #:cl)
  (:export
   #:*lob-new-stdout*
   #:make-project))

(in-package #:com.inuoe.lob-new)

(defvar *lob-new-stdout* (make-synonym-stream '*standard-output*))

(defun user-full-name ()
  "Try to infer the current user's name from their git setting or $USER or $USERNAME"
  (or (ignore-errors (string-trim '(#\Newline) (uiop:run-program '("git" "config" "user.name") :output :string)))
      (uiop:getenv "USER")
      (uiop:getenv "USERNAME")))

(defun user-mail-address ()
  "Try to infer the current user's e-mail from their git config"
  (ignore-errors (string-trim '(#\Newline) (uiop:run-program '("git" "config" "user.email") :output :string))))

(defun make-project (dir &key name description prefix author license type &aux prefix.name)
  (setf dir (uiop:ensure-directory-pathname dir)
        name (or name (car (last (pathname-directory dir))))
        (values prefix name)
        (cond
          (prefix
           (values prefix name))
          ((position #\. name :from-end t)
           (let ((last-dot (position #\. name :from-end t)))
             (values (subseq name 0 last-dot)
                     (subseq name (1+ last-dot)))))
          (t
           (values nil name)))
        prefix.name (if prefix (concatenate 'string prefix "." name) name)
        description (or description name)
        author (or author (format nil "~A~@[ <~A>~]" (user-full-name) (user-mail-address)))
        license (or license "MIT")
        type (or type :exe)
        type (cond
               ((member type '("x" "exe" "executable") :test #'string-equal) :exe)
               ((member type '("l" "lib" "library") :test #'string-equal) :lib)
               (t (error "Unknown type '~A'" type))))

  (format *lob-new-stdout* "lob: Creating project in directory ~A" dir)
  (format *lob-new-stdout* "  name:~A~%  prefix: ~A~%" name prefix)

  (ensure-directories-exist dir)
  (with-open-file (readme (uiop:merge-pathnames* (make-pathname :name "README" :type "md") dir)
                          :direction :output :external-format :utf-8)
    (format readme "# ~A
" name))

  (with-open-file (gitignore (uiop:merge-pathnames* (make-pathname :name ".gitignore" :type nil) dir)
                             :direction :output :external-format :utf-8)
    (write-line "*.fasl" gitignore))

  (let ((src (ensure-directories-exist (uiop:merge-pathnames* (make-pathname :directory '(:relative "src")) dir))))
    (with-open-file (asd (uiop:merge-pathnames* (make-pathname :name prefix.name :type "asd") src)
                         :direction :output :external-format :utf-8)
      (format asd "(defsystem #:~A
  :version \"0.0.1\"
  :description \"~A\"
  :author \"~A\"
  :license \"~A\"
  :components
  ((:file \"~A\"))
  :depends-on
  ())
"
              prefix.name
              description
              author
              license
              name))

    (with-open-file (lisp-file (uiop:merge-pathnames* (make-pathname :name name :type "lisp") src)
                               :direction :output :external-format :utf-8)
      (format lisp-file "(defpackage #:~A
  (:use #:cl))

(in-package #:~A)
"
              prefix.name
              prefix.name)

      (when (eq type :exe)
        (format lisp-file "(defun main (&rest argv)
  (declare (ignore argv))
  0)
"))))

  (let* ((name_test (concatenate 'string name "_test"))
         (prefix.name_test (concatenate 'string prefix.name "_test"))
         (test (ensure-directories-exist (uiop:merge-pathnames* (make-pathname :directory '(:relative "test")) dir))))
    (with-open-file (asd (uiop:merge-pathnames* (make-pathname :name prefix.name_test :type "asd") test)
                         :direction :output :external-format :utf-8)
      (format asd "(defsystem #:~A
  :version \"0.0.1\"
  :description \"~A\"
  :author \"~A\"
  :license \"~A\"
  :components
  ((:file \"~A\"))
  :depends-on
  (#:fiveam
   #:~A))
"
              prefix.name_test
              description
              author
              license
              name
              prefix.name))
    (with-open-file (name_test.lisp (uiop:merge-pathnames* (make-pathname :name name_test :type "lisp") test)
                                    :direction :output :external-format :utf-8)
      (format name_test.lisp "(defpackage #:~A
  (:use
   #:cl
   #:~A)
  (:import-from
   #:fiveam
   #:def-suite
   #:finishes
   #:in-suite
   #:is
   #:signals
   #:test)
  (:export
   #:~A
   #:run
   #:main))

(in-package #:~A)

(defun run ()
  (fiveam:run! '~A))

(defun main (&rest argv)
  (declare (ignore argv))
  (let ((result (fiveam:run '~A)))
    (if result 0 -1)))

(def-suite ~A
  :description \"Tests for ~A.\")
(in-suite ~A)
"
              prefix.name_test
              prefix.name
              name
              prefix.name_test
              name
              name
              name
              prefix.name
              name))))
