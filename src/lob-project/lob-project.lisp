(defpackage #:com.inuoe.lob-project
  (:use #:cl))

(in-package #:com.inuoe.lob-project)

(defun user-full-name ()
  "Try to infer the current user's name from their git setting or $USER or $USERNAME"
  (or (ignore-errors (string-trim '(#\Newline) (uiop:run-program '("git" "config" "user.name") :output :string)))
      (uiop:getenv "USER")
      (uiop:getenv "USERNAME")))

(defun user-mail-address ()
  "Try to infer the current user's e-mail from their git config"
  (ignore-errors (string-trim '(#\Newline) (uiop:run-program '("git" "config" "user.email") :output :string))))

(defun make-project (dir &key name description prefix author license &aux prefix.name)
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
        license (or license "CC0 1.0 Universal"))

  (ensure-directories-exist dir)
  (with-open-file (asd-file (uiop:merge-pathnames* (make-pathname :name prefix.name :type "asd") dir)
                            :direction :output)
    (format asd-file "(defsystem #:~A
  :version \"0.0.1\"
  :description \"~A\"
  :author \"~A\"
  :license \"~A\"
  :components
  ((:file \"~A\")
   (:file \"main\" :if-feature :lob :depends-on \"~A\"))
  :depends-on
  ())
"
            prefix.name
            description
            author
            license
            name
            name))

  (with-open-file (lisp-file (uiop:merge-pathnames* (make-pathname :name name :type "lisp") dir)
                             :direction :output)
    (format lisp-file "(defpackage #:~A
  :use (#:cl))

(in-package #:~A)
"
            prefix.name
            prefix.name))


  (with-open-file (main-file (uiop:merge-pathnames* (make-pathname :name "main" :type "lisp") dir)
                             :direction :output)
    (format main-file "(in-package #:~A)

(defun cl-user::main (argv)
  0)
" prefix.name)))
