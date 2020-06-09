(in-package #:com.inuoe.lob)

(defun executable-find (command)
  "Attempt to find the executable corresponding to `command'."
  (multiple-value-bind (outstring errstring exit-code)
      (uiop:run-program (list (cond
                                ((uiop:os-windows-p) "where")
                                ((or (uiop:os-unix-p)
                                     (uiop:os-macosx-p)) "which"))
                              command) :force-shell t :output '(:string :stripped t) :ignore-error-status t)
    (declare (ignore errstring))
    (when (zerop exit-code) (uiop:parse-native-namestring
                             (if (uiop:os-windows-p)
                                 (subseq outstring 0 (position #\Return outstring))
                                 outstring)))))

(defmacro string-case (value &body clauses)
  (let ((value-sym (gensym "VALUE")))
    `(let ((,value-sym ,value))
       (cond
         ,@(mapcar (lambda (c)
                     (destructuring-bind (clause . body) c
                       (cond
                         ((eq clause t)
                          `(t ,@body))
                         ((atom clause)
                          `((string= ,value-sym ,clause) ,@body))
                         (t
                          `((or ,@(mapcar (lambda (x) `(string= ,value-sym ,x)) clause))
                            ,@body)))))
                   clauses)))))

(defun parse-entry-name (name)
  (let* ((colon (position #\: name))
         (package-name (if colon (subseq name 0 colon)))
         (name-start (if colon
                         (let ((second-colon (position #\: name :start (1+ colon))))
                           (if second-colon
                               (prog1 (1+ second-colon)
                                 (when (position #\: name :start (1+ second-colon))
                                   (error "invalid entry point name - too many colons: ~A" name)))
                               (1+ colon)))
                         0))
         (symbol-name (subseq name name-start)))
    (values package-name symbol-name)))

(defun cl-user::main (argv)
  (flet ((lose (str &rest args)
           (apply #'format *error-output* str args)
           (finish-output *standard-output*)
           (finish-output *error-output*)
           (return-from cl-user::main 1)))
    (handler-case
        (loop
          :with toplevel-package-name := nil
          :with toplevel-symbol-name := nil
          :with loaded-things := ()
          :with output-path := nil
          :with gui := nil
          :with include-directories := ()
          :with debug-build := nil
          :with verbose := nil
          :with cell := (cdr argv)
          :for elt := (car cell)
          :while cell
          :do
             (flet ((take-arg ()
                      (setf cell (cdr cell))
                      (unless cell
                        (lose "lob: missing argument for ~A" elt))
                      (car cell)))
               (string-case elt
                 ("--help"
                  (format t "usage: lob [--version] [--help] [-v | --verbose]
           [-I <path>] [-o <path>] [-g] [-d]
           [-e | --entry-point [<package>:[:]]<name>] [-l <system-name>]
           <path>...~2%")
                  (return-from cl-user::main 0))
                 ("--version"
                  (format t "1.0.0~%")
                  (return-from cl-user::main 0))
                 (("-e" "--entry-point")
                  (setf (values toplevel-package-name toplevel-symbol-name)
                        (parse-entry-name (take-arg))))
                 ("-g"
                  (setf gui t))
                 ("-o"
                  (setf output-path (take-arg)))
                 ("-l"
                  (push (make-instance 'system-name :name (take-arg)) loaded-things))
                 ("-I"
                  (push (truename (uiop:ensure-directory-pathname (take-arg))) include-directories))
                 ("-d"
                  (setf debug-build t))
                 (("-v" "--verbose")
                  (setf verbose t))
                 (t
                  (when (or (string= elt "-" :end1 (min 1 (length elt)))
                            (string= elt "--" :end1 (min 2 (length elt))))
                    (lose "lob: unknown option: ~A" elt))
                  (push elt loaded-things)))

               (setf cell (cdr cell)))
          :finally
             (setf include-directories (nreverse include-directories)
                   loaded-things (nreverse loaded-things))
             (unless loaded-things
               (lose "lob: no input files"))
             (return (let ((*lob-stdout* (if verbose *standard-output* (make-broadcast-stream))))
                       (build :image (executable-find "sbcl")
                              :gui gui
                              :toplevel-symbol-name toplevel-symbol-name
                              :toplevel-package-name toplevel-package-name
                              :loaded-things loaded-things
                              :output-path output-path
                              :debug-build debug-build
                              :additional-source-registry include-directories))))
      (error (e)
        (lose "lob: ~A" e)))))
