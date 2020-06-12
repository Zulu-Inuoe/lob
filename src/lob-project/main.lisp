(in-package #:com.inuoe.lob-project)

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

(defun cl-user::main (argv)
  (flet ((lose (str &rest args)
           (apply #'format *error-output* str args)
           (finish-output *standard-output*)
           (finish-output *error-output*)
           (return-from cl-user::main 1)))
    (handler-case
        (loop
          :with dir := nil
          :with name := nil
          :with prefix := nil
          :with author := nil
          :with license := nil
          :with cell := (cdr argv)
          :for elt := (car cell)
          :while cell
          :do
             (flet ((take-arg ()
                      (setf cell (cdr cell))
                      (unless cell
                        (lose "lob-project: missing argument for ~A" elt))
                      (car cell)))
               (string-case elt
                 ("--help"
                  (format t "usage: lob-project [--version] [--help]
           [-n <name>] [-a <author>] [-l <license>]
           <dir>~%")
                  (return-from cl-user::main 0))
                 ("--version"
                  (format t "0.1.0~%")
                  (return-from cl-user::main 0))
                 ("-n"
                  (setf name (take-arg)))
                 ("-a"
                  (setf author (take-arg)))
                 ("-l"
                  (setf license (take-arg)))
                 (t
                  (setf dir elt)))
               (setf cell (cdr cell)))
          :finally
             (unless dir
               (lose "lob-project: missing output directory"))
             (return (make-project dir :name name :prefix prefix :author author :license license)))
      (error (e)
        (lose "lob-project: ~A" e)))))