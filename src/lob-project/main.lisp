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

(defun main (argv)
  (flet ((lose (str &rest args)
           (apply #'format *error-output* str args)
           (finish-output *standard-output*)
           (finish-output *error-output*)
           (return-from main 1)))
    (loop
      :with dir := nil
      :with name := nil
      :with prefix := nil
      :with description := nil
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
                   [-n <name>] [-a <author>] [-l <license>] [-d <description>]
                  <dir>~%")
              (return-from main 0))
             ("--version"
              (format t "0.1.0~%")
              (return-from main 0))
             ("-n"
              (setf name (take-arg)))
             ("-a"
              (setf author (take-arg)))
             ("-l"
              (setf license (take-arg)))
             ("-d"
              (setf description (take-arg)))
             (t
              (setf dir elt)))
           (setf cell (cdr cell)))
      :finally
         (unless dir
           (lose "lob-project: missing output directory"))
         (return (make-project dir :name name :prefix prefix :description description
                                   :author author :license license)))))
