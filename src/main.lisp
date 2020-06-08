(in-package #:lob)

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

(defun cl-user::main (argv)
  (flet ((lose (str)
               (format *error-output* "lob: ~A~%" str)
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
          :with cell := (cdr argv)
          :with include-directories := ()
          :with debug-build := nil
          :with verbose := nil
          :for elt := (car cell)
          :while cell
          :do
             (cond
               ((string= elt "--help")
                (format t "usage: lob [--version] [--help] [-v | --verbose]
           [-I <path>] [-o <path>] [-g] [-d]
           [-e | --entry-point [<package>:[:]]<name>] [-s <system-name>]
           <path>...~2%")
                (return-from cl-user::main 0))
               ((string= elt "--version")
                (format t "1.0.0~%")
                (return-from cl-user::main 0))
               ((or (string= elt "-e")
                    (string= elt "--entry-point"))
                (let ((next (cdr cell)))
                  (unless next
                    (lose (format nil "missing argument for ~A" elt)))
                  (let* ((name (car next))
                         (colon (position #\: name))
                         (package-name (if colon (subseq name 0 colon)))
                         (name-start (if colon
                                         (let ((second-colon (position #\: name :start (1+ colon))))
                                           (if second-colon
                                               (prog1 (1+ second-colon)
                                                 (when (position #\: name :start (1+ second-colon))
                                                   (lose (format nil "invalid entry point name - too many colons: ~A" name))))
                                               (1+ colon)))
                                         0))
                         (symbol-name (subseq name name-start)))
                    (setf toplevel-package-name package-name
                          toplevel-symbol-name symbol-name
                          cell (cdr next)))))
               ((string= elt "-g")
                (setf gui t
                      cell (cdr cell)))
               ((string= elt "-o")
                (let ((next (cdr cell)))
                  (unless next
                    (lose (format nil "missing argument for ~A" elt)))
                  (setf output-path (car next)
                        cell (cdr next))))
               ((string= elt "-s")
                (let ((next (cdr cell)))
                  (unless next
                    (lose (format nil "missing argument for ~A" elt)))
                  (push (make-instance 'system-name :name (car next)) loaded-things)
                  (setf cell (cdr next))))
               ((string= elt "-I")
                (let ((next (cdr cell)))
                  (unless next
                    (lose (format nil "missing argument for ~A" elt)))
                  (push (truename (uiop:ensure-directory-pathname (car next))) include-directories)
                  (setf cell (cdr next))))
               ((string= elt "-d")
                (setf debug-build t
                      cell (cdr cell)))
               ((or (string= elt "-v")
                    (string= elt "--verbose"))
                (setf verbose t
                      cell (cdr cell)))
               ((or (string= elt "-" :end1 (min 1 (length elt)))
                    (string= elt "--" :end1 (min 2 (length elt))))
                (lose (format nil "unknown option: ~A" elt)))
               (t
                (push elt loaded-things)
                (setf cell (cdr cell))))
          :finally
             (setf include-directories (nreverse include-directories)
                   loaded-things (nreverse loaded-things))
             (unless loaded-things
               (lose "no input files"))
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
        (lose (format nil "~A" e))))))
