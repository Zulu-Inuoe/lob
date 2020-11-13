(defpackage #:com.inuoe.lob
  (:use #:cl)
  (:import-from #:com.inuoe.lob-build)
  (:import-from #:com.inuoe.lob-new)
  (:import-from #:com.inuoe.lob-test)
  (:local-nicknames
   (#:build #:com.inuoe.lob-build)
   (#:new #:com.inuoe.lob-new)
   (#:test #:com.inuoe.lob-test))
  (:export
   #:main))

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

(defmacro lose (str &rest args)
  `(progn
     (format *error-output* ,str ,@args)
     (finish-output *standard-output*)
     (finish-output *error-output*)
     (return 1)))

(defparameter *lob-new-usage* "usage: lob new [--version] [--help]
               [-n <name>] [-a <author>] [-l <license>] [-d <description>]
               <dir>")

(defun main-new (argv)
  "Entry point for `new' subcommand"
  (loop
    :with dir := nil
    :with name := nil
    :with prefix := nil
    :with description := nil
    :with author := nil
    :with license := nil
    :with verbose := nil
    :with cell := (cdr argv)
    :for elt := (car cell)
    :while cell
    :do
       (flet ((take-arg ()
                (setf cell (cdr cell))
                (unless cell
                  (lose "lob new: missing argument for ~A" elt))
                (car cell)))
         (string-case elt
           ("--help"
            (write-line *lob-new-usage*)
            (return 0))
           ("--version"
            (format t "0.1.0~%")
            (return 0))
           ("-n"
            (setf name (take-arg)))
           ("-a"
            (setf author (take-arg)))
           ("-l"
            (setf license (take-arg)))
           ("-d"
            (setf description (take-arg)))
           (("-v" "--verbose")
            (setf verbose t))
           (t
            (setf dir elt)))
         (setf cell (cdr cell)))
    :finally
       (unless dir
         (lose "lob new: missing output directory"))
       (let ((new:*lob-new-stdout* (if verbose *standard-output* (make-broadcast-stream))))
         (return (new:make-project dir :name name :prefix prefix :description description
                                                     :author author :license license)))))

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

(defparameter *lob-usage* "usage:
  lob <command> [flags]
  lob [--version] [--help] [-v | --verbose]
      [-I <path>] [-o <path>] [-g] [-d] [--format-error <format-string>]
      [-e | --entry-point [<package>:[:]]<name>] [-l <system-name>]
      <path>...")

(defun main-build (argv)
  "Main entry point for plain lob command"
  (loop
    :with toplevel-package-name := nil
    :with toplevel-symbol-name := nil
    :with loaded-things := ()
    :with output-path := nil
    :with gui := nil
    :with include-directories := ()
    :with debug-build := nil
    :with format-error := t
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
            (write-line *lob-usage*)
            (return 0))
           ("--version"
            (format t "0.1.0~%")
            (return 0))
           (("-e" "--entry-point")
            (setf (values toplevel-package-name toplevel-symbol-name)
                  (parse-entry-name (take-arg))))
           ("-g"
            (setf gui t))
           ("-o"
            (setf output-path (take-arg)))
           ("-l"
            (push (make-instance 'build:system-name :name (take-arg)) loaded-things))
           ("-I"
            (push (truename (uiop:ensure-directory-pathname (take-arg))) include-directories))
           ("-d"
            (setf debug-build t))
           ("--format-error"
            (setf format-error
                  (let ((arg (take-arg)))
                    (cond
                      ((string-equal arg "true") t)
                      ((string-equal arg "false") nil)
                      (t arg)))))
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
       (return (let ((build:*lob-stdout* (if verbose *standard-output* (make-broadcast-stream))))
                 (build:build :image (executable-find "sbcl")
                        :gui gui
                        :toplevel-symbol-name toplevel-symbol-name
                        :toplevel-package-name toplevel-package-name
                        :loaded-things loaded-things
                        :output-path output-path
                        :debug-build debug-build
                        :format-error format-error
                              :additional-source-registry include-directories)))))

(defun main-run (argv)
  "Main entry point for run lob command"
  (loop
    :with toplevel-package-name := nil
    :with toplevel-symbol-name := nil
    :with loaded-things := ()
    :with include-directories := ()
    :with debug := nil
    :with format-error := t
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
            (write-line *lob-usage*)
            (return 0))
           ("--version"
            (format t "0.1.0~%")
            (return 0))
           (("-e" "--entry-point")
            (setf (values toplevel-package-name toplevel-symbol-name)
                  (parse-entry-name (take-arg))))
           ("-l"
            (push (make-instance 'run:system-name :name (take-arg)) loaded-things))
           ("-I"
            (push (truename (uiop:ensure-directory-pathname (take-arg))) include-directories))
           ("-d"
            (setf debug t))
           ("--format-error"
            (setf format-error
                  (let ((arg (take-arg)))
                    (cond
                      ((string-equal arg "true") t)
                      ((string-equal arg "false") nil)
                      (t arg)))))
           (("-v" "--verbose")
            (setf verbose t))
           (t
            (when (or (string= elt "-" :end1 (min 1 (length elt)))
                      (string= elt "--" :end1 (min 2 (length elt))))
              (lose "lob: unknown option: ~A" elt))
            (push elt loaded-things)

            ;; Start
            (loop-finish)))

         (setf cell (cdr cell)))
    :finally
       (setf include-directories (nreverse include-directories)
             loaded-things (nreverse loaded-things))
       (unless loaded-things
         (lose "lob: no input files"))
       (return (let ((run:*lob-stdout* (if verbose *standard-output* (make-broadcast-stream))))
                 (run:run :image "sbcl"
                          :argv cell
                          :toplevel-symbol-name toplevel-symbol-name
                          :toplevel-package-name toplevel-package-name
                          :loaded-things loaded-things
                          :debug debug
                          :format-error format-error
                          :additional-source-registry include-directories)))))

(defun main (&rest argv)
  (string-case (second argv)
    ("--help"
     (write-line *lob-usage*)
     0)
    ("new"
     (main-new (cdr argv)))
    ("run"
     (main-run (cdr argv)))
    (t
     (main-build argv))))
