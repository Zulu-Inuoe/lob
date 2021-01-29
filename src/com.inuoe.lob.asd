(defsystem #:com.inuoe.lob
  :version "0.1.0"
  :description "A command-line tool for building"
  :author "Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>"
  :license "MIT"
  :components ((:static-file "uiop.lisp")
               (:static-file "asdf.lisp" :depends-on ("uiop.lisp"))
               (:file "lob-build" :depends-on ("uiop.lisp" "asdf.lisp"))
               (:file "lob-new")
               (:file "lob" :depends-on ("lob-build" "lob-new")))
  :depends-on
  ())
