(defsystem #:com.inuoe.lob
  :version "0.1.0"
  :description "A command-line tool for building"
  :author "Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>"
  :license "MIT"
  :class :package-inferred-system
  :components ((:static-file "uiop.lisp")
               (:static-file "asdf.lisp"))
  :depends-on
  (#:com.inuoe.lob/lob))
