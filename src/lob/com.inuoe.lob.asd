(defsystem #:com.inuoe.lob
  :version "0.1.0"
  :description "A command-line tool for building"
  :author "Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>"
  :license "MIT"
  :components ((:file "lob")
               (:file "main" :if-feature :lob))
  :depends-on
  (#:uiop))
