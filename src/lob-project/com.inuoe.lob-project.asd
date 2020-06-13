(defsystem #:com.inuoe.lob-project
  :version "0.1.0"
  :description "A command-line tool for creating projects"
  :author "Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>"
  :license "MIT"
  :components ((:file "lob-project")
               (:file "main" :if-feature :lob))
  :depends-on
  (#:uiop))
