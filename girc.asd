(asdf:defsystem #:girc
  :name "girc"
  :description "Simple IRC client for the terminal"
  :author "Anton Vidovic <anton.vidovic@gmx.de>"
  :maintainer "Anton Vidovic"
  :licence "MIT"
  :version "0.0.1"
  :depends-on (:split-sequence :usocket :croatoan)
  :pathname "src/"
  :serial t
  :components

  ((:file "package")
   (:file "parse")
   (:file "buffer")
   (:file "ui")
   (:file "connection")
   (:file "protocol")
   (:file "event")
   (:file "command")
   (:file "girc")))
