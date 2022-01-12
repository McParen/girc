(asdf:defsystem #:girc
  :name "girc"
  :description "Simple IRC client for the terminal"
  :author "Anton Vidovic <anton.vidovic@gmx.de>"
  :maintainer "Anton Vidovic"
  :licence "MIT"
  :version "0.0.1"
  :depends-on (:alexandria :split-sequence :usocket :croatoan)
  :pathname "src/"
  :serial t
  :components

  ((:file "package")
   (:file "global")
   (:file "parse")
   (:file "ui")
   (:file "buffer")
   (:file "connection")
   (:file "protocol")
   (:file "numerics")
   (:file "event")
   (:file "command")
   (:file "girc")))
