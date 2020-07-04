(asdf:defsystem #:girc
  :name "girc"
  :description "Simple IRC client for the terminal"
  :author "Anton Vidovic <anton.vidovic@gmx.de>"
  :maintainer "Anton Vidovic"
  :licence "MIT"
  :version "0.0.1"
  :depends-on (:split-sequence :usocket :croatoan)
  ;; :serial t
  :components ((:module "src"
                :components ((:file "package")
                             (:file "classes")
                             (:file "parse")
                             (:file "connection")
                             (:file "protocol")
                             (:file "event")
                             (:file "command")
                             (:file "girc")))))
