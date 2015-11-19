(asdf:defsystem #:girc
    :name "girc"
    :description "Simple IRC client for the terminal"
    :author "Anton Vidovic <anton.vidovic@gmx.de>"
    :maintainer "Anton Vidovic"
    :licence "GPLv3"
    :version "0.0.1"
    :depends-on (:split-sequence :usocket :croatoan)
;;  serial t = the dependencies are linear.
;;  :serial t
    :components ((:file "package")
                 (:file "classes")
                 (:file "parse")
                 (:file "girc")))
