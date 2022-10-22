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

;; Specify how to build executables by calling standard asdf operators.
;; (asdf:make :girc/build)
;; (asdf:operate :program-op :girc/build)
;; sbcl --eval "(asdf:make :girc/build)"

(asdf:defsystem "girc/build"
  :depends-on ("girc")
  :build-operation program-op
  :build-pathname "girc"
  :entry-point "girc:run")

;; if sbcl is built with binary compression (zstd or zlib) use the default
;; compression level (t) to build the binary.
#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))
