(in-package #:asdf/user)

(defsystem "cl-tfnsw-gtfs"
  :description ""
  :version "0.1.0"
  :author "Jong-won Choi<oz.jongwon.choi@gmail.com>"
  :license "MIT License"
  :defsystem-depends-on (protobuf split-sequence)
  :components ((:module "src"
                        :components
                        ((:static-file "Makefile")
                         (:static-file "README.md")
                         (:protobuf-source-file "tfnsw-gtfs-realtime")
                         (:file "package" :depends-on ("tfnsw-gtfs-realtime"))
                         (:file "realtime-bus" :depends-on ("package" "tfnsw-gtfs-realtime"))))))
