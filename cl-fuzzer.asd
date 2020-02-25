;;;; cl-fuzzer.asd

(asdf:defsystem #:cl-fuzzer
  :description "Describe cl-fuzzer here"
  :author "k1d77a"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "cl-fuzzer")))
