;;;; cl-fuzzer.asd

(asdf:defsystem #:cl-fuzzer
  :description "Describe cl-fuzzer here"
  :author "k1d77a"
  :license  "MIT"
  :version "0.0.1"
  :depends-on (:cl-ppcre
               :usocket)
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "string-generation")
               (:file "cl-fuzzer")))
