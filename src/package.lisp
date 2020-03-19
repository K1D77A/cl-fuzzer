;;;; package.lisp

(defpackage #:cl-fuzzer
  (:use #:cl #:cl-ppcre)
  (:export #:generate-fuzzy-list
           #:fuzzy-list-to-fuzzy-string
           #:fuzz))
