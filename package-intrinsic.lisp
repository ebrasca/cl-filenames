(cl:in-package #:common-lisp-user)

(defpackage #:cl-filenames
  (:use #:common-lisp)
  (:shadow
   #:directory
   #:type))
