(cl:in-package #:asdf-user)

(defsystem #:cl-filenames-intrinsic
  :serial t
  :components
  ((:file "package-intrinsic")
   (:file "cl-filenames")))
