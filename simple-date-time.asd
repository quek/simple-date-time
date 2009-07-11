;;;; -*- Mode: LISP; -*-
(asdf:defsystem :simple-date-time
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "date-time"))
  :depends-on (cl-ppcre))
