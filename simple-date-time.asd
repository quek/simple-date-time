;;;; -*- Mode: LISP; -*-
(asdf:defsystem :simple-date-time
  :version "0.1.0"
  :description "date and time library for common lisp"
  :author "TAHARA Yoshinori <read.eval.print@gmail.com>"
  :license "BSD"
  :serial t
  :components ((:file "package")
               (:file "date-time"))
  :depends-on (cl-ppcre))
