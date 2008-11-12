;;;; -*- Mode: LISP -*-

(in-package :cl-user)

(defpackage :yason.system
  (:use :cl :asdf))

(in-package :yason.system)

(defsystem :yason
  :name "YASON"
  :author "Hans Huebner <hans@huebner.org>"
  :version "0"
  :maintainer "Hans Huebner <hans@huebner.org>"
  :licence "BSD"
  :description "JSON parser/encoder"
  :long-description ""

  :depends-on ()

  :components ((:file "package")
	       (:file "encode" :depends-on ("package"))
	       (:file "parse" :depends-on ("package"))))
