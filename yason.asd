;; This file is part of yason, a Common Lisp JSON parser/encoder
;;
;; Copyright (c) 2008 Hans Hübner
;; All rights reserved.
;;
;; Please see the file LICENSE in the distribution.

;;;; -*- Mode: LISP -*-

(in-package :cl-user)

(defpackage :yason.system
  (:use :cl :asdf))

(in-package :yason.system)

(defsystem :yason
  :name "yason"
  :author "Hans Huebner <hans@huebner.org>"
  :version "1"
  :maintainer "Hans Huebner <hans@huebner.org>"
  :licence "BSD"
  :description "JSON parser/encoder"
  :long-description ""

  :components ((:file "package")
	       (:file "encode" :depends-on ("package"))
	       (:file "parse" :depends-on ("package"))))
