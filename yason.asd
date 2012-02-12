;;;; -*- Mode: Lisp -*-

;; This file is part of yason, a Common Lisp JSON parser/encoder
;;
;; Copyright (c) 2008, 2011 Hans Huebner and contributors
;; All rights reserved.
;;
;; Please see the file LICENSE in the distribution.

(in-package :cl-user)

(defpackage :yason.system
  (:use :cl :asdf))

(in-package :yason.system)

(defmethod perform ((op test-op) (c (eql (find-system :yason))))
  (oos 'test-op 'yason-tests))

;;; A tester's job is never done!
(defmethod operation-done-p ((op test-op) (c (eql (find-system :yason))))
  nil)

(defsystem :yason
  :name "YASON"
  :author "Hans Huebner <hans@huebner.org>"
  :version "0.4.0"
  :maintainer "Hans Huebner <hans@huebner.org>"
  :licence "BSD"
  :description "JSON parser/encoder"
  :long-description "YASON is a Common Lisp library for encoding and
    decoding data in the JSON interchange format.  JSON is used as a
    lightweight alternative to XML.  YASON has the sole purpose of
    encoding and decoding data and does not impose any object model on
    the Common Lisp application that uses it."

  :depends-on (:alexandria :trivial-gray-streams)
  :in-order-to ((test-op (load-op :yason-test)))
  :components ((:file "package")
	       (:file "encode" :depends-on ("package"))
	       (:file "parse" :depends-on ("package"))
               (:file "test")))

(defsystem :yason-test
  :name "yason-test"
  :description "YASON tests"

  :depends-on (:unit-test :yason :alexandria)
  :components ((:file "test")))
