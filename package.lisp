;; This file is part of yason, a Common Lisp JSON parser/encoder
;;
;; Copyright (c) 2008-2019 Hans Huebner and contributors
;; All rights reserved.
;;
;; Please see the file LICENSE in the distribution.

(defpackage :yason

  (:use :cl)
  (:nicknames :yason-1 :yason1)

  (:export
   ;; Parser
   #:parse
   #:*parse-object-key-fn*
   #:*parse-object-as*
   #:*parse-object-as-alist* ; deprecated
   #:*parse-json-arrays-as-vectors*
   #:*parse-json-booleans-as-symbols*
   #:*parse-json-null-as-keyword*

   #:true
   #:false
   #:null

   ;; Basic encoder interface
   #:encode
   #:encode-slots
   #:encode-object
   #:encode-plist
   #:encode-alist
   #:encode-plain-list-to-array
   #:*list-encoder*
   #:*symbol-encoder*
   #:*symbol-key-encoder*
   #:encode-symbol-as-lowercase
   #:encode-symbol-as-string

   #:make-json-output-stream

   ;; Streaming encoder interface
   #:with-output
   #:with-output-to-string*
   #:no-json-output-context
   #:with-array
   #:encode-array-element
   #:encode-array-elements
   #:with-object
   #:encode-object-element
   #:encode-object-elements
   #:encode-object-slots
   #:with-object-element))

(delete-package :yason-2)
(defpackage :yason-2
  (:nicknames :yason2)
  (:use :cl)
  (:import-from :yason-1
                yason-1:null
                yason-1:true
                yason-1:false

                yason-1::with-aggregate/object
                yason-1::with-element-output
                yason-1::with-output-to-string*
                yason-1::with-output
                yason-1::escape-string-to-stream
                yason-1::json-output-stream
                yason-1::indent

                yason-1::parse-string
                )
  (:export #:encode
           #:with-output-to-string*
           #:indentation-mixin
           #:compact-mixin
           #:hash-table-to-dict-mixin
           #:obj-to-dict-mixin
           #:lists-as-vector-mixin

           #:parse
           #:default-parse-ctx
           #:alist-parse-ctx

           #:parse-context
           #:obj-key-keep-string-mixin
           #:obj-key-to-symbol-mixin
           #:obj-key-to-existing-symbol-mixin
           #:obj-to-alist-mixin
           #:obj-to-plist-mixin
           #:obj-to-hash-table-mixin
           #:ignore-duplicate-object-keys
           #:json-array-as-vector
           #:default-parse-class
           #:default-alist-class
  ))
