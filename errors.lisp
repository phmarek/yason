;; This file is part of yason, a Common Lisp JSON parser/encoder
;;
;; Copyright (c) 2008-2012 Hans Huebner and contributors
;; All rights reserved.
;;
;; Please see the file LICENSE in the distribution.

(in-package :yason)

(define-condition yason-error (error) ())

(define-condition yason-parse-error (yason-error) ())

(define-condition encode-error (yason-error) ())

(define-condition invalid-constant-error (yason-parse-error) ())

(define-condition invalid-key-error (yason-parse-error)
  ((key-string :initarg :key-string
               :reader key-string)))

(define-condition colon-expected-error (yason-parse-error)
  ((key-string :initarg :key-string
               :reader key-string)))

(define-condition output-context-error (encode-error)
  ()
  (:documentation "This condition is signalled when one of the stream
  encoding function is used outside the dynamic context of a
  WITH-OUTPUT or WITH-OUTPUT-TO-STRING* body."))

(defmacro print-condition (stream control-string &rest format-arguments)
  `(if *print-escape*
       (call-next-method)
       (format, stream ,control-string ,@format-arguments)))

;;;; Printing errors

(defparameter +invalid-key-error-message+
  "Could not convert key ~S used in JSON object to hash table key:
calling *parse-object-key-fn* on it failed.")

(defparameter +colon-expected-error-message+ "A colon was expected following key ~S.")

(defparameter +output-context-error-message+ "No JSON output context is active.")

(defmethod print-object ((object invalid-key-error) stream)
  (print-condition stream +invalid-key-error-message+ (key-string object)))

(defmethod print-object ((object colon-expected-error) stream)
  (print-condition stream +colon-expected-error-message+ (key-string object)))

(defmethod print-object ((object output-context-error) stream)
  (print-condition stream +output-context-error-message+))
