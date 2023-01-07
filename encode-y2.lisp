;; This file is part of yason, a Common Lisp JSON parser/encoder
;;
;; Copyright (c) 2022-2023 Philipp Marek and contributors
;; All rights reserved.
;;
;; Please see the file LICENSE in the distribution.

(in-package :yason-2)

(defvar *json-output*)


;;; I pondered some time whether to have CONTEXT be a CLOS instance or structure
;;; that contains all the required callbacks, for performance reasons;
;;; but if users do
;;;
;;;    (defparameter *my-context* (make-instance ...))
;;;    (defmethod yason-2:encode ((ctx (eql *my-context*)) ...) )
;;;
;;; any redefinition of *MY-CONTEXT* (which would be needed to get
;;; callbacks referenced via #' updated!) means that all the DEFMETHODs
;;; need to be redone, too; and that's too bothersome.
;;;
;;; So it's a plain, simple CLOS implementation that's easy to augment.

(defgeneric encode (context object stream)
  (:documentation 
  "Encode OBJECT to STREAM in JSON format.
  CONTEXT, if NIL, gets translated to HASH-TABLE+VECTOR+CL-BOOLS,
  which is an empty class composed of some mixins;
  by passing a different class you can override the behaviour."))

(defgeneric encode-dict-key (context key)
  (:documentation
   "Returns KEY for an object/dictionary as a string.")
  (:method (ctx (key string))
    (princ-to-string key)))

;;; ---------------------------------

(defclass cl-bools () ())

(defmethod encode ((ctx cl-bools) (obj (eql t)) stream)
  (princ "true" stream))

(defmethod encode ((ctx cl-bools) (obj (eql nil)) stream)
  (princ "false" stream))

;;; ---------------------------------

(defclass hash-table-to-obj-mixin () ())

(defun encode-key/value (key key-ctx value value-ctx stream)
  (let ((key-string (encode-dict-key key-ctx key)))
    (check-type key-string string)
    (encode key-ctx key-string stream))
  (write-char #\: stream)
  (when (and (typep stream 'json-output-stream)
             (indent stream))
    (write-char #\space stream))
  (encode value-ctx value stream))

(defmethod encode ((ctx hash-table-to-obj-mixin) (obj hash-table) stream)
  (if (zerop (hash-table-count obj))
      (write-string "{}" stream)
      (with-aggregate/object (stream #\{ #\})
        (maphash (lambda (key value)
                   (with-element-output ()
                     (encode-key/value key ctx value ctx stream)))
                 obj)))
  obj)

;;; ---------------------------------

(defclass lists-as-vector-mixin () ())

(defmethod encode ((ctx lists-as-vector-mixin) (obj list) stream)
  (with-aggregate/object (stream #\[ #\])
    (dolist (value obj)
      (with-element-output ()
        (encode ctx value stream)))
    obj))

;;; ---------------------------------
(defclass json-numbers () ())

;; For FLOATs and RATIOs
(defmethod encode ((ctx json-numbers) (num real) stream)
  (let ((*read-default-float-format* 'double-float))
    (format stream "~f" (coerce num 'double-float))))

(defmethod encode ((ctx json-numbers) (num integer) stream)
  (princ num stream))

;;; ---------------------------------
(defmethod encode (ctx (string string) stream)
  (write-char #\" stream)
  (escape-string-to-stream string stream)
  (write-char #\" stream))


;;; ---------------------------------

(defclass symbol-names-as-key-mixin ()
  ((char-case-fn :type function
                 :initform #'identity
                 :initarg :symbol-name-as-key-char-case-fn
                 :reader s-n-a-k-m-c-c-f))
  (:documentation
   "Use symbols' names as keys of JSON objects.
   :SYMBOL-NAME-AS-KEY-CHAR-CASE-FN is #'IDENTITY,
   but could also be #'STRING-UPCASE or similar."))

(defmethod encode-dict-key ((ctx symbol-names-as-key-mixin) (key symbol))
  (funcall (s-n-a-k-m-c-c-f ctx)
           (symbol-name key)))


(defclass symbol-names-with-package-as-key-mixin ()
  ((package-symname-delimiter :type string 
                              :initform "::"
                              :reader s-n-p-p-a-k-m--p-s-d)))

(defmethod encode-dict-key ((ctx symbol-names-with-package-as-key-mixin) (key symbol))
  (concatenate 'string
               (symbol-name (symbol-package key))
               (s-n-p-p-a-k-m--p-s-d ctx)
               (symbol-name key)))


;;; ---------------------------------

(defclass integers-as-key-mixin () ())

(defmethod encode-dict-key ((ctx integers-as-key-mixin) (key integer))
  (write-to-string key))


;;; ---------------------------------

(defclass NIL-as-json-null-mixin () ())

(defmethod encode ((ctx NIL-as-json-null-mixin) (key (eql nil)) stream)
  (princ "null" stream))


#+(or) (
(defclass unbound-as-json-null-mixin () ())

(defmethod encode ((ctx unbound-as-json-null-mixin) 
                   ;; not bound???!?
                   (key (satisfies #'boundp))
                   stream)
  (princ "null" stream))
  )


;;; ---------------------------------

(defclass lists-as-vector-mixin () ())

(defmethod encode ((ctx lists-as-vector-mixin) (obj list) stream)
  (with-aggregate/object (stream #\[ #\])
    (dolist (value obj)
      (with-element-output ()
        (encode ctx value stream)))
    obj))

;;; ---------------------------------

(defmethod encode (ctx (vec vector) stream)
  (with-aggregate/object (stream #\[ #\])
    (loop for value across vec
          do (with-element-output ()
               (encode ctx value stream)))))

;;; ---------------------------------
;;; TODO 

(defmethod slots-to-encode ((obj standard-object))
  (closer-mop:class-slots (class-of obj)))

(defclass CLOS-obj-to-json-obj-mixin ()
  ((slot-name-ctx :initarg :clos-obj-slot-name-encode-ctx
                   :initform (make-instance 'symbol-names-as-key-mixin)
                   :reader s-n-ctx))
  (:documentation 
   "Encodes slots that are returned by the GF SLOTS-TO-ENCODE 
   for the CLOS object by using the context
   :CLOS-OBJ-SLOT-NAME-ENCODE-CTX on the slot name symbol,
   and by running ENCODE on the slot value recursively."))

(defmethod encode ((ctx CLOS-obj-to-json-obj-mixin) (obj standard-object) stream)
  (with-aggregate/object (stream #\{ #\})
    (dolist (slot (slots-to-encode obj))
      (with-element-output ()
        (let ((slot-name (closer-mop:slot-definition-name slot)))
          (encode-key/value 
            slot-name 
            (s-n-ctx ctx)
            (slot-value obj (closer-mop:slot-definition-name slot))
            ctx 
            stream))))))

;;; ---------------------------------

(defclass HASH-TABLE+CL-BOOLS+NUMBERS+NIL-class
  ( ; report-duplicate-key-mixin
    cl-bools json-numbers
    hash-table-to-obj-mixin lists-as-vector-mixin
    ;; Default only for integers -- FLOATs might be rounded and/or
    ;; printed differently and so might not survive a round-trip.
    integers-as-key-mixin
    NIL-as-json-null-mixin
    ;;obj-to-hash-table-mixin vector-to-list-mixin
    )
  ())

(defparameter HASH-TABLE+CL-BOOLS+NIL
  (make-instance 'HASH-TABLE+CL-BOOLS+NUMBERS+NIL-class)
  "Default serialization context class.")

(defparameter default-context HASH-TABLE+CL-BOOLS+NIL
  "Default serialization context.")


(defclass default+clos-objects-context-class 
  (HASH-TABLE+CL-BOOLS+NUMBERS+NIL-class
    CLOS-obj-to-json-obj-mixin)
  ())

(defparameter default+clos-objects-context
  (make-instance 'default+clos-objects-context-class)
  "Serialization context class including CLOS objects.")



(defmethod encode ((ctx (eql nil)) obj stream)
  (encode default-context
          obj
          stream))

;; TODO: default includes numbers => string for keys?


(defclass test () ((|xYz| :initform 4)))

#+(or)
(yason2:with-output-to-string* ()
  (yason-2:encode default+clos-objects-context
                  (list 1 2 
                        #((1 2 3) (4 5 6))
                        "aa" 
                        (sqrt 2)
                        (make-instance 'test)
                        (alexandria:plist-hash-table 
                          `("a" 5/81 2 ,(coerce 5.31 'double-float))))
                  yason::*json-output*))
