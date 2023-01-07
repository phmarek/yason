;; This file is part of yason, a Common Lisp JSON parser/encoder
;;
;; Copyright (c) 2022-2023 Philipp Marek and contributors
;; All rights reserved.
;;
;; Please see the file LICENSE in the distribution.

(in-package :yason-2)


(defclass parse-context ()
  ((depth :type fixnum
          :initform 0
          :accessor ctx-depth)))

(defgeneric parse-true (context)
  (:method ((ctx cl-bools))
    't))

(defgeneric parse-false (context)
  (:method ((ctx cl-bools))
    'nil))

(defgeneric parse-null (context)
  (:method ((ctx cl-bools))
    'yason:null))

;; TODO: parse-string, parse-number GF?

(defgeneric collect-json-object (ctx k-v-callback)
  (:documentation
  "Collect data from a JSON object;
  K-V-CALLBACK returns the KEY and a VALUE as two values,
  or NIL when done."))


(define-condition cannot-convert-key (error)
  ((key-string :initarg :key-string
               :reader key-string))
  (:report (lambda (c stream)
             (format stream "cannot convert key ~S used in JSON object to hash table key"
                     (key-string c)))))

(define-condition duplicate-key (error)
  ((key :initarg :key
        :reader key))
  (:report (lambda (c stream)
             (format stream "Duplicate obj key ~S"
                     (key-string c)))))


(defclass obj-key-keep-string-mixin () ())

(defclass obj-key-to-symbol-mixin ()
  ((package :initarg target-package
            :reader obj-key-tgt-pkg
            :type (or string symbol package))))

(defclass obj-key-to-existing-symbol-mixin (obj-key-to-symbol-mixin) ())


(defclass obj-to-alist-mixin () ())
(defclass obj-to-plist-mixin () ())

(defgeneric parse-obj (context key-value-callback)
  (:documentation
   "Parsing a Javascript object:
   Getting the KEY and VALUE as two values from
   KEY-VALUE-CALLBACK, build a result according to CONTEXT;
   KEY will be a string until the JSON object
   is exhausted, when it will be NIL."))

(defgeneric transform-obj-key (context key)
  (:documentation
   "Transforms a string KEY as needed."))

(defmethod transform-obj-key ((ctx obj-key-keep-string-mixin) (key string))
  key)

(defmethod transform-obj-key ((ctx obj-key-to-existing-symbol-mixin) (key string))
  (when key
    (let ((sym (find-symbol key (obj-key-tgt-pkg ctx))))
      (if sym
          sym
          (make-condition 'cannot-convert-key 
                          :key-string key)))))

(defmethod transform-obj-key ((ctx obj-key-to-symbol-mixin) (key string))
  (when key
    (intern key (obj-key-tgt-pkg ctx))))


;;; ---------------------------------

(defclass obj-to-hash-table-mixin ()
  ((ht-options :initform '(:test equal)
               :initarg :obj-to-ht-initargs
               :reader obj-to-ht-initargs)))

;; We test for a superclass, and not an (overridden)
;; member slot in the context, because this way
;; the _order_ doesn't matter - ie. the user can
;; define their own class via
;;   (defclass my-context (ignore-duplicate-... ...) ...)
;; or
;;   (defclass my-context (... ignore-duplicate-...) ...)
;; and both works.
(defclass ignore-duplicate-object-keys ()
  ())

(defmacro %collect-json-object ((ht-args dupl?) &body body)
  (alexandria:once-only (ht-args dupl?)
    ;; The hash is used in every case to quickly detect duplicates.
    `(let ((hash (apply #'make-hash-table ,ht-args)))
       (loop for (k v) = (multiple-value-list (funcall cb))
             while k
             when (and ,dupl?
                       (nth-value 1 (gethash k hash)))
             do (signal 'duplicate-key
                        :key k)
             do (setf (gethash k hash)
                      v)
             ,@ body))))

(defmethod collect-json-object ((ctx obj-to-hash-table-mixin) cb)
  (%collect-json-object ((obj-to-ht-initargs ctx)
                         (typep ctx 'ignore-duplicate-object-keys))
    finally (return hash)))

(defmethod collect-json-object ((ctx obj-to-alist-mixin) cb)
  (%collect-json-object ('(:test equal)
                         (typep ctx 'ignore-duplicate-object-keys))
    collect (cons k v)))

(defmethod collect-json-object ((ctx obj-to-plist-mixin) cb)
  (%collect-json-object ('(:test equal)
                         (typep ctx 'ignore-duplicate-object-keys))
    collect k
    collect v))

;;; ---------------------------------

(defclass json-array-as-vector ()
  ((overall-type :initform '(array t (*))
                 :reader overall-type
                 :initarg :array-overall-type)))

;; TODO: allow to specify an array type?
(defgeneric collect-json-array (ctx element-callback)
  (:documentation
  "Collect data from a JSON array;
  ELEMENT-CALLBACK returns an element and T as values,
  or NIL NIL when done."))

(defmethod collect-json-array ((ctx json-array-as-vector) element-callback)
  (loop for (el found?) = (multiple-value-list (funcall element-callback))
        while found?
        collect el into vals
        finally (return
                  (if vals
                      (coerce vals
                              (overall-type ctx))
                      #()))))

(defun parse-string-2 (ctx input)
  (declare (ignore ctx))
  (yason-1::parse-string input))

(defun parse-number-2 (ctx input)
  (declare (ignore ctx))
  (yason-1::parse-number input))

;;; ---------------------------------

(defun parse-constant-2 (ctx input)
  (let ((chars (make-array 5 
                           :fill-pointer 4
                           :element-type 'character
                           :initial-contents (list (read-char input)
                                                   (read-char input)
                                                   (read-char input)
                                                   (read-char input)
                                                   #\.)
                           )))
    (flet ((inv ()
             (error "Invalid JSON constant ~s" chars)))
      (cond
        ((string-equal chars "true")
         (parse-true ctx))
        ((string-equal chars "null")
         (parse-null ctx))
        ((string-equal chars "fals")
         (let ((ch (read-char input)))
           (setf (aref chars 4) ch)
           (if (eql ch #\e)
               (parse-false ctx)
               (inv))))
        (t
         (inv))))))

(defun parse-object-2 (ctx input)
  ;; read open brace
  (read-char input)
  (incf (ctx-depth ctx))
  (let ((need-comma? nil))
    (labels
        ((expct (x &optional (eat t))
           (let ((ch (yason-1::peek-char-skipping-whitespace input)))
             (unless (eql ch x)
               (error "Expected ~s, got ~s" x ch))
             (when eat
               (read-char input))))
         (next ()
           (cond
             ((eql (yason1::peek-char-skipping-whitespace input)
                   #\})
              (values nil nil))
             (t
              (when need-comma?
                (expct #\,))
              (expct #\" nil)
              (let ((key (parse-string-2 ctx input)))
                (expct #\:)
                (let ((val (parse ctx input)))
                  (setf need-comma? t)
                  (values key
                          val)))))))
      (prog1
          (collect-json-object ctx #'next)
        ;; read closing bracket
        (read-char input)
        (decf (ctx-depth ctx))))))

(defun parse-array-2 (ctx input)
  ;; read open bracket
  (read-char input)
  (incf (ctx-depth ctx))
  (let ((need-comma? nil))
    (flet ((next ()
             (cond
               ((eql (yason1::peek-char-skipping-whitespace input)
                     #\])
                (values nil nil))
               (t
                (when need-comma?
                  (let ((ch (yason1::peek-char-skipping-whitespace input)))
                    (unless (eql ch #\,)
                      (error "Expected comma, got ~s" ch)))
                  (read-char input))
                (let ((val (parse ctx input)))
                  (setf need-comma? t)
                  (values val
                          t))))))
      (prog1
          (collect-json-array ctx #'next)
        ;; read closing bracket
        (read-char input)
        (decf (ctx-depth ctx))))))

;;; ---------------------------------

(defclass default-parse-class (parse-context
                                json-array-as-vector 
                                obj-to-hash-table-mixin
                                obj-key-keep-string-mixin
                                cl-bools)
  ())

(defparameter default-parse-ctx (make-instance 'default-parse-class))

(defclass default-alist-class (parse-context
                                json-array-as-vector 
                                obj-to-alist-mixin
                                obj-key-keep-string-mixin
                                cl-bools)
  ())

(defparameter alist-parse-ctx (make-instance 'default-alist-class))

;;; ---------------------------------


(defgeneric parse (ctx input)
  (:method ((ctx (eql nil)) stream)
    (parse default-parse-ctx stream))
  (:method ((ctx parse-context) (input stream))
    (ecase (yason-1::peek-char-skipping-whitespace input)
      (#\"
       (parse-string-2 ctx input))
      ((#\+ #\- #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (parse-number-2 ctx  input))
      (#\{
       (parse-object-2 ctx input))
      (#\[
       (parse-array-2 ctx input))
      ((#\t #\f #\n)
       (parse-constant-2 ctx input))))
  (:method (ctx (input pathname))
    (with-open-file (stream input)
      (parse ctx stream)))
  (:method (ctx (input string))
    (parse ctx (make-string-input-stream input))))

#+(or)
(with-input-from-string 
  (s "[1,\"aaa\",{\"c\":6, \"f\":[null, true, false]}]")
  #+(or)
  (trace "YASON-2")
  (prog1
      (parse alist-parse-ctx s)
    (untrace "YASON-2")))
