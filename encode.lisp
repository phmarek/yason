;; This file is part of yason, a Common Lisp JSON parser/encoder
;;
;; Copyright (c) 2008 Hans Hübner
;; All rights reserved.
;;
;; Please see the file LICENSE in the distribution.

(in-package :yason)

(defvar *json-output*)

(defgeneric encode (object &optional stream)

  (:documentation "Encode OBJECT to STREAM in JSON format.  May be
  specialized by applications to perform specific rendering.  STREAM
  defaults to *STANDARD-OUTPUT*."))

;; from alexandria
(defun plist-hash-table (plist &rest hash-table-initargs)
  "Returns a hash table containing the keys and values of the property list
PLIST. Hash table is initialized using the HASH-TABLE-INITARGS."
  (let ((table (apply #'make-hash-table hash-table-initargs)))
    (do ((tail plist (cddr tail)))
        ((not tail))
      (setf (gethash (car tail) table) (cadr tail)))
    table))

(defparameter *char-replacements*
  (plist-hash-table
   '(#\\ "\\\\"
     #\" "\\\""
     #\/ "\\/"
     #\Backspace "\\b"
     #\Page "\\f"
     #\Newline "\\n"
     #\Return "\\r"
     #\Tab "\\t")))

(defmethod encode ((string string) &optional (stream *standard-output*))
  (write-char #\" stream)
  (dotimes (i (length string))
    (let* ((char (aref string i))
           (replacement (gethash char *char-replacements*)))
      (if replacement
          (write-string replacement stream)
          (write-char char stream))))
  (write-char #\" stream)
  string)

(defmethod encode ((object rational) &optional (stream *standard-output*))
  (encode (float object) stream)
  object)

(defmethod encode ((object double-float) &optional (stream *standard-output*))
  (encode (coerce object 'single-float) stream)
  object)

(defmethod encode ((object float) &optional (stream *standard-output*))
  (princ object stream)
  object)

(defmethod encode ((object integer) &optional (stream *standard-output*))
  (princ object stream))

(defun encode-key/value (key value stream)
  (encode key stream)
  (write-char #\: stream)
  (encode value stream))

(defmethod encode ((object hash-table) &optional (stream *standard-output*))
  (write-char #\{ stream)
  (let (printed)
    (maphash (lambda (key value)
               (if printed
                   (write-char #\, stream)
                   (setf printed t))
               (encode-key/value key value stream))
             object))
  (write-char #\} stream)
  object)

(defmethod encode ((object vector) &optional (stream *standard-output*))
  (write-char #\[ stream)
  (let (printed)
    (loop
       for value across object
       do
       (when printed
         (write-char #\, stream))
       (setf printed t)
       (encode value stream)))
  (write-char #\] stream)
  object)

(defmethod encode ((object list) &optional (stream *standard-output*))
  (write-char #\[ stream)
  (let (printed)
    (dolist (value object)
      (if printed
          (write-char #\, stream)
          (setf printed t))
      (encode value stream)))
  (write-char #\] stream)
  object)

(defun encode-symbol/value (symbol value stream)
  (let ((string (symbol-name symbol)))
    (encode-key/value string value stream)))

(defun encode-alist (object &optional (stream *standard-output*))
  (loop initially (write-char #\{ stream)
     with printed = nil
     for (key . value) in object
     do (if printed
            (write-char #\, stream)
            (setf printed t))
       (encode-symbol/value key value stream)
     finally (write-char #\} stream)
       (return object)))

(defun encode-plist (object &optional (stream *standard-output*))
  (loop initially (write-char #\{ stream)
     with printed = nil
     for (key value . rest) on object by #'cddr
     do (if printed
            (write-char #\, stream)
            (setf printed t))
       (encode-symbol/value key value stream)
     finally (write-char #\} stream)
       (return object)))

(defmethod encode ((object (eql 'true)) &optional (stream *standard-output*))
  (write-string "true" stream)
  object)

(defmethod encode ((object (eql 'false)) &optional (stream *standard-output*))
  (write-string "false" stream)
  object)

(defmethod encode ((object (eql 'null)) &optional (stream *standard-output*))
  (write-string "null" stream)
  object)

(defmethod encode ((object (eql t)) &optional (stream *standard-output*))
  (write-string "true" stream)
  object)

(defmethod encode ((object (eql nil)) &optional (stream *standard-output*))
  (write-string "null" stream)
  object)

(defclass json-output-stream ()
  ((output-stream :reader output-stream
                  :initarg :output-stream)
   (stack :accessor stack
          :initform nil))
  (:documentation "Objects of this class capture the state of a JSON stream encoder."))

(defun next-aggregate-element ()
  (if (car (stack *json-output*))
      (write-char (car (stack *json-output*)) (output-stream *json-output*))
      (setf (car (stack *json-output*)) #\,)))

(defmacro with-output ((stream) &body body)
  "Set up a JSON streaming encoder context on STREAM, then evaluate BODY."
  `(let ((*json-output* (make-instance 'json-output-stream :output-stream ,stream)))
     ,@body))

(defmacro with-output-to-string* (() &body body)
  "Set up a JSON streaming encoder context, then evaluate BODY.
Return a string with the generated JSON output."
  `(with-output-to-string (s)
     (with-output (s)
       ,@body)))

(define-condition no-json-output-context (error)
  ()
  (:report "No JSON output context is active")
  (:documentation "This condition is signalled when one of the stream
  encoding function is used outside the dynamic context of a
  WITH-OUTPUT or WITH-OUTPUT-TO-STRING* body."))

(defmacro with-aggregate ((begin-char end-char) &body body)
  `(progn
     (unless (boundp '*json-output*)
       (error 'no-json-output-context))
     (when (stack *json-output*)
       (next-aggregate-element))
     (write-char ,begin-char (output-stream *json-output*))
     (push nil (stack *json-output*))
     (prog1
         (progn ,@body)
       (pop (stack *json-output*))
       (write-char ,end-char (output-stream *json-output*)))))

(defmacro with-array (() &body body)
  "Open a JSON array, then run BODY.  Inside the body,
ENCODE-ARRAY-ELEMENT must be called to encode elements to the opened
array.  Must be called within an existing JSON encoder context, see
WITH-OUTPUT and WITH-OUTPUT-TO-STRING*."
  `(with-aggregate (#\[ #\]) ,@body))

(defmacro with-object (() &body body)
  "Open a JSON object, then run BODY.  Inside the body,
ENCODE-OBJECT-ELEMENT or WITH-OBJECT-ELEMENT must be called to encode
elements to the object.  Must be called within an existing JSON
encoder context, see WITH-OUTPUT and WITH-OUTPUT-TO-STRING*."
  `(with-aggregate (#\{ #\}) ,@body))

(defun encode-array-element (object)
  "Encode OBJECT as next array element to the last JSON array opened
with WITH-ARRAY in the dynamic context.  OBJECT is encoded using the
ENCODE generic function, so it must be of a type for which an ENCODE
method is defined."
  (next-aggregate-element)
  (encode object (output-stream *json-output*)))

(defun encode-array-elements (&rest objects)
  "Encode OBJECTS, a list of JSON encodeable object, as array elements."
  (dolist (object objects)
    (encode-array-element object)))

(defun encode-object-element (key value)
  "Encode KEY and VALUE as object element to the last JSON object
opened with WITH-OBJECT in the dynamic context.  KEY and VALUE are
encoded using the ENCODE generic function, so they both must be of a
type for which an ENCODE method is defined."
  (next-aggregate-element)
  (encode-key/value key value (output-stream *json-output*))
  value)

(defun encode-object-elements (&rest elements)
  "Encode plist ELEMENTS as object elements."
  (loop for (key value) on elements by #'cddr
     do (encode-object-element key value)))

(defmacro with-object-element ((key) &body body)
  "Open a new encoding context to encode a JSON object element.  KEY
  is the key of the element.  The value will be whatever BODY
  serializes to the current JSON output context using one of the
  stream encoding functions.  This can be used to stream out nested
  object structures."
  `(progn
     (next-aggregate-element)
     (encode ,key (output-stream *json-output*))
     (setf (car (stack *json-output*)) #\:)
     (unwind-protect
          (progn ,@body)
       (setf (car (stack *json-output*)) #\,))))

(defgeneric encode-slots (object)
  (:method-combination progn)
  (:documentation
   "Generic function to encode objects.  Every class in a hierarchy
   implements a method for ENCODE-OBJECT that serializes its slots.
   It is a PROGN generic function so that for a given instance, all
   slots are serialized by invoking the ENCODE-OBJECT method for all
   classes that it inherits from."))

(defgeneric encode-object (object)
  (:documentation
   "Encode OBJECT, presumably a CLOS object as a JSON object, invoking
   the ENCODE-SLOTS method as appropriate.")
  (:method (object)
    (with-object ()
      (json:encode-slots object))))
