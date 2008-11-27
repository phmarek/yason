;; This file is part of yason, a Common Lisp JSON parser/encoder
;;
;; Copyright (c) 2008 Hans Hübner
;; All rights reserved.
;;
;; Please see the file LICENSE in the distribution.

(in-package :yason)

(defvar *json-output*)

(defmacro with-standard-output-to ((stream) &body body)
  `(let ((*standard-output* ,stream))
     ,@body))

(defgeneric encode (object &optional stream)

  (:documentation "Encode OBJECT to STREAM in JSON format.  May be
  specialized by applications to perform specific rendering.  STREAM
  defaults to *STANDARD-OUTPUT*."))

(defparameter *char-replacements*
  (alexandria:plist-hash-table
   '(#\\ "\\\\"
     #\" "\\\""
     #\/ "\\/"
     #\Backspace "\\b"
     #\Page "\\f"
     #\Newline "\\n"
     #\Return "\\r"
     #\Tab "\\t")))
       

(defmethod encode ((string string) &optional (stream *standard-output*))
  (with-standard-output-to (stream)
    (write-char #\")
    (dotimes (i (length string))
      (let* ((char (aref string i))
             (replacement (gethash char *char-replacements*)))
        (if replacement
            (write-string replacement)
            (write-char char))))
    (write-char #\")
    string))

(defmethod encode ((object rational) &optional (stream *standard-output*))
  (encode (float object) stream)
  object)

(defmethod encode ((object integer) &optional (stream *standard-output*))
  (princ object stream))

(defmethod encode ((object hash-table) &optional (stream *standard-output*))
  (with-standard-output-to (stream)
    (write-char #\{)
    (let (printed)
      (maphash (lambda (key value)
                 (if printed
                     (write-char #\,)
                     (setf printed t))
                 (encode key stream)
                 (write-char #\:)
                 (encode value stream))
               object))
    (write-char #\}))
  object)

(defmethod encode ((object vector) &optional (stream *standard-output*))
  (with-standard-output-to (stream)
    (write-char #\[)
    (let (printed)
      (loop
         for value across object
         do
         (when printed
           (write-char #\,))
         (setf printed t)
         (encode value stream)))
    (write-char #\]))
  object)

(defmethod encode ((object list) &optional (stream *standard-output*))
  (with-standard-output-to (stream)
    (write-char #\[)
    (let (printed)
      (dolist (value object)
        (if printed
            (write-char #\,)
            (setf printed t))
        (encode value stream)))
    (write-char #\]))
  object)

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
  (encode key (output-stream *json-output*))
  (write-char #\: (output-stream *json-output*))
  (encode value (output-stream *json-output*))
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

