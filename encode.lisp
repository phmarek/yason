;; This file is part of yason, a Common Lisp JSON parser/encoder
;;
;; Copyright (c) 2008-2012 Hans Huebner and contributors
;; All rights reserved.
;;
;; Please see the file LICENSE in the distribution.

(in-package :yason)

(defvar *json-output*)

(defparameter *default-indent* nil
  "Set to T or an numeric indentation width in order to have YASON
  indent its output by default.")

(defparameter *default-indent-width* 2
  "Default indentation width for output if indentation is selected
  with no indentation width specified.")

(defgeneric encode (object &optional stream)

  (:documentation "Encode OBJECT to STREAM in JSON format.  May be
  specialized by applications to perform specific rendering.  STREAM
  defaults to *STANDARD-OUTPUT*."))

(defparameter *char-replacements*
  (alexandria:plist-hash-table
   '(#\\ "\\\\"
     #\" "\\\""
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

(defmethod encode ((object float) &optional (stream *standard-output*))
  (princ (coerce object 'single-float) stream)
  object)

(defmethod encode ((object integer) &optional (stream *standard-output*))
  (princ object stream))

(defmacro with-aggregate/object ((stream opening-char closing-char) &body body)
  "Set up serialization context for aggregate serialization with the
  object encoder."
  (alexandria:with-gensyms (printed)
    `(progn
       (write-delimiter ,opening-char ,stream)
       (change-indentation ,stream #'+)
       (prog1
           (let (,printed)
             (macrolet ((with-element-output (() &body body)
                          `(progn
                             (cond
                               (,',printed
                                (write-delimiter #\, ,',stream))
                               (t
                                (setf ,',printed t)))
                             (write-indentation ,',stream)
                             ,@body)))
               ,@body))
         (change-indentation ,stream #'-)
         (write-indentation ,stream)
         (write-delimiter ,closing-char ,stream)))))

(defun encode-key/value (key value stream)
  (encode key stream)
  (write-char #\: stream)
  (encode value stream))

(defmethod encode ((object hash-table) &optional (stream *standard-output*))
  (with-aggregate/object (stream #\{ #\})
    (maphash (lambda (key value)
               (with-element-output ()
                 (encode-key/value key value stream)))
             object)
    object))
                 
(defmethod encode ((object vector) &optional (stream *standard-output*))
  (with-aggregate/object (stream #\[ #\])
    (loop for value across object
          do (with-element-output ()
               (encode value stream)))
    object))

(defmethod encode ((object list) &optional (stream *standard-output*))
  (with-aggregate/object (stream #\[ #\])
    (dolist (value object)
      (with-element-output ()
        (encode value stream)))
    object))

(defun encode-assoc-key/value (key value stream)
  (let ((string (string key)))
    (encode-key/value string value stream)))

(defun encode-alist (object &optional (stream *standard-output*))
  (with-aggregate/object (stream #\{ #\})
    (loop for (key . value) in object
          do (with-element-output ()
               (encode-assoc-key/value key value stream)))
    object))

(defun encode-plist (object &optional (stream *standard-output*))
  (with-aggregate/object (stream #\{ #\})
    (loop for (key value) on object by #'cddr
          do (with-element-output ()
               (encode-assoc-key/value key value stream)))
    object))

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

(defclass json-output-stream (trivial-gray-streams:fundamental-character-output-stream)
  ((output-stream :reader output-stream
                  :initarg :output-stream)
   (stack :accessor stack
          :initform nil)
   (indent :initarg :indent
           :reader indent
           :accessor indent%)
   (indent-string :initform ""
                  :accessor indent-string))
  (:default-initargs :indent *default-indent*)
  (:documentation "Objects of this class capture the state of a JSON stream encoder."))

(defmethod initialize-instance :after ((stream json-output-stream) &key indent)
  (when (eq indent t)
    (setf (indent% stream) *default-indent-width*)))

(defgeneric make-json-output-stream (stream &key indent))

(defmethod make-json-output-stream (stream &key (indent t))
  "Create a JSON output stream with indentation enabled."
  (if indent
      (make-instance 'json-output-stream :output-stream stream :indent indent)
      stream))

(defmethod trivial-gray-streams:stream-write-char ((stream json-output-stream) char)
  (write-char char (output-stream stream)))

(defgeneric write-indentation (stream)
  (:method ((stream t))
    nil)
  (:method ((stream json-output-stream))
    (when (indent stream)
      (fresh-line (output-stream stream))
      (write-string (indent-string stream) (output-stream stream)))))

(defgeneric write-delimiter (char stream)
  (:method (char stream)
    (write-char char stream))
  (:method (char (stream json-output-stream))
    (write-char char (output-stream stream))))

(defgeneric change-indentation (stream operator)
  (:method ((stream t) (operator t))
    nil)
  (:method ((stream json-output-stream) operator)
    (when (indent stream)
      (setf (indent-string stream) (make-string (funcall operator (length (indent-string stream))
                                                         (indent stream))
                                                :initial-element #\Space)))))

(defun next-aggregate-element ()
  (if (car (stack *json-output*))
      (write-char (car (stack *json-output*)) (output-stream *json-output*))
      (setf (car (stack *json-output*)) #\,)))

(defmacro with-output ((stream &rest args &key indent) &body body)
  (declare (ignore indent))
  "Set up a JSON streaming encoder context on STREAM, then evaluate BODY."
  `(let ((*json-output* (make-instance 'json-output-stream :output-stream ,stream ,@args)))
     ,@body))

(defmacro with-output-to-string* ((&rest args &key indent) &body body)
  "Set up a JSON streaming encoder context, then evaluate BODY.
Return a string with the generated JSON output."
  (declare (ignore indent))
  `(with-output-to-string (s)
     (with-output (s ,@args)
       ,@body)))

(define-condition no-json-output-context (error)
  ()
  (:report "No JSON output context is active")
  (:documentation "This condition is signalled when one of the stream
  encoding function is used outside the dynamic context of a
  WITH-OUTPUT or WITH-OUTPUT-TO-STRING* body."))

(defmacro with-aggregate/stream ((begin-char end-char) &body body)
  "Set up context for aggregate serialization for the stream encoder."
  `(progn
     (unless (boundp '*json-output*)
       (error 'no-json-output-context))
     (when (stack *json-output*)
       (next-aggregate-element))
     (write-indentation *json-output*)
     (write-delimiter ,begin-char *json-output*)
     (change-indentation *json-output* #'+)
     (push nil (stack *json-output*))
     (prog1
         (progn ,@body)
       (pop (stack *json-output*))
       (change-indentation *json-output* #'-)
       (write-indentation *json-output*)
       (write-delimiter ,end-char *json-output*))))

(defmacro with-array (() &body body)
  "Open a JSON array, then run BODY.  Inside the body,
ENCODE-ARRAY-ELEMENT must be called to encode elements to the opened
array.  Must be called within an existing JSON encoder context, see
WITH-OUTPUT and WITH-OUTPUT-TO-STRING*."
  `(with-aggregate/stream (#\[ #\]) ,@body))

(defmacro with-object (() &body body)
  "Open a JSON object, then run BODY.  Inside the body,
ENCODE-OBJECT-ELEMENT or WITH-OBJECT-ELEMENT must be called to encode
elements to the object.  Must be called within an existing JSON
encoder context, see WITH-OUTPUT and WITH-OUTPUT-TO-STRING*."
  `(with-aggregate/stream (#\{ #\}) ,@body))

(defun encode-array-element (object)
  "Encode OBJECT as next array element to the last JSON array opened
with WITH-ARRAY in the dynamic context.  OBJECT is encoded using the
ENCODE generic function, so it must be of a type for which an ENCODE
method is defined."
  (next-aggregate-element)
  (write-indentation *json-output*)
  (encode object (output-stream *json-output*)))

(defun encode-array-elements (&rest objects)
  "Encode OBJECTS, a list of JSON encodable objects, as array elements."
  (dolist (object objects)
    (encode-array-element object)))

(defun encode-object-element (key value)
  "Encode KEY and VALUE as object element to the last JSON object
opened with WITH-OBJECT in the dynamic context.  KEY and VALUE are
encoded using the ENCODE generic function, so they both must be of a
type for which an ENCODE method is defined."
  (next-aggregate-element)
  (write-indentation *json-output*)
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
     (write-indentation *json-output*)
     (encode ,key (output-stream *json-output*))
     (setf (car (stack *json-output*)) #\:)
     (unwind-protect
          (progn ,@body)
       (setf (car (stack *json-output*)) #\,))))

(defgeneric encode-slots (object)
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
      (yason:encode-slots object))))
