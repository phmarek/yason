;; This file is part of yason, a Common Lisp JSON parser/encoder
;;
;; Copyright (c) 2008-2014 Hans Huebner and contributors
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

(defparameter *list-encoder* 'encode-plain-list-to-array
  "The actual function used to encode a LIST.
  Can be changed to encode ALISTs or PLISTs as dictionaries by
  setting it to ENCODE-ALIST or ENCODE-PLIST.")

(defparameter *symbol-key-encoder* 'encode-symbol-key-error
  "The actual function used to encode a SYMBOL when seen as a key.
  You might want ENCODE-SYMBOL-AS-LOWERCASE here.")


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

(defun unicode-code (char)
  (char-code char))

(defun unicode-char (code)
  (code-char code))

(defun write-surrogate-pair-escape (code stream)
  (let ((upper (+ (ldb (byte 10 10) (- code #x10000))
                  #xD800))
        (lower (+ (ldb (byte 10 0) (- code #x10000))
                  #xDC00)))
    (format stream "\\u~4,'0X\\u~4,'0X" upper lower)))

(defmethod encode ((string string) &optional (stream *standard-output*))
  (write-char #\" stream)
  (dotimes (i (length string))
    (let* ((char (aref string i))
           (replacement (gethash char *char-replacements*)))
      (cond
        (replacement (write-string replacement stream))
        ;; Control characters (U+0000 - U+001F) must be escaped.
        ((<= #x0000 (unicode-code char) #x001F)
         (format stream "\\u~4,'0X" (unicode-code char)))
        ;; Non-BMP characters must be escaped as a UTF-16 surrogate pair.
        ((<= #x010000 (unicode-code char) #x10FFFF)
         (write-surrogate-pair-escape (unicode-code char) stream))
        (t (write-char char stream)))))
  (write-char #\" stream)
  string)

(defmethod encode ((object ratio) &optional (stream *standard-output*))
  (encode (coerce object 'double-float) stream)
  object)

(defmethod encode ((object float) &optional (stream *standard-output*))
  (let ((*read-default-float-format* 'double-float))
    (format stream "~F" (coerce object 'double-float)))
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

(defun encode-plain-list-to-array (object stream)
  (with-aggregate/object (stream #\[ #\])
    (dolist (value object)
      (with-element-output ()
        (encode value stream)))
    object))

(defmethod encode ((object list) &optional (stream *standard-output*))
  (funcall *list-encoder* object stream))


(defun encode-symbol-key-error (key)
  (error "No policy for symbols as keys defined. ~
         Please check YASON:*SYMBOL-KEY-ENCODER*."))

(defun encode-symbol-as-lowercase (key)
  "Encodes a symbol KEY as a lowercase string.
  Ensure that there's no intentional lower-case character lost."
  (let ((name (symbol-name key)))
    (assert (notany #'lower-case-p name))
    (string-downcase name)))

(defun encode-assoc-key/value (key value stream)
  ;; Checking (EVERY #'UPPER-CASE-P name) breaks with non-alpha characters like #\-
  (let ((string (if (symbolp key)
                    (funcall *symbol-key-encoder* key)
                    (string key))))
    (encode-key/value string value stream)))

(defun encode-alist (object &optional (stream *standard-output*))
  ;; Failsafe in case this here is not an ALIST but a normal list
  (if (consp (first object))
      (with-aggregate/object (stream #\{ #\})
        (loop for (key . value) in object
              do (with-element-output ()
                                      (encode-assoc-key/value key value stream)))
        object)
      ;; We can't call *LIST-ENCODER* again, that would be an unlimited recursion
    (encode-plain-list-to-array object stream)))

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

(defmethod encode ((object (eql :null)) &optional (stream *standard-output*))
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

(defmacro with-output-to-string* ((&rest args &key indent stream-symbol) &body body)
  "Set up a JSON streaming encoder context, then evaluate BODY.
  Return a string with the generated JSON output."
  (declare (ignore indent))
  (let ((stream (or stream-symbol (gensym "STREAM"))))
    (remf args :stream-symbol)
    `(with-output-to-string (,stream)
       (with-output (,stream ,@args)
                    ,@body))))

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

(defun encode-object-slots (object slots)
  "For each slot in SLOTS, encode that slot on OBJECT as an object element.
Equivalent to calling ENCODE-OBJECT-ELEMENT for each slot where the
key is the slot name, and the value is the (SLOT-VALUE OBJECT slot)"
  (loop for slot in slots
     do (encode-object-element (string slot)
                               (slot-value object slot))))

(define-compiler-macro encode-object-slots (&whole form &environment env object raw-slots)
  "Compiler macro to allow open-coding with encode-object-slots when slots are literal list."
  (let ((slots (macroexpand raw-slots env)))
    (cond
      ((null slots) nil)
      ((eq (car slots) 'quote)
       (setf slots (cadr slots))        ; Get the quoted list
       `(with-slots ,slots ,object
          ,@(loop for slot in slots
               collect `(encode-object-element ,(string slot) ,slot))))
      (t form))))

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
   "Generic function to encode object slots. It should be called in an
    object encoding context. It uses PROGN combinatation with
    MOST-SPECIFIC-LAST order, so that base class slots are encoded
    before derived class slots.")
  (:method-combination progn :most-specific-last))

(defgeneric encode-object (object)
  (:documentation
   "Generic function to encode an object. The default implementation
    opens a new object encoding context and calls ENCODE-SLOTS on
    the argument.")
  (:method (object)
    (with-object ()
      (yason:encode-slots object))))
