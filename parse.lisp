;; This file is part of yason, a Common Lisp JSON parser/encoder
;;
;; Copyright (c) 2008-2014 Hans Huebner and contributors
;; All rights reserved.
;;
;; Please see the file LICENSE in the distribution.

(in-package :yason)

(defconstant +default-string-length+ 20
  "Default length of strings that are created while reading json input.")

(defvar *parse-object-key-fn* #'identity
  "Function to call to convert a key string in a JSON array to a key
  in the CL hash produced.")

(defvar *parse-json-arrays-as-vectors* nil
  "If set to a true value, JSON arrays will be parsed as vectors, not
  as lists.")

(defvar *parse-json-booleans-as-symbols* nil
  "If set to a true value, JSON booleans will be read as the symbols
  TRUE and FALSE, not as T and NIL, respectively.")

(defvar *parse-json-null-as-keyword* nil
  "If set to a true value, JSON nulls will be read as the keyword :NULL, not as NIL.")

(defvar *parse-object-as* :hash-table
  "Set to either :hash-table, :plist or :alist to determine the data
  structure that objects are parsed to.")

(defvar *parse-object-as-alist* nil
  "DEPRECATED, provided for backward compatibility")

(defun make-adjustable-string ()
  "Return an adjustable empty string, usable as a buffer for parsing strings and numbers."
  (make-array +default-string-length+
              :adjustable t :fill-pointer 0 :element-type 'character))

(defun parse-number (input)
  "Parse a JSON number.

May signal an ‘arithmetic-error’ like, e.g. ‘floating-point-overflow’
or ‘floating-point-underflow’."
  (let ((buffer (make-adjustable-string))
	;; Sequence of valid digit characters.
	(digits "0123456789")
	;; Invalid input character.
	(invalid #\Space))
    (flet ((read-digits (optional)
	     (when (not optional)
	       (unless (position (peek-char nil input nil invalid) digits :test #'char=)
		 (error 'parse-error)))
	     (loop :while (position (peek-char nil input nil invalid) digits :test #'char=)
		   :do (vector-push-extend (read-char input) buffer))))
      (declare (inline read-digits))
      ;; Optional minus sign.
      (when (char= (peek-char nil input nil invalid) #\-)
	(vector-push-extend (read-char input) buffer))
      ;; Integer part.
      (case (peek-char nil input nil invalid)
	(#\0
	 (vector-push-extend (read-char input) buffer))
	((#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
	 (vector-push-extend (read-char input) buffer)
	 (read-digits t))
	(t
	 (error 'parse-error)))
      ;; Optional fractional part.
      (when (char= (peek-char nil input nil invalid) #\.)
	(vector-push-extend (read-char input) buffer)
	(read-digits nil))
      ;; Optional exponent part.
      (when (char-equal (peek-char nil input nil invalid) #\E)
	(vector-push-extend (read-char input) buffer)
	(case (peek-char nil input nil invalid)
	  ((#\+ #\-)
	   (vector-push-extend (read-char input) buffer)))
	(read-digits nil))
      ;; Read the number.
      (let ((*read-default-float-format* 'double-float))
	(values (read-from-string buffer))))))

(defun parse-unicode-escape (input)
  (let ((char-code (let ((buffer (make-string 4)))
                     (read-sequence buffer input)
                     (parse-integer buffer :radix 16))))
    (if (and (>= char-code #xd800)
             (<= char-code #xdbff))
        (let ((buffer (make-string 6)))
          (read-sequence buffer input)
          (when (not (string= buffer "\\u" :end1 2))
            (error "Lead Surrogate without Tail Surrogate"))
          (let ((tail-code (parse-integer buffer :radix 16 :start 2)))
            (when (not (and (>= tail-code #xdc00)
                            (<= tail-code #xdfff)))
              (error "Lead Surrogate without Tail Surrogate"))
	    #-cmucl
            (code-char (+ #x010000
                          (ash (- char-code #xd800) 10)
                          (- tail-code #xdc00)))
	    ;; Cmucl strings use utf-16 encoding.  Just return the two
	    ;; surrogate chars as is.
	    #+cmucl
	    (values (code-char char-code) (code-char tail-code))))
        (code-char char-code))))

(defun parse-string (input)
  (let ((output (make-adjustable-string)))
    (labels ((outc (c)
               (vector-push-extend c output))
             (next ()
               (read-char input))
             (peek ()
               (peek-char nil input)))
      (let* ((starting-symbol (next))
             (string-quoted (equal starting-symbol #\")))
        (unless string-quoted
          (outc starting-symbol))
        (loop
          (cond
            ((eql (peek) #\")
             (next)
             (return-from parse-string output))
            ((eql (peek) #\\)
             (next)
             (ecase (next)
               (#\" (outc #\"))
               (#\\ (outc #\\))
               (#\/ (outc #\/))
               (#\b (outc #\Backspace))
               (#\f (outc #\Page))
               (#\n (outc #\Newline))
               (#\r (outc #\Return))
               (#\t (outc #\Tab))
               (#\u
		#-cmucl
		(outc (parse-unicode-escape input))
		#+cmucl
		(multiple-value-bind (char tail)
		    (parse-unicode-escape input)
		  (outc char)
		  ;; Output the surrogate as is for cmucl.
		  (when tail
		    (outc tail))))))
            ((and (or (whitespace-p (peek))
                      (eql (peek) #\:))
                  (not string-quoted))
             (return-from parse-string output))
            (t
             (outc (next)))))))))

(defun whitespace-p (char)
  (member char '(#\Space #\Newline #\Tab #\Linefeed #\Return)))

(defun skip-whitespace (input)
  (loop for c = (peek-char nil input nil nil)
     while (and c (whitespace-p c))
     do (read-char input)))

(defun peek-char-skipping-whitespace (input &optional (eof-error-p t))
  (skip-whitespace input)
  (peek-char nil input eof-error-p))

(defun parse-constant (input)
  (destructuring-bind (expected-string return-value)
      (find (peek-char nil input nil)
            `(("true" ,(if *parse-json-booleans-as-symbols* 'true t))
              ("false" ,(if *parse-json-booleans-as-symbols* 'false nil))
              ("null"  ,(if *parse-json-null-as-keyword* :null nil)))
            :key (lambda (entry) (aref (car entry) 0))
            :test #'eql)
    (loop for char across expected-string
          unless (eql (read-char input nil) char)
            do (error "invalid constant"))
    return-value))

(define-condition duplicate-key (parse-error)
  ((key-string :initarg :key-string
               :reader key-string))
  (:report (lambda (c stream)
             (format stream "Duplicate JSON object key ~S."
                     (key-string c)))))

(defun parse-object (input)
  "Parse a JSON object."
  (let ((object (when (eq *parse-object-as* :hash-table)
		  (make-hash-table :test #'equal)))
	(unknown (when (eq *parse-object-as* :plist)
		   (gensym)))
	(emptyp t))
    ;; Discard opening brace.
    (read-char input)
    ;; Parse members.
    (loop
      (case (peek-char-skipping-whitespace input)
        (#\}
	 (return))
	(#\,
	 (when emptyp
	   (error 'parse-error))
	 (read-char input)
         (skip-whitespace input))
	(t
	 (when (not emptyp)
	   ;; Require a comma.
	   (error 'parse-error))))
      (let* ((key-string (parse-string input))
             (key (funcall *parse-object-key-fn* key-string)))
	(when (ecase *parse-object-as*
		(:hash-table
		 (nth-value 1 (gethash key object)))
		(:alist
		 (assoc key object :test #'equal))
		(:plist
		 #-(and)
		 (unless (symbolp key)
		   (error "Object keys must be symbols."))
		 (not (eq (getf object key unknown) unknown))))
          (error 'duplicate-key :key-string key-string))
        (skip-whitespace input)
        (unless (eql #\: (read-char input))
          (error 'parse-error))
        (skip-whitespace input)
        (let ((value (parse* input)))
	  (ecase *parse-object-as*
	    (:hash-table
	     (setf (gethash key object) value))
	    (:alist
	     (setf object (acons key value object)))
	    (:plist
	     (setf object (nconc object (list key value)))))
	  (setf emptyp nil))))
    ;; Discard closing brace.
    (read-char input)
    ;; Return value.
    (if (eq *parse-object-as* :alist)
        (nreverse object)
      object)))

(defconstant +initial-array-size+ 20
  "Initial size of JSON arrays read, they will grow as needed.")

(defun %parse-array (input add-element-function)
  "Parse JSON array from input, calling ADD-ELEMENT-FUNCTION for each array element parsed."
  (read-char input)
  (loop
    (when (eql (peek-char-skipping-whitespace input)
               #\])
      (return))
    (funcall add-element-function (parse* input))
    (ecase (peek-char-skipping-whitespace input)
      (#\, (read-char input))
      (#\] nil)))
  (read-char input))

(defun parse-array (input)
  (if *parse-json-arrays-as-vectors*
      (let ((return-value (make-array +initial-array-size+ :adjustable t :fill-pointer 0)))
        (%parse-array input
                      (lambda (element)
                        (vector-push-extend element return-value)))
        return-value)
      (let (return-value)
        (%parse-array input
                      (lambda (element)
                        (push element return-value)))
        (nreverse return-value))))

(defun parse* (input)
  "Parse any JSON value."
  (ecase (peek-char-skipping-whitespace input)
    (#\"
     (parse-string input))
    ((#\- #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
     (parse-number input))
    (#\{
     (parse-object input))
    (#\[
     (parse-array input))
    ((#\t #\f #\n)
     (parse-constant input))))

(defun parse (input
              &key
                (object-key-fn *parse-object-key-fn*)
                (object-as *parse-object-as* object-as-supplied-p)
                (json-arrays-as-vectors *parse-json-arrays-as-vectors*)
                (json-booleans-as-symbols *parse-json-booleans-as-symbols*)
                (json-nulls-as-keyword *parse-json-null-as-keyword*)
		junk-allowed)
  "Parse INPUT, which needs to be a stream, pathname, or string, as JSON.

If keyword argument JUNK-ALLOWED is false, signal an error of type
 ‘parse-error’ if a non-whitespace character occurs after the JSON
 structure.
The remaining keyword arguments can be used to override the parser
 settings as defined by the respective special variables.

Returns the Lisp representation of the JSON structure parsed."
  (check-type object-as (member :hash-table :alist :plist))
  (when (and (not object-as-supplied-p) (not (eq *parse-object-as* :alist)) *parse-object-as-alist*)
    (error "Incompatible combination of *PARSE-OBJECT-AS* and *PARSE-OBJECT-AS-ALIST*, please use *PARSE-OBJECT-AS* exclusively."))
  (let ((*parse-object-key-fn* object-key-fn)
        (*parse-object-as* object-as)
        (*parse-json-arrays-as-vectors* json-arrays-as-vectors)
        (*parse-json-booleans-as-symbols* json-booleans-as-symbols)
        (*parse-json-null-as-keyword* json-nulls-as-keyword))
    (flet ((%parse (stream)
	     (prog1
		 (parse* stream)
	       ;; Check for end of file.
	       (skip-whitespace stream)
	       (unless (or junk-allowed (eq (peek-char nil stream nil stream) stream))
		 (error 'parse-error)))))
      (etypecase input
	(stream
	 (%parse input))
	(pathname
	 (with-open-file (stream input)
	   (%parse stream)))
	(string
	 (with-input-from-string (stream input)
	   (%parse stream)))))))
