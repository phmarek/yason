;; This file is part of yason, a Common Lisp JSON parser/encoder
;;
;; Copyright (c) 2008-2014 Hans Huebner and contributors
;; All rights reserved.
;;
;; Please see the file LICENSE in the distribution.

(in-package :yason)

(defvar *parse-strict* nil
  "Whether or not to parse JSON strictly according to the standard.
The relaxed parser accepts the following non-standard constructs:

   * Keys of object members may be unquoted strings,
     i.e. literal JavaScript identifier names.

   * A comma may appear after the last object member
     or after the last array element.

   * Numbers may have an explicit plus sign and digits
     before or after the decimal point may be omitted.

See also ‘*parse-duplicate-keys*’.")

(defvar *parse-nesting-depth* 1000
  "The maximum number of nested JSON structures.
Value must be a positive number and should be at least 500.
A value of ‘nil’ means to not limit the depth of nesting.")
(declaim (type (or (integer (0)) null) *parse-nesting-depth*))

(defvar *parse-duplicate-keys* nil
  "Whether or not to accept duplicate keys in JSON objects.
If enabled, the value of an existing object member is replaced by a
successive object member with the same key.  Special value ‘:ignore’
means to ignore successive duplicate object members, i.e. the value
of an existing object member will not be replaced.
If disabled, signal a ‘syntax-error’.  This is the default.

ECMA-404 says nothing about duplicate object keys.  RFC 8259 says
that object keys should be unique.  Anyway, with this user option,
you have the choice.")

(defvar *parse-object-as-alist* nil
  "Deprecated, provided for backward compatibility only.
Please use ‘*parse-object-as*’ exclusively.")

(defvar *parse-object-as* :hash-table
  "The data structure that JSON objects are parsed to.
Value is either ‘:hash-table’, ‘:alist’, or ‘:plist’.  The default
is to parse objects as hash tables.

For hash tables and alists, object keys are compared with ‘equal’,
i.e. using strings as object keys works as expected.  For plists,
object keys are compared with ‘eql’, i.e. you should also set
‘*parse-object-key-fn*’ to a function returning a symbol when
parsing objects as plists.")
(declaim (type (member :hash-table :alist :plist) *parse-object-as*))

(defvar *parse-object-key-fn* #'identity
  "The function to convert the key string of an object member.
Default is the ‘identity’ function.

The function is called with one argument, the key string.  The value
of the function is used as the key for the data structure produced.
See also ‘*parse-object-as*’.")

(defvar *parse-json-arrays-as-vectors* nil
  "True means to parse JSON arrays as vectors.
Default is to parse arrays as lists.")

(defvar *parse-json-booleans-as-symbols* nil
  "True means to parse JSON booleans as symbols.
More precisely, use the value of the symbols ‘true’ and ‘false’.
Default is to parse the literal name tokens ‘true’ and ‘false’
as ‘t’ and ‘nil’ respectively.")

(defvar *parse-json-null-as-keyword* nil
  "True means to parse JSON nulls as the keyword ‘:null’.
Default is to parse the literal name token ‘null’ as ‘nil’.")

(defvar true 'true
  "Symbol representing the JSON value true.")
(declaim (type symbol true))

(defvar false 'false
  "Symbol representing the JSON value false.")
(declaim (type symbol false))

(declaim (inline whitespace-char-p))
(defun whitespace-char-p (char)
  "Return true if CHAR is a whitespace character.
Argument CHAR has to be a character object."
  (declare (type character char))
  (or (char= char #\Space)
      (char= char #\Tab)
      (char= char #\Linefeed)
      (char= char #\Return)
      (char= char #\Newline)
      #+cl-unicode
      (unless *parse-strict*
	(cl-unicode:has-binary-property char (quote #.(cl-unicode:property-symbol "White_Space"))))))

(defvar input-stream *standard-input*
  "The current input stream.")
(declaim (type stream input-stream))

(defvar next-char nil
  "The last character read by the ‘next-char’ function.")
(declaim (type (or null character) next-char))

(declaim (inline next-char))
(defun next-char (&optional (eof-error-p t))
  "Read the next character from ‘input-stream’."
  (setf next-char (read-char input-stream eof-error-p nil)))

(declaim (inline next-char*))
(defun next-char* (&optional (eof-error-p t))
  "Like the ‘next-char’ function but skip over whitespace characters."
  (loop
    (next-char eof-error-p)
    (unless (and next-char (whitespace-char-p next-char))
      (return)))
  next-char)

(defvar nesting-depth 0
  "The current number of nested structures.")
(declaim (type integer nesting-depth))

(declaim (inline incr-nesting))
(defun incr-nesting ()
  "Increase the nesting depth."
  (incf nesting-depth)
  (when (and *parse-nesting-depth* (> nesting-depth *parse-nesting-depth*))
    (syntax-error "Too many nested structures, current limit is ~A." *parse-nesting-depth*)))

(declaim (inline decr-nesting))
(defun decr-nesting ()
  "Decrease the nesting depth."
  (decf nesting-depth))

(define-condition syntax-error (parse-error stream-error simple-condition)
  ()
  (:documentation "Base class for all syntax errors.")
  (:report (lambda (condition stream)
	     (format stream "Invalid JSON syntax")
	     (alexandria:when-let ((input (stream-error-stream condition)))
	       (format stream " in ~S" input)
	       (alexandria:when-let (position (file-position input))
		 (format stream " at ~S" position)))
	     (format stream ".")
	     (when (stringp (simple-condition-format-control condition))
	       (terpri stream)
	       (apply #'format stream
		      (simple-condition-format-control condition)
		      (simple-condition-format-arguments condition))))))

(define-condition duplicate-key (syntax-error)
  ()
  (:documentation "Deprecated, provided for backward compatibility only."))

(defun syntax-error (&optional (datum nil datum-supplied-p) &rest arguments)
  "Signal a syntax error."
  (when next-char
    (unread-char next-char input-stream))
  (cond ((stringp datum)
	 (error 'syntax-error
		:stream input-stream
		:format-control datum
		:format-arguments arguments))
	(datum-supplied-p
	 (apply #'error (or datum 'syntax-error)
		:stream input-stream
		arguments))
	(next-char
	 (error 'syntax-error
		:stream input-stream
		:format-control "Unexpected character ‘~A’."
		:format-arguments (list next-char)))
	(t
	 (error 'syntax-error
		:stream input-stream
		:format-control "Premature end of file."
		:format-arguments ()))))

(defun parse (input
	      &rest
		arguments
              &key
                (strict *parse-strict*)
                (nesting-depth *parse-nesting-depth*)
		(duplicate-keys *parse-duplicate-keys*)
                (object-as *parse-object-as* object-as-supplied-p)
                (object-key-fn *parse-object-key-fn*)
                (json-arrays-as-vectors *parse-json-arrays-as-vectors*)
                (json-booleans-as-symbols *parse-json-booleans-as-symbols*)
                (json-nulls-as-keyword *parse-json-null-as-keyword*)
		junk-allowed)
  "Parse a JSON value.

First argument INPUT is the input object.  Value is either a stream,
 pathname, or string.
If keyword argument JUNK-ALLOWED is false, signal an error of type
 ‘syntax-error’ if a non-whitespace character occurs after the JSON
 value.
The remaining keyword arguments can be used to override the parser
 settings as defined by the respective special variables.

Return value is the Lisp representation of the JSON value.

Exceptional situations:

   * Signals an ‘end-of-file’ error if the input ends in the
     middle of a JSON value.

   * Signals a ‘syntax-error’ if the input contains an invalid
     JSON structure.

   * May signal an ‘arithmetic-error’ if a JSON number can not
     be represented as a Lisp number.

   * Signals a ‘program-error’ if JSON objects are parsed as
     plists and the return value of ‘*parse-object-key-fn*’
     is not a symbol, number, or character."
  (check-type object-as (member :hash-table :alist :plist))
  (when (and (not object-as-supplied-p) (not (eq *parse-object-as* :alist)) *parse-object-as-alist*)
    (error "Incompatible combination of ‘*parse-object-as*’ and ‘*parse-object-as-alist*’, please use ‘*parse-object-as*’ exclusively."))
  (let ((*parse-strict* strict)
	(*parse-nesting-depth* nesting-depth)
	(*parse-duplicate-keys* duplicate-keys)
        (*parse-object-as* object-as)
	(*parse-object-key-fn* object-key-fn)
        (*parse-json-arrays-as-vectors* json-arrays-as-vectors)
        (*parse-json-booleans-as-symbols* json-booleans-as-symbols)
        (*parse-json-null-as-keyword* json-nulls-as-keyword))
    (apply #'parse* input arguments)))

(defun parse* (input &key junk-allowed &allow-other-keys)
  "Like the ‘parse’ function but with a lightweight interface."
  (flet ((%parse (stream)
	   (let ((input-stream stream)
		 (next-char nil)
		 (nesting-depth 0))
	     ;; Read first character.
	     (next-char*)
	     (prog1
		 (parse-value)
	       ;; Check for end of file.
	       (unless (or junk-allowed (null next-char))
		 (syntax-error))))))
    (etypecase input
      (stream
       (%parse input))
      (pathname
       (with-open-file (stream input)
	 (%parse stream)))
      (string
       (with-input-from-string (stream input)
	 (%parse stream))))))

(defun parse-value ()
  "Parse any JSON value."
  (case next-char
    (#\{
     (parse-object))
    (#\[
     (parse-array))
    (#\"
     (parse-string))
    ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\- #\+ #\.)
     (parse-number))
    (t
     (parse-literal))))

(defun parse-object ()
  "Parse a JSON object."
  (let ((object (when (eq *parse-object-as* :hash-table)
		  (make-hash-table :test #'equal)))
	(emptyp t) key-string key value dup)
    ;; Discard opening brace.
    (next-char*)
    (incr-nesting)
    ;; Parse object members.
    (loop
      (case next-char
        (#\}
	 (return))
	(#\,
	 (when emptyp
	   (syntax-error "Leading comma before object member."))
	 ;; Discard comma.
	 (next-char*)
	 ;; Check for trailing comma.
	 (when (and (not *parse-strict*) (char= next-char #\}))
	   (return)))
	(t
	 (when (not emptyp)
	   (syntax-error "Missing comma after object member."))))
      ;; Read the key.
      (setf key-string (if (char= next-char #\")
			   (parse-string)
			 (progn
			   (when *parse-strict*
			     (syntax-error "Object key must be a quoted string."))
			   (parse-literal t)))
	    key (funcall *parse-object-key-fn* key-string))
      ;; Check if the key already exists.
      (setf dup (ecase *parse-object-as*
		  (:hash-table
		   (nth-value 1 (gethash key object)))
		  (:alist
		   (assoc key object :test #'equal))
		  (:plist
		   (unless (typep key '(or symbol number character))
		     (error 'program-error))
		   (nth-value 2 (get-properties object (list key))))))
      (when (and dup (not *parse-duplicate-keys*))
        (syntax-error 'duplicate-key
		      :format-control "Duplicate object key ‘~A’."
		      :format-arguments (list key-string)))
      ;; Read the key/value separator.
      (when (null next-char)
	(error 'end-of-file :stream input-stream))
      (unless (char= next-char #\:)
        (syntax-error "Expect a colon between object key and value."))
      (next-char*)
      ;; Read the value.
      (setf value (parse-value))
      (cond ((not dup)
	     ;; First occurrence of the key.
	     (ecase *parse-object-as*
	       (:hash-table
		(setf (gethash key object) value))
	       (:alist
		(setf object (acons key value object)))
	       (:plist
		(setf object (nconc object (list key value))))))
	    ((not (eq *parse-duplicate-keys* :ignore))
	     ;; Successive occurrence of the same key.
	     ;; Replace existing value.
	     (ecase *parse-object-as*
	       (:hash-table
		(setf (gethash key object) value))
	       (:alist
		(rplacd dup value))
	       (:plist
		(setf (second dup) value)))))
      ;; Object is not empty.
      (setf emptyp nil))
    ;; Discard closing brace and skip trailing whitespace.
    (decr-nesting)
    (next-char* nil)
    ;; Return value.
    (if (eq *parse-object-as* :alist)
        (nreverse object)
      object)))

(defun parse-array ()
  "Parse a JSON array."
  (let ((array (when *parse-json-arrays-as-vectors*
		 (make-array 10 :adjustable t :fill-pointer 0)))
	(emptyp t) element)
    ;; Discard opening bracket.
    (next-char*)
    (incr-nesting)
    ;; Parse array elements.
    (loop
      (case next-char
        (#\]
	 (return))
	(#\,
	 (when emptyp
	   (syntax-error "Leading comma before array element."))
	 ;; Discard comma.
	 (next-char*)
	 ;; Check for trailing comma.
	 (when (and (not *parse-strict*) (char= next-char #\]))
	   (return)))
	(t
	 (when (not emptyp)
	   (syntax-error "Missing comma after array element."))))
      ;; Read the array element.
      (setf element (parse-value))
      (if *parse-json-arrays-as-vectors*
	  (vector-push-extend element array)
        (push element array))
      ;; Array is not empty.
      (setf emptyp nil))
    ;; Discard closing bracket and skip trailing whitespace.
    (decr-nesting)
    (next-char* nil)
    ;; Return value.
    (if (not *parse-json-arrays-as-vectors*)
	(nreverse array)
      array)))

(defun parse-string ()
  "Parse a JSON string."
  (with-output-to-string (buffer)
    (labels ((outc (char)
	       "Append a character to the output buffer."
	       (write-char char buffer)))
      ;; Parse quoted string.
      (loop
	;; Initially, this call discards the
	;; opening quote character.
	(next-char)
	(case next-char
	  (#\"
	   ;; Discard closing quote character
	   ;; and skip trailing whitespace.
	   (next-char* nil)
	   (return))
	  (#\\
	   ;; Escape sequence.
	   (next-char)
	   (case next-char
	     (#\" (outc #\"))
	     (#\\ (outc #\\))
	     (#\/ (outc #\/))
	     (#\b (outc #\Backspace))
	     (#\f (outc #\Page))
	     (#\n (outc #\Linefeed))
	     (#\r (outc #\Return))
	     (#\t (outc #\Tab))
	     (#\u
	      #-cmucl
	      (outc (parse-unicode-escape))
	      #+cmucl
	      (multiple-value-bind (high low)
		  (parse-unicode-escape)
		(outc high)
		(when low
		  (outc low))))
	     (t
	      (syntax-error "Unknown escape sequence ‘\\~A’ in string." next-char))))
	  (t
	   ;; Any other character.
	   ;;
	   ;; “All code points may be placed within the
	   ;; quotation marks except for the code points
	   ;; that must be escaped: quotation mark (U+0022),
	   ;; reverse solidus (U+005C), and the control
	   ;; characters U+0000 to U+001F.”
	   (when (and *parse-strict* (<= 0 (char-code next-char) #x1F))
	     (syntax-error "Raw control character ‘~A’ in string." next-char))
	   (outc next-char)))))))

(defun parse-unicode-escape ()
  "Helper function for ‘parse-string’."
  (flet ((parse-hex ()
	   "Read four hexadecimal digits and return the corresponding numerical value."
	   (logior (ash (or (digit-char-p (next-char) 16) (syntax-error)) 12)
		   (ash (or (digit-char-p (next-char) 16) (syntax-error))  8)
		   (ash (or (digit-char-p (next-char) 16) (syntax-error))  4)
			(or (digit-char-p (next-char) 16) (syntax-error)))))
    (let ((high (parse-hex)))
      (if (not (<= #xD800 high #xDFFF))
	  ;; A regular character.
	  (code-char high)
	;; A surrogate pair.
	(progn
	  (unless (and (char= (next-char) #\\)
		       (char= (next-char) #\u))
	    (syntax-error))
	  (let ((low (parse-hex)))
            (unless (and (<= #xD800 high #xDBFF)
			 (<= #xDC00 low #xDFFF))
	      (syntax-error "Invalid UTF-16 surrogate pair U+~4,'0X and U+~4,'0X in string." high low))
	    #-cmucl
            (code-char (+ (ash (- high #xD800) 10)
                          (- low #xDC00)
			  #x10000))
	    ;; CMUCL strings use UTF-16 encoding.  Just return the
	    ;; surrogate pair as is.
	    #+cmucl
	    (values (code-char high) (code-char low))))))))

(defun parse-number ()
  "Parse a JSON number."
  ;; The idea is to read the number into a string buffer and report
  ;; syntax errors as soon as possible.  Once the number is read, use
  ;; the Lisp reader to convert it into a Lisp object.
  (let ((number (with-output-to-string (buffer)
		  (labels ((outc (char)
			     "Append a character to the output buffer."
			     (write-char char buffer))
			   (read-digits ()
			     "Read a sequence of digits."
			     (let ((length 0))
			       (loop
				 (unless (and next-char
					      (standard-char-p next-char)
					      (digit-char-p next-char))
				   (return))
				 (incf length)
				 (outc next-char)
				 (next-char nil))
			       length)))
		    ;; See ‘read-number:read-float’.
		    (prog ((digits 0))
		       ;; Optional number sign.
		       (cond ((char= next-char #\-)
			      (outc #\-)
			      (next-char))
			     ((char= next-char #\+)
			      (when *parse-strict*
				(syntax-error "Number starts with an explicit plus sign."))
			      (next-char)))
		       ;; Integer part.
		       (cond ((char= next-char #\0)
			      (incf digits)
			      (outc #\0)
			      (next-char nil))
			     (t
			      (incf digits (read-digits))
			      (when (and *parse-strict* (zerop digits))
				(syntax-error "Integer part of a number must not be empty."))))
		       (when (null next-char)
			 (return))
		       ;; Optional fractional part.
		       (when (char= next-char #\.)
			 (outc #\.)
			 ;; Skip decimal point.  If the integer part
			 ;; is empty, the fractional part must be not
			 ;; empty.
			 (next-char (or *parse-strict* (zerop digits)))
			 (when (null next-char)
			   ;; Lisp reads ‘1.’ as an integer.
			   (outc #\0)
			   (return))
			 ;; Fractional part.
			 (cond ((and (standard-char-p next-char)
				     (digit-char-p next-char))
				(incf digits (read-digits)))
			       (t
				(when (or *parse-strict* (zerop digits))
				  (syntax-error "Fractional part of a number must not be empty."))
				(outc #\0)))
			 (when (null next-char)
			   (return)))
		       ;; Need at least one digit.
		       (when (zerop digits)
			 (syntax-error "Significant of a number must consist of at least one digit."))
		       ;; Optional exponent part.
		       (when (or (char= next-char #\E)
				 (char= next-char #\e))
			 (outc next-char)
			 ;; Skip exponent marker.
			 (next-char)
			 ;; Exponent.
			 (cond ((char= next-char #\-)
				(outc #\-)
				(next-char))
			       ((char= next-char #\+)
				(next-char)))
			 (when (zerop (read-digits))
			   (syntax-error "Exponent of a number must not be empty."))))))))
    (prog1
	(handler-case
	    (let ((*read-default-float-format* 'double-float))
	      (read-from-string number))
	  (arithmetic-error (condition)
	    ;; Re-throw the error.
	    (error condition))
	  (error ()
	    (error 'arithmetic-error
		   :operation 'read-from-string
		   :operands (list number))))
      ;; Skip trailing whitespace.
      (when (and next-char (whitespace-char-p next-char))
	(next-char* nil)))))

(defun parse-literal (&optional identifierp)
  "Parse a JSON literal name token, i.e. ‘true’, ‘false’, or ‘null’.

If optional argument IDENTIFIERP is true, accept any valid JavaScript
 identifier.

Return either the Lisp value of the literal name token or the identifier
name (a string)."
  ;; The idea is to parse a JavaScript identifier name and then check
  ;; whether or not it is a literal name token.
  (let ((name (with-output-to-string (buffer)
		(labels ((outc (char)
			   "Append a character to the output buffer."
			   (write-char char buffer)))
		  ;; Identifier names do not start with a digit.
		  (unless (and (or (alpha-char-p next-char)
				   (char= next-char #\$)
				   (char= next-char #\_)))
		    (syntax-error))
		  (loop
		    (outc next-char)
		    (next-char nil)
		    (unless (and next-char
				 (or (alpha-char-p next-char)
				     (digit-char-p next-char)
				     (char= next-char #\$)
				     (char= next-char #\_)))
		      (return)))))))
    (prog1
	(if (not identifierp)
	    ;; Expect a literal name token.
	    (cond ((string= name "true")
		   (if *parse-json-booleans-as-symbols* true t))
		  ((string= name "false")
		   (if *parse-json-booleans-as-symbols* false))
		  ((string= name "null")
		   (if *parse-json-null-as-keyword* :null))
		  (t
		   (syntax-error "Unknown literal name token ‘~A’." name)))
	  ;; Accept any identifier name.
	  (if (or (string= name "true")
		  (string= name "false")
		  (string= name "null"))
	      (syntax-error "Literal name token ‘~A’ is not a valid identifier name." name)
	    name))
      ;; Skip trailing whitespace.
      (when (and next-char (whitespace-char-p next-char))
	(next-char* nil)))))

;;; parse.lisp ends here
