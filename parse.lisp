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

(defvar *allow-nan* t
  "Allow parsing of [+-]Infinity and [+-]Nan into 'nan, 'plus-infinity, 'minus-infinity
symbols. These are not a part of JSON, but some implementations allow it.")

(defvar *yason-float-parser* nil
  "A optional external function, taking a string as its one argument,
for parsing floats, that will be used over Lisp read.")

(defvar *yason-float-type* 'double-float
  "The output type for floats, one of 'single-float and 'double-float.
Does not apply if *YASON-FLOAT-PARSER* is set.")

(defvar *allow-loose-floats* t
  "If set to a true value, then allow numbers to have a leading +
sign, and allow the exponent in a float to be d or D, which are not
normally permitted by JSON standard.")

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




;; verify that the buffer contains a float, and is not a symbol like
;; 0e++d that might be interned by Lisp reader.
;;
;; require leading +,-, or digit; at least one digit in mantissa; no more
;; than 1 decimal point,; allow zero or one of e,E,d,D followed by +- or digit,
;; with at least one digit in exponent if e,E,d,D is present
;; verify that the buffer contains a float, and is not a symbol like
;; 0e++d that might be interned by Lisp reader.
;;
;; require leading +,-, or digit; at least one digit in mantissa; no more
;; than 1 decimal point,; allow zero or one of e,E,d,D followed by +- or digit,
;; with at least one digit in exponent if e,E,d,D is present
(defun yason-validate-float (buffer)
  (declare (type string buffer)
	   (optimize speed))
  (let ((idec  nil) ;; position of decimal point
	(iexp  nil) ;; position of exponent char
 	(ndigman  0) ;; number of of digits in mantissa
	(ndigexp 0)) ;; number of digits in exponent
    (declare (type (or null fixnum) idec iexp ndigman ndigexp))
    ;; first char must be a digit or +/-
    (when (and (plusp (length buffer))
	       (or (position (aref buffer 0) ".+-")
		   (digit-char-p (aref buffer 0))))
      (when (digit-char-p (aref buffer 0))
	(setf ndigman 1))
      (loop for i from 1 below (length buffer)
	    for c of-type character = (aref buffer i)
	    do
	       (cond ((digit-char-p c) ;; count digits in mantissa and exponent
		      (if iexp (incf ndigexp) (incf ndigman)))
		     ((position c "eEdD")
		      (when iexp  (return nil)) ;; error: 2 exponents
		      (setf iexp i))
		     ((position c  "+-")
		      (when (not (eql iexp (1- i))) ;; error: +/- not after 'E,e,D,d' 
			(return nil)))
		     ((char= c #\.)
		      (if (or idec iexp) ;; error: 2 decimal points, or . in exponent
			  (return nil)
			  (setf idec i))))
	    finally
	       (return (and
			;; must have some digits in mantissa
			(plusp ndigman)
			;; if exponent present, it must have some digits 
			(or (not iexp)
			    (plusp ndigexp))))))))
      



(defun yason-parse-float (buffer)
  (declare (type string buffer))
  (cond
    ;; use separate parser if supplied
    (*yason-float-parser*
     (funcall *yason-float-parser* buffer))
    ;; check if float is valid, then use Lisp read to parse it
    ((yason-validate-float buffer)
     (let* ((*read-default-float-format* *yason-float-type*)
	    (value (ignore-errors (read-from-string buffer))))
       (if (numberp value)
	 (coerce  value *yason-float-type*)
	 (error "Could not parse float despite being validated ~S" buffer))))
    (t
     (error "Failed to parse float string ~S" buffer))))



(defun parse-number (input)
  (let ((sign 1)
	(c (peek-char nil input nil))
	(all-digits t)) ;; all chars are digits, so it's an int
    (when (member c '(#\+ #\-))
      (read-char input)  ;; eat the sign and store it
      (when (eql c #\-) (setf sign -1)))
    (cond
      ;; is it +/- infinity (allow +/- NaN too, but ignore sign and return 'NaN)
      ((member (peek-char nil input nil) '(#\i #\I #\n #\N))
       (parse-constant input sign)) ;; parse Infinity, Inf, etc 
      (t
       (let ((buffer (make-adjustable-string)))
	 (loop
	   for c = (peek-char nil input nil)
	   for is-digit = (digit-char-p c)
	   while (or is-digit
		     (position c (if *allow-loose-floats* ".+-EeDd" ".+-Ee")))
	   do
	      (when (not is-digit)
		(setf all-digits nil))
	      (vector-push-extend (read-char input) buffer))
	 (* sign
	    (if all-digits
		(parse-integer buffer)
		(yason-parse-float buffer))))))))

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
            (code-char (+ #x010000
                          (ash (- char-code #xd800) 10)
                          (- tail-code #xdc00)))))
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
               (#\u (outc (parse-unicode-escape input)))))
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


;; new parsing function to permit nan,inf
;; infinity-sign is a leading sign for infinity, passed from
;; parse-number when it hits a +/- followed by i
(defun parse-constant (input &optional (infinity-sign 1))
  (let ((buffer (make-adjustable-string)))
    (loop while (alpha-char-p (peek-char nil input))
          do (vector-push-extend (read-char input) buffer))
    (cond ((string= buffer "true")
	   (if *parse-json-booleans-as-symbols* 'true t))
	  ((string= buffer "false")
	   (if *parse-json-booleans-as-symbols* 'false nil))
	  ((string= buffer "null")
	   (if *parse-json-booleans-as-symbols* 'null  nil))
	  ((and *allow-nan* (string-equal buffer "nan"))
	   'nan)
	  ((and *allow-nan* (or (string-equal buffer "inf")
				(string-equal buffer "infinity")))
	   (if (= infinity-sign +1)
	       'plus-infinity
	       'minus-infinity))
	  (t
	   (error "invalid constant '~A'" buffer)))))


(define-condition cannot-convert-key (error)
  ((key-string :initarg :key-string
               :reader key-string))
  (:report (lambda (c stream)
             (format stream "cannot convert key ~S used in JSON object to hash table key"
                     (key-string c)))))

(defun create-container ()
  (ecase *parse-object-as*
    ((:plist :alist)
     nil)
    (:hash-table
     (make-hash-table :test #'equal))))

(defun add-attribute (to key value)
  (ecase *parse-object-as*
    (:plist
     (append to (list key value)))
    (:alist
     (acons key value to))
    (:hash-table
     (setf (gethash key to) value)
     to)))

(define-condition expected-colon (error)
  ((key-string :initarg :key-string
               :reader key-string))
  (:report (lambda (c stream)
             (format stream "expected colon to follow key ~S used in JSON object"
                     (key-string c)))))

(defun parse-object (input)
  (let ((return-value (create-container)))
    (read-char input)
    (loop
      (when (eql (peek-char-skipping-whitespace input)
                 #\})
        (return))
      (skip-whitespace input)
      (setf return-value
            (add-attribute return-value
                           (let ((key-string (parse-string input)))
                             (prog1
                                 (or (funcall *parse-object-key-fn* key-string)
                                     (error 'cannot-convert-key :key-string key-string))
                               (skip-whitespace input)
                               (unless (eql #\: (read-char input))
                                 (error 'expected-colon :key-string key-string))
                               (skip-whitespace input)))
                           (parse input)))
      (ecase (peek-char-skipping-whitespace input)
        (#\, (read-char input))
        (#\} nil)))
    (read-char input)
    return-value))

(defconstant +initial-array-size+ 20
  "Initial size of JSON arrays read, they will grow as needed.")

(defun %parse-array (input add-element-function)
  "Parse JSON array from input, calling ADD-ELEMENT-FUNCTION for each array element parsed."
  (read-char input)
  (loop
    (when (eql (peek-char-skipping-whitespace input)
               #\])
      (return))
    (funcall add-element-function (parse input))
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

(defgeneric parse% (input)
  (:method ((input stream))
    ;; backward compatibility code
    (assert (or (not *parse-object-as-alist*)
                (eq *parse-object-as* :hash-table))
            () "unexpected combination of *parse-object-as* and *parse-object-as-alist*, please use *parse-object-as* exclusively")
    (let ((*parse-object-as* (if *parse-object-as-alist*
                                 :alist
                                 *parse-object-as*)))
      ;; end of backward compatibility code
      (check-type *parse-object-as* (member :hash-table :alist :plist))
      (let ((c (peek-char-skipping-whitespace input))) 
	(ecase c 
	  (#\"
	   (parse-string input))
	  ((#\- #\+ #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
	   (when (and (eql c #\+) (not *allow-loose-floats*))
	     (error "Encountered + sign in a number which is not compliant with standard JSON, and *ALLOW-LOOSE-FLOATS* is not true."))
	   (parse-number input))
	  (#\{
	   (parse-object input))
	  (#\[
	   (parse-array input))
	  ((#\t #\f #\n #\N 
		#\i #\I) ;; Infinity
	   (parse-constant input))
	  ))))
  (:method ((input pathname))
    (with-open-file (stream input)
      (parse stream)))
  (:method ((input string))
    (parse (make-string-input-stream input))))

(defun parse (input
              &key
                (object-key-fn *parse-object-key-fn*)
                (object-as *parse-object-as*)
                (json-arrays-as-vectors *parse-json-arrays-as-vectors*)
                (json-booleans-as-symbols *parse-json-booleans-as-symbols*)
                (json-nulls-as-keyword *parse-json-null-as-keyword*))
  "Parse INPUT, which needs to be a string or a stream, as JSON.
  Returns the lisp representation of the JSON structure parsed.  The
  keyword arguments can be used to override the parser settings as
  defined by the respective special variables."
  (let ((*parse-object-key-fn* object-key-fn)
        (*parse-object-as* object-as)
        (*parse-json-arrays-as-vectors* json-arrays-as-vectors)
        (*parse-json-booleans-as-symbols* json-booleans-as-symbols)
        (*parse-json-null-as-keyword* json-nulls-as-keyword))
    (parse% input)))
