(defpackage :yason-test
  (:use :cl :unit-test))

(in-package :yason-test)

(defparameter *basic-test-json-string* "[{\"foo\":1,\"bar\":[7,8,9]},2,3,4,[5,6,7],true,null]")
(defparameter *basic-test-json-string-indented* "
[
  {\"foo\":1,
   \"bar\":[7,8,9]
  },
  2, 3, 4, [5, 6, 7], true, null
]")
(defparameter *basic-test-json-dom* (list (alexandria:plist-hash-table
                                           '("foo" 1 "bar" (7 8 9))
                                           :test #'equal)
                                          2 3 4
                                          '(5 6 7)
                                          t nil))


(deftest :yason "parser.basic"
  (let ((result (yason:parse *basic-test-json-string*)))
    (test-equal (first *basic-test-json-dom*) (first result) :test #'equalp)
    (test-equal (rest *basic-test-json-dom*) (rest result))))

(deftest :yason "parser.basic-with-whitespace"
  (let ((result (yason:parse *basic-test-json-string-indented*)))
    (test-equal (first *basic-test-json-dom*) (first result) :test #'equalp)
    (test-equal (rest *basic-test-json-dom*) (rest result))))

(deftest :yason "dom-encoder.basic"
  (let ((result (yason:parse
                 (with-output-to-string (s)
                   (yason:encode *basic-test-json-dom* s)))))
    (test-equal (first *basic-test-json-dom*) (first result) :test #'equalp)
    (test-equal (rest *basic-test-json-dom*) (rest result))))

(deftest :yason "dom-encoder.w-o-t-s*"
      (let* ((stg (yason:with-output-to-string* (:indent 2)
                    (yason:encode *basic-test-json-dom*)))
             (result (yason:parse stg)))
    (test-equal (subseq stg 2 5) "  {")
    (test-equal (first *basic-test-json-dom*) (first result) :test #'equalp)
    (test-equal (rest *basic-test-json-dom*) (rest result))))

(deftest :yason "dom-encoder.w-o"
  (let* ((stg (with-output-to-string (s)
                    (yason:with-output (s :indent 3)
                      (yason:encode *basic-test-json-dom*))))
         (result (yason:parse stg)))
    (test-equal (subseq stg 2 6) "   {")
    (test-equal (first *basic-test-json-dom*) (first result) :test #'equalp)
    (test-equal (rest *basic-test-json-dom*) (rest result))))

(defun whitespace-char-p (char)
  (member char '(#\space #\tab #\return #\newline #\linefeed)))

(deftest :yason "dom-encoder.indentation"
  (test-equal "[
          1,
          2,
          3
]"
              (with-output-to-string (s)
                (yason:encode '(1 2 3) (yason:make-json-output-stream s :indent 10))))
  (dolist (indentation-arg '(nil t 2 20))
    (test-equal "[1,2,3]" (remove-if #'whitespace-char-p
                                     (with-output-to-string (s)
                                       (yason:encode '(1 2 3)
                                                    (yason:make-json-output-stream s :indent indentation-arg)))))))

(deftest :yason "dom-encoder.object-indentation"
  (test-equal "{
  \"foo\": [
    1
  ],
  \"bar\": [
    2,
    3
  ]
}"
              (with-output-to-string (s)
                (yason:encode-alist
                 '(("foo" 1) ("bar" 2 3))
                 (yason:make-json-output-stream s :indent 2)))))

(deftest :yason "dom-encoder.empty-array-and-object-indentation"
  (test-equal "[
  [],
  {}
]"
              (with-output-to-string (s)
                (yason:encode
                 (list (vector) (make-hash-table))
                 (yason:make-json-output-stream s :indent 2)))))

(deftest :yason "stream-encoder.basic-array"
  (test-equal "[0,1,2]"
              (with-output-to-string (s)
                (yason:with-output (s)
                  (yason:with-array ()
                    (dotimes (i 3)
                      (yason:encode-array-element i)))))))

(deftest :yason "stream-encoder.basic-object"
  (test-equal "{\"hello\":\"hu hu\",\"harr\":[0,1,2]}"
              (with-output-to-string (s)
                (yason:with-output (s)
                  (yason:with-object ()
                    (yason:encode-object-element "hello" "hu hu")
                    (yason:with-object-element ("harr")
                      (yason:with-array ()
                        (dotimes (i 3)
                          (yason:encode-array-element i)))))))))

(deftest :yason "stream-encode.unicode-string"
  (test-equal "\"ab\\u0002 cde \\uD834\\uDD1E\""
              (with-output-to-string (s)
                (yason:encode (format nil "ab~C cde ~C" (code-char #x02) (code-char #x1d11e)) s))))

(defstruct user name age password)

(defmethod yason:encode ((user user) &optional (stream *standard-output*))
           (yason:with-output (stream)
             (yason:with-object ()
               (yason:encode-object-element "name" (user-name user))
               (yason:encode-object-element "age" (user-age user)))))

(deftest :yason "stream-encoder.application-struct"
  (test-equal "[{\"name\":\"horst\",\"age\":27},{\"name\":\"uschi\",\"age\":28}]"
              (with-output-to-string (s)
                (yason:encode (list (make-user :name "horst" :age 27 :password "puppy")
                                   (make-user :name "uschi" :age 28 :password "kitten"))
                             s))))

(deftest :yason "recursive-alist-encode"
  (test-equal "{\"a\":3,\"b\":[1,2,{\"c\":4,\"d\":[6]}]}"
              (yason:with-output-to-string* (:stream-symbol s)
                (let ((yason:*list-encoder* #'yason:encode-alist))
                  (yason:encode
                    `(("a" . 3) ("b" . #(1 2 (("c" . 4) ("d" . #(6))))))
                    s)))))

(deftest :yason "symbols-as-keys"
  (test-condition
    (yason:with-output-to-string* (:stream-symbol s)
      (let ((yason:*symbol-key-encoder* #'yason:encode-symbol-as-lowercase))
        (yason:encode-alist
          `((:|abC| . 3))
          s)))
    'error)
  (test-equal "{\"a\":3}"
              (yason:with-output-to-string* (:stream-symbol s)
                (let ((yason:*symbol-key-encoder* #'yason:encode-symbol-as-lowercase))
                  (yason:encode-alist
                    `((:a . 3))
                    s)))))

(deftest :yason "symbols-as-ht-keys"
  (test-equal "{\"bar\":2}"
              (let* ((yason:*symbol-key-encoder* #'yason:encode-symbol-as-lowercase))
                (with-output-to-string (*standard-output*)
                  (yason:with-output (*standard-output*) 
                    (yason:encode (alexandria:plist-hash-table `(:bar 2))))))))

(deftest :yason "ENCODE-as-obj-value"
  (test-equal "{\"foo\":1}"
              (with-output-to-string (*standard-output*)
                (yason:with-output (*standard-output*) 
                  (yason:with-object ()
                    (yason:with-object-element ("foo")
                      (yason:encode 1)))))))

(deftest :yason "ENCODE-as-obj-value-2"
  (test-equal "{\"baz\":12,\"foo\":{\"bar\":1}}"
              (with-output-to-string (*standard-output*)
                (yason:with-output (*standard-output*) 
                  (yason:with-object ()
                    (yason:with-object-element ("baz")
                      (yason:encode 12))
                    (yason:with-object-element ("foo")
                      (yason:with-object ()
                        (yason:with-object-element ("bar")
                          (yason:encode 1)))))))))

(deftest :yason "object-in-array"
  (test-equal "[5,[6,{\"far\":8,\"near\":9,\"there\":1},7],{\"foo\":\"bar\",\"bar\":\"baz\"},7]"
              (with-output-to-string (*standard-output*)
                (yason:with-output (*standard-output* :indent nil)
                  (yason:with-array ()
                    (yason:encode-array-element 5)
                    (yason:with-array ()
                      (yason:encode-array-element 6)
                      (yason:with-object ()
                        (yason:with-object-element ("far")
                          (yason:encode 8))
                        (yason:encode-object-elements 
                          "near" 9
                          "there" 1))
                      (yason:encode-array-element 7))
                    (yason:with-object ()
                      (yason:encode-object-element "foo" "bar")
                      (yason:encode-object-element "bar" "baz"))
                    (yason:encode-array-element 7))))))

(deftest :yason "output-to-string-with-indentation"
  (test-equal "[
  1,
  2,
  3
]"
              (yason:with-output-to-string* (:indent 2 :stream-symbol s)
                (yason:encode #(1 2 3)))))

(deftest :yason "parse-double-float"
  (test-equal 1579806040.0d0
              (yason:parse "1579806040.0")))

(deftest :yason "parse-ordering-hash"
  (let ((parsed-hash (yason:parse "{\"foo\":0,\"bar\":1,\"foO\":2}")))
    (test-equal 0
                (gethash "foo" parsed-hash))
    (test-equal 2
                (gethash "foO" parsed-hash))
    (test-equal 1
                (gethash "bar" parsed-hash))))

(deftest :yason "parse-ordering-alist"
  (let ((yason:*parse-object-as* :alist))
    (test-equal '(("foo" . 0) ("bar" . 1) ("foO" . 2))
                (yason:parse "{\"foo\":0,\"bar\":1,\"foO\":2}"))))

(deftest :yason "parse-ordering-plist"
  (let ((yason:*parse-object-as* :plist))
    (test-equal '("foo" 0 "bar" 1 "foo" 2)
                (yason:parse "{\"foo\":0,\"bar\":1,\"foo\":2}"))))


(deftest :yason "duplicate-key"
  (test-condition (yason:parse "{\"a\":1,\"a\":2}")
                  'yason::duplicate-key)
  (test-condition (yason:parse "{\"a\":1,\"a\\ud800\":2}")
                  'error))
