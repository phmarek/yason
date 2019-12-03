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
