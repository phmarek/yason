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
  (let ((result (json:parse *basic-test-json-string*)))
    (test-equal (first *basic-test-json-dom*) (first result) :test #'equalp)
    (test-equal (rest *basic-test-json-dom*) (rest result))))

(deftest :yason "parser.basic-with-whitespace"
  (let ((result (json:parse *basic-test-json-string-indented*)))
    (test-equal (first *basic-test-json-dom*) (first result) :test #'equalp)
    (test-equal (rest *basic-test-json-dom*) (rest result))))

(deftest :yason "dom-encoder.basic"
  (let ((result (json:parse
                 (with-output-to-string (s)
                   (json:encode *basic-test-json-dom* s)))))
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
                (json:encode '(1 2 3) (yason:make-json-output-stream s :indent 10))))
  (dolist (indentation-arg '(nil t 2 20))
    (test-equal "[1,2,3]" (remove-if #'whitespace-char-p
                                     (with-output-to-string (s)
                                       (json:encode '(1 2 3)
                                                    (yason:make-json-output-stream s :indent indentation-arg)))))))
    
(deftest :yason "stream-encoder.basic-array"
  (test-equal "[0,1,2]"
              (with-output-to-string (s)
                (json:with-output (s)
                  (json:with-array ()
                    (dotimes (i 3)
                      (json:encode-array-element i)))))))

(deftest :yason "stream-encoder.basic-object"
  (test-equal "{\"hello\":\"hu hu\",\"harr\":[0,1,2]}"
              (with-output-to-string (s)
                (json:with-output (s)
                  (json:with-object ()
                    (json:encode-object-element "hello" "hu hu")
                    (json:with-object-element ("harr")
                      (json:with-array ()
                        (dotimes (i 3)
                          (json:encode-array-element i)))))))))

(defstruct user name age password)

(defmethod json:encode ((user user) &optional (stream *standard-output*))
           (json:with-output (stream)
             (json:with-object ()
               (json:encode-object-element "name" (user-name user))
               (json:encode-object-element "age" (user-age user)))))

(deftest :yason "stream-encoder.application-struct"
  (test-equal "[{\"name\":\"horst\",\"age\":27},{\"name\":\"uschi\",\"age\":28}]"
              (with-output-to-string (s)
                (json:encode (list (make-user :name "horst" :age 27 :password "puppy")
                                   (make-user :name "uschi" :age 28 :password "kitten"))
                             s))))

