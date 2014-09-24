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

(defun compare-tree (tree1 tree2 &optional (test #'equal))
  (if (atom tree1)
      (funcall test tree1 tree2)
      (and (compare-tree (car tree1)
                         (car tree2))
           (compare-tree (cdr tree1)
                         (cdr tree2)))))

(defun roundtrip (sexp convention)
  (case convention
    (:plist (yason:parse (with-output-to-string (stream)
                           (yason:encode-plist sexp stream))
                         :object-as :plist
                         :object-key-fn (lambda (str)
                                           (intern str :keyword))))
    (:alist (yason:parse (with-output-to-string (stream)
                           (yason:encode-alist sexp stream))
                         :object-as :alist
                         :object-key-fn (lambda (str)
                                           (intern str :keyword))))
    (otherwise (yason:parse (with-output-to-string (stream)
                              (yason:encode sexp stream))))))

(defparameter *serialisable-plists*
  '((:a "a" :one 1)
    (:a-b-c "a-b-c")
    (:a "a" :b-and-c (:b "b" :c "c"))
    (:a "a" :b "b" :c "c" :1-2-3 (1 2 3))
    (:a "a" :b-and-c-and-d ("b" (:c "c" :d "d")))))

(deftest :yason "plist-roundtrip"
  (dolist (plist *serialisable-plists*)
    (test-assert (compare-tree plist
                               (roundtrip plist :plist)))))

(defparameter *serialisable-alists*
  '(((:a . "a") (:one . 1))
    ((:a-b-c . "a-b-c"))
    ((:a . "a") (:b-and-c . ((:b . "b") (:c . "c"))))
    ((:a . "a") (:b . "b") (:c . "c") (:1-2-3 . (1 2 3)))
    ((:a . "a") (:b-and-c-and-d . ("b" ((:c . "c") (:d . "d")))))))

(deftest :yason "alist-roundtrip"
  (dolist (alist *serialisable-alists*)
    (test-assert (compare-tree alist
                               (roundtrip alist :alist)))))

(deftest :yason "no-keywords-in-alist/plist-content"
  (test-condition (yason:encode-plist '(:a :b0rk)) 'condition)
  (test-condition (yason:encode-plist '((:a . :b0rk))) 'condition))
