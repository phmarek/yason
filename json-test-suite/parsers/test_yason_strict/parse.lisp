(in-package #:de.ralph-schleicher.json-test-suite)

(defun parse (file-name)
  (let ((yason:*parse-strict* t)
	(yason:*parse-duplicate-keys* t)
	(yason:*parse-object-as* :alist)
	(yason:*parse-object-key-fn* #'identity)
	(yason:*parse-json-arrays-as-vectors* t)
	(yason:*parse-json-booleans-as-symbols* t)
	(yason:*parse-json-null-as-keyword* t))
    (yason:parse file-name)))

;;; parse.lisp ends here
