(in-package #:de.ralph-schleicher.json-test-suite)

(defun parse (file-name)
  (with-open-file (stream file-name)
    (json:decode-json stream)))

;;; parse.lisp ends here
