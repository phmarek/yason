(in-package #:de.ralph-schleicher.json-test-suite)

(defun parse (file-name)
  (with-open-file (stream file-name)
    (shasht:read-json stream)))

;;; parse.lisp ends here
