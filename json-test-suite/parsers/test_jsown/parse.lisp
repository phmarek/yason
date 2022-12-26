(in-package #:de.ralph-schleicher.json-test-suite)

(defun parse (file-name)
  (jsown:parse (alexandria:read-file-into-string file-name)))

;;; parse.lisp ends here
