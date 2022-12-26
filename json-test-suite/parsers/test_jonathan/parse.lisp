(in-package #:de.ralph-schleicher.json-test-suite)

(defun parse (file-name)
  (jonathan:parse (alexandria:read-file-into-string file-name)))

;;; parse.lisp ends here
