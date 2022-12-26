(in-package #:de.ralph-schleicher.json-test-suite)

(defun parse (file-name)
  (with-open-file (stream file-name)
    (json-streams:json-parse stream)))

;;; parse.lisp ends here
