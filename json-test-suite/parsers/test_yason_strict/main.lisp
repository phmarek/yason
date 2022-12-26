(in-package #:de.ralph-schleicher.json-test-suite)

(defun run-test (file-name)
  "Return true if the JSON file can be read."
  (handler-case
      (progn (parse file-name) t)
    (error ())))

(defun main ()
  "Program entry point."
  (let ((program (file-namestring (uiop:argv0)))
	(arguments (uiop:command-line-arguments)))
    (unless (= (length arguments) 1)
      (uiop:die 1 "~A: wrong number of arguments~%" program))
    ;; Run the actual program.
    (uiop:shell-boolean-exit
     (run-test (parse-namestring (first arguments))))))

;;; main.lisp ends here
