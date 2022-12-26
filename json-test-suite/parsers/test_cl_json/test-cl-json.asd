(in-package :asdf-user)

(defsystem "test-cl-json"
  :description "Test program using the CL-JSON parser."
  :author "Ralph Schleicher <rs@ralph-schleicher.de>"
  :license "Modified BSD License"
  :version "1.0"
  :depends-on (:cl-json)
  :serial t
  :components ((:file "packages")
	       (:file "parse")
	       (:file "main"))
  :build-operation program-op
  :build-pathname "test-cl-json"
  :entry-point "de.ralph-schleicher.json-test-suite:main")

;;; test-cl-json.asd ends here
