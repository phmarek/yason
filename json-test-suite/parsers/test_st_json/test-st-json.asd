(in-package :asdf-user)

(defsystem "test-st-json"
  :description "Test program using the ST-JSON parser."
  :author "Ralph Schleicher <rs@ralph-schleicher.de>"
  :license "Modified BSD License"
  :version "1.0"
  :depends-on (:st-json)
  :serial t
  :components ((:file "packages")
	       (:file "parse")
	       (:file "main"))
  :build-operation program-op
  :build-pathname "test-st-json"
  :entry-point "de.ralph-schleicher.json-test-suite:main")

;;; test-st-json.asd ends here
