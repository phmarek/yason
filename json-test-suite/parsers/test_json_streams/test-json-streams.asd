(in-package :asdf-user)

(defsystem "test-json-streams"
  :description "Test program using the json-streams parser."
  :author "Ralph Schleicher <rs@ralph-schleicher.de>"
  :license "Modified BSD License"
  :version "1.0"
  :depends-on (:json-streams)
  :serial t
  :components ((:file "packages")
	       (:file "parse")
	       (:file "main"))
  :build-operation program-op
  :build-pathname "test-json-streams"
  :entry-point "de.ralph-schleicher.json-test-suite:main")

;;; test-json-streams.asd ends here
