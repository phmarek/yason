(in-package :asdf-user)

(defsystem "test-shasht"
  :description "Test program using the shasht parser."
  :author "Ralph Schleicher <rs@ralph-schleicher.de>"
  :license "Modified BSD License"
  :version "1.0"
  :depends-on (:shasht)
  :serial t
  :components ((:file "packages")
	       (:file "parse")
	       (:file "main"))
  :build-operation program-op
  :build-pathname "test-shasht"
  :entry-point "de.ralph-schleicher.json-test-suite:main")

;;; test-shasht.asd ends here
