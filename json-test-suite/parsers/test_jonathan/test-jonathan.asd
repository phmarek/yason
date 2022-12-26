(in-package :asdf-user)

(defsystem "test-jonathan"
  :description "Test program using the Jonathan parser."
  :author "Ralph Schleicher <rs@ralph-schleicher.de>"
  :license "Modified BSD License"
  :version "1.0"
  :depends-on (:alexandria :jonathan)
  :serial t
  :components ((:file "packages")
	       (:file "parse")
	       (:file "main"))
  :build-operation program-op
  :build-pathname "test-jonathan"
  :entry-point "de.ralph-schleicher.json-test-suite:main")

;;; test-jonathan.asd ends here
