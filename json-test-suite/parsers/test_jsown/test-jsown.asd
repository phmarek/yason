(in-package :asdf-user)

(defsystem "test-jsown"
  :description "Test program using the jsown parser."
  :author "Ralph Schleicher <rs@ralph-schleicher.de>"
  :license "Modified BSD License"
  :version "1.0"
  :depends-on (:alexandria :jsown)
  :serial t
  :components ((:file "packages")
	       (:file "parse")
	       (:file "main"))
  :build-operation program-op
  :build-pathname "test-jsown"
  :entry-point "de.ralph-schleicher.json-test-suite:main")

;;; test-jsown.asd ends here
