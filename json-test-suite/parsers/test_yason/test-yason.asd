(in-package :asdf-user)

(defsystem "test-yason"
  :description "Test program using the YASON parser."
  :author "Ralph Schleicher <rs@ralph-schleicher.de>"
  :license "Modified BSD License"
  :version "1.0"
  :depends-on (:yason)
  :serial t
  :components ((:file "packages")
	       (:file "parse")
	       (:file "main"))
  :build-operation program-op
  :build-pathname "test-yason"
  :entry-point "de.ralph-schleicher.json-test-suite:main")

;;; test-yason.asd ends here
