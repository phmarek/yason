(in-package :asdf-user)

(defsystem "test-yason-strict"
  :description "Test program using the strict YASON parser."
  :author "Ralph Schleicher <rs@ralph-schleicher.de>"
  :license "Modified BSD License"
  :version "1.0"
  :depends-on (:yason-strict)
  :serial t
  :components ((:file "packages")
	       (:file "parse")
	       (:file "main"))
  :build-operation program-op
  :build-pathname "test-yason-strict"
  :entry-point "de.ralph-schleicher.json-test-suite:main")

;;; test-yason-strict.asd ends here
