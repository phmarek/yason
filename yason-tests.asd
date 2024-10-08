(defsystem "yason-tests"
  :name "YASON"
  :author "Hans Huebner <hans@huebner.org>"
  :licence "BSD"
  :description "Tests for Yason - JSON parser/encoder"
  :depends-on (:yason :unit-test)
  :components ((:file "test"))
  :perform (test-op (o c)
                    (unless (symbol-call :unit-test '#:run-all-tests
                                         :unit :yason)
                      (error "Tests failed"))))
