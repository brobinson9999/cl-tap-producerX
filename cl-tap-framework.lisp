(require "cl-tap-producerX.package.lisp")

; we want to read unit-test-framework and cl-tap-producer into the
; cl-tap-producerX namespace, but we don't want to disturb the current
; namespace, so we set it back after loading. We have to use a macro
; because the behaviour of the in-package macro does not allow us to
; use a variable as it's parameter.
(defmacro temp-in-package (new-package &rest forms)
  (let ((old-package (package-name *package*)))
    `(progn
       (in-package ,new-package)
       ,@forms
       (in-package ,old-package))))

(temp-in-package :cl-tap-producerX
  (require "unit-test-framework.lisp")
  (require "cl-tap-producer.lisp")
  (setf cl-tap-producerX::*report-func* #'cl-tap-producerX:structured-test->tap))

; for convenience, we'll use the cl-tap-producerX package
(use-package :cl-tap-producerX)

