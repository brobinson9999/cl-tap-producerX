; Uses the package :cl-tap-producerX since :cl-tap-producer may infringe
; the TAP Namespace Non-proliferation Treaty located at:
; http://testanything.org/wiki/index.php/TAP_Namespace_Nonproliferation_Treaty
(defpackage :cl-tap-producerX
  (:use :common-lisp)
  (:export ; tap exporter
           :print-test-list
	   :structured-test-list->tap
	   :structured-test->tap
	   :print-test-plan
	   :format-test-value
	   ; test framework
	   :check
	   :combine-results
	   :deftest
	   :is
	   :is-condition
	   :capture-condition
; now in plist-utils.lisp
;	   :compare-plists
))
