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
	   ; test framework
	   :check
	   :combine-results
	   :deftest
	   :is
	   :is-condition
	   :compare-plists))
