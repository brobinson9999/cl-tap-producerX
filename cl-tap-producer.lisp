(require "split-sequence.lisp")

; a structured-test is a plist. The following properties are recognized:
; test-name: The name or description of the test.
; test-passed: If NIL, the test failed. Otherwise, the test passed.
; todo-desc: If non-NIL, the test is expected to fail and the value of this property is a string explaining why.
; skip-desc: If non-NIL, this test was skipped and the value of this property is a string explaining why.
; test-value: For comparison tests, the value that was found.
; expected-value: For comparison tests, the value that was expected.
; compare-sym: For comparison tests, the symbol for the comparison function used.
; compare-fun: For comparison tests, the function object that was used.
; raw-test: The form that was evaluated to produce this test.

; print-test-list: (listof structured-test) symbol -> string
; purpose: prints and returns a test report from the list of tests passed in, using
; structured-test-list->tap.
(defun print-test-list (test-result-list &optional (print-test-plan 'after))
  (let ((result-string (structured-test-list->tap test-result-list print-test-plan)))
    (format T "~a" result-string)
    result-string))

; structured-test-list->tap: (listof structured-test) symbol -> string
; purpose: produces a test report from the list of tests passed in. The report is in the TAP
; (Test Anything Protocol) format.
; print-test-plan is whether to print the test plan (1..n), and if so when to print it:
; NIL: don't print any test plan
; 'before: print the test plan before printing a list of tests
; 'after: print the test plan after printing a list of test
(defun structured-test-list->tap (test-result-list &optional (print-test-plan NIL))
  (format NIL "~a~%~{~&~a~}~%~a"
    (if (eql print-test-plan 'before) (format NIL "1..~a" (length test-result-list)) "")
    (loop for test-result in test-result-list 
	  collecting (structured-test->tap test-result))
    (if (eql print-test-plan 'after) (format NIL "1..~a" (length test-result-list)) "")))

; structured-test->tap: structured-test -> string
; prupose: produces a string containing a part of a report on the test passed in, in the
; TAP (Test Anything Protocol) format.
(defun structured-test->tap (test-result)
  (let ((diagnostic-text (get-diagnostic-text test-result)))
    (format NIL "~:[not ok~;ok~] ~a~a~a~a~a"
      (getf test-result :test-passed)
      (next-test-index)
      (if (plist-contains-p test-result :test-name)
	  (format NIL " ~a" (getf test-result :test-name)) "")
      (if (plist-contains-p test-result :skip-desc)
	  (format NIL " # skip ~a" (getf test-result :skip-desc)) "")
      (if (plist-contains-p test-result :todo-desc)
	  (format NIL " # TODO ~a" (getf test-result :todo-desc)) "")
      (if (not (equalp diagnostic-text ""))
	  (format NIL "~%~a" (start-all-lines-with-whitespace diagnostic-text :always-add-whitespace T)) ""))))

; get-diagnostic-text: structured-test -> string
; purpose: produces a string containing the diagnostic text, in the TAP format, for the test
; that is passed in.
(defun get-diagnostic-text (test-result)
  (format NIL "~{~a~^~&~}"
    (remove NIL (list
      (if (plist-contains-p test-result :test-value)
        (format NIL "found: ~S" (getf test-result :test-value)))
      (if (plist-contains-p test-result :expected-value)
        (format NIL "wanted: ~S" (getf test-result :expected-value)))
      (if (plist-contains-p test-result :raw-test)
        (format NIL "raw_test: ~S" (getf test-result :raw-test)))
      (if (plist-contains-p test-result :compare-sym)
        (format NIL "compare-sym: ~a" (getf test-result :compare-sym)))
      (if (plist-contains-p test-result :compare-fun)
        (format NIL "compare-fun: ~S" (getf test-result :compare-fun)))))))

; start-all-lines-with-whitespace: string -> string
; purpose: produces a string equal to the initial string, but with each line beginning with
; at least 2 spaces before the first non-whitespace character. Additional spaces are prefixed
; to each line as necessary to ensure that there are at least 2 spaces before the first non-
; whitespace character on each line. Lines containing no non-whitespace characters still get
; the whitespace prefix.
(defun start-all-lines-with-whitespace (input-text &key (always-add-whitespace NIL))
  (format NIL "~{~a~^~&~}"
    (loop for line in (split-sequence:split-sequence #\Newline input-text)
	  collecting (start-line-with-whitespace line 2 :always-add-whitespace always-add-whitespace))))
  
; start-line-with-whitespace: string nat -> string
; purpose: produces a string equal to the input string, with at least whitespace-quantity
; whitespace characters preceding the first non-whitespace character. Additional spaces are
; prefixed if necessary to ensure that there are at least whitespace-quantity leading space
; characters in the string. Lines containing no non-whitespace characters still get the
; whitespace prefix.
(defun start-line-with-whitespace (input-text whitespace-quantity &key (always-add-whitespace NIL))
  (cond
    ((<= whitespace-quantity 0) input-text)
    ((and
      (not always-add-whitespace) 
      (starts-with-whitespace-p input-text))
     (format NIL " ~a" (start-line-with-whitespace (subseq input-text 1)
						   (- whitespace-quantity 1)
						   :always-add-whitespace always-add-whitespace)))
    (T (format NIL " ~a" (start-line-with-whitespace input-text
						     (- whitespace-quantity 1)
						     :always-add-whitespace always-add-whitespace)))))

; starts-with-whitespace-p: string -> bool
; purpose: produces T if the input string starts with a space, otherwise produces NIL
(defun starts-with-whitespace-p (input-text)
  (cond
    ((eql (length input-text) 0) NIL)
    ((char= (elt input-text 0) #\Space) T)
    (T NIL)))

; plist-contains-p: plist any -> bool
; purpose: produces T if the input plist contains a key matching the second parameter, otherwise produces NIL
(defun plist-contains-p (input-plist desired-key)
  (loop for plist-key in input-plist by #'cddr do (when (eql plist-key desired-key) (return-from plist-contains-p T)))
  NIL)

; next-report-index: -> nat
; purpose: produces the next unusued test number and increments the global counter.
(defvar *report-test-index* 0)
(defun next-test-index ()
  (setf *report-test-index* (+ 1 *report-test-index*))
  *report-test-index*)

; print-test-plan: -> string
; purpose: prints and returns the test plan string in the TAP format.
(defun print-test-plan ()
  (let ((result (get-test-plan)))
    (format T result)
    result))

; get-test-plan: -> string
; purpose: returns the test plan string in the TAP format.
(defun get-test-plan ()
  (format NIL "1..~a" *report-test-index*))
