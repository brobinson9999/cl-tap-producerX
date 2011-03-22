(load "mw-equiv-0.1.3/package.lisp")
(load "mw-equiv-0.1.3/equiv.lisp")

; *test-name* maintains a stack of deftest names to give the "path" to the currently running test when deftest is being used
(defvar *test-name* NIL)

(defun compare-plists (plist-1 plist-2)
  "compare-plists: plist plist -> boolean
purpose: returns T if the two plists are equivalent. We can't just use equalp to compare the lists because the key-value pairs could occur in a different order, or one could contain an explicit NIL while the other does not contain that key, so getf would return a NIL in either case. These should also be considered equivalent."
  (let ((combined-property-list (merge 'list (get-plist-keys plist-1) (get-plist-keys plist-2) #'eql)))
    (loop for property in combined-property-list
      when (not (equalp (getf plist-1 property) (getf plist-2 property))) do (return-from compare-plists NIL)))

  T)

(defun get-plist-keys (plist)
  "get-plist-keys: plist -> (listof symbol)
purpose: produces the list of keys that appear in the given plist."
  (loop for property in plist by #'cddr collecting property))

(defun add-to-plist-if-nonexistent (key value plist)
  "add-to-plist-if-nonexistent: symbol any plist -> plist
purpose: consumes a plist, a symbol, and a value and produces the plist with the symbol/value pair added, iff that symbol was not already a key in the plist. If the symbol was already a key in the plist, the plist is produced unaltered."
  (loop for plist-key in plist by #'cddr
	do (when (eql plist-key key) (return-from add-to-plist-if-nonexistent plist)))
  (cons key (cons value plist)))

(defun report-result (result form)
  "report-result: (union boolean structured-test) -> structured-test
purpose: consumes either T, NIL, or a structured-test and produces a structured-test. If the argument was a structured-test it is returned unaltered. If it was T or NIL, a structured-test is produced which passed or failed, respectively."
  (cond 
   ((or (eql result NIL) (eql result T))
    (list :test-name *test-name* :raw-test form :test-passed result))
   (T
    (add-to-plist-if-nonexistent :test-name *test-name*
				 (add-to-plist-if-nonexistent :raw-test form
							      result)))))

(defmacro check (&body forms)
  "check: (union boolean structured-test) -> (listof structured-test)
purpose: produces a list of structured-test results from forms. If a form produces a structured-test, that test is added to the list. If a form produces T or NIL, a structured-test is added to the list which passed or failed, respectively."
  `(list ,@(loop for f in forms collect `(report-result ,f ',f))))

(defmacro combine-results (&rest forms)
  "combine-results: (listof structured-test)* -> (listof structured-test)
purpose: appends all of the passed in lists of structured-tests and produces the combined list of structured-tests."
  `(append ,@forms))
  
(defmacro deftest (name parameters &body body)
  "deftest: any (listof parameter) any* -> any
purpose: as defun but maintains a call stack in the *test-name* dynamic variable."
  `(defun ,name ,parameters
    (let ((*test-name* (append *test-name* (list ',name))))
      ,@body)))

(defvar *report-func* NIL)
(defun report-immediately-if-at-top-level (test-result)
  "report-immediately-if-at-top-level: structured-test -> structured-test
purpose: always produces the same argument that it consumes. If the test is not being executed within a def-test, and *report-func* is a function which consumes a structured-test, then the *report-func* function will be called to report the test result immediately."
  (when (and (eql *test-name* NIL) (not (eql *report-func* NIL)))
    (format T "~a~&" (funcall *report-func* test-result)))
  test-result)

(defmacro is (valA valB &rest plist-rest)
  "is: any any (symbol any)* -> structured-test
purpose: consumes two values of any type and returns a structured-test. The test is passed if the values are equal, otherwise the test is failed. Optionally, any number of keyword/value pairs can be supplied which will be added to the resulting structured-test. identical to is-func but adds the raw-test property if it was not specified already."
  (let ((modified-plist (add-to-plist-if-nonexistent :raw-test `'(is ,valA ,valB ,@plist-rest) plist-rest)))
    `(is-func ,valA ,valB ,@modified-plist)))

(defun is-func (valA valB &rest plist-rest)
  "is-func: any any (symbol any)* -> structured-test
purpose: consumes two values of any type and returns a structured-test. The test is passed if the values are equal, otherwise the test is failed. Optionally, any number of keyword/value pairs can be supplied which will be added to the resulting structured-test."
  (report-immediately-if-at-top-level
   (append (list :test-passed (is-comparison valA valB plist-rest) :test-value valA :expected-value valB) plist-rest)))

(defun is-comparison (valA valB plist-rest)
  "is-comparison: any any (list (symbol any)*) -> boolean
purpose: consumes two values of any type as well as a plist, and returns a boolean indicating whether or not the two values are equivalent. If the plist contains the key :compare-sym, the corresponding value is the function symbol used for the comparison. otherwise, if the plist contains the key :compare-fun, the corresponding value is the function object used for the comparison. otherwise, equalp is used."
  (cond ((getf plist-rest :compare-sym) (funcall (symbol-function (getf plist-rest :compare-sym)) valA valB))
	((getf plist-rest :compare-fun) (funcall (getf plist-rest :compare-fun) valA valB))
	(T (check-equivalence valA valB))))

(defun check-equivalence (x y)
  "check-equivalence: any any -> boolean
purpose: compares two values for equivalence, using equiv:object=."
  (equiv:object= x y T))

(defmethod equiv:object-constituents ((anysymbol symbol))
  "equiv:object-constituents: symbol -> (listof (any -> any))
purpose: consumes a symbol (representing the type of the object?) and produces a list of functions whose results must be equivalent when run against two different objects, in order for the two objects to be considered equivalent. (most general case, works for any type, and only compares the type.)"
  (format T "Warning: no more specific equiv:object-constituents for type: ~S" anysymbol)
  (load-time-value (list #'type-of)))

(defmacro defequivs (&rest equiv-entries)
  "defequivs: (list (listof symbol) (listof (any -> any)))* -> defmethods
purpose: defines methods for equiv:object-constituents. Each equiv entry consists of a list of symbols for the types covered by that equiv entry, followed by a list of functions used for comparing instances of those types."
  `(progn
     ,@(reduce #'append
	       (map 'list
		    (lambda (equiv-entry)
		      (map 'list
			   (lambda (entry-symbol)
			     `(defmethod equiv:object-constituents ((type (eql ,entry-symbol)))
				(load-time-value ,(second equiv-entry))))
			   (first equiv-entry)))
		    equiv-entries))))

(defequivs
    (('symbol) (list #'identity))
    (('error) (list #'type-of))
  (('simple-error) (list #'type-of (lambda (x) (format NIL "~a" x)))))

(defmacro capture-condition (&body forms)
  "capture-condition: any* -> condition
purpose: executes the supplied forms, and if any condition is raised, that condition is returned. If no condition is raised, NIL is returned."
  `(handler-case (progn ,@forms NIL)
	       (condition (captured-condition) captured-condition)))

(defmacro is-condition (valA valB &rest plist-rest)
  "is-condition: any any (symbol any)* -> structured-test
purpose: generates a handler to capture a condition generated by valA, and compares it to valB, returning a structured-test. The test is passed if the condition captured from valA is equal to valB, otherwise the test is failed. Optionally, any number of keyword/value pairs can be supplied which will be added to the resulting structured-test."
  (let ((modified-plist
	 (add-to-plist-if-nonexistent :compare-sym ''check-equivalence
				      (add-to-plist-if-nonexistent :raw-test `'(is-condition ,valA ,valB ,@plist-rest)
								   plist-rest))))
    `(is (capture-condition ,valA) ,valB ,@modified-plist)))

(defun test-helper-proxy-arguments (in-args)
  "test-helper-proxy-arguments: (listof any) -> (listof symbol)
purpose: Consumes a list of arguments from a function declaration, and produces a list of parameters that could be used within that function to pass the same arguments to that same function, specifying all optional and keyword parameters explicitly."
  (let ((decomposed-arguments (decompose-function-arguments in-args)))
    (append (first decomposed-arguments)
	    (second decomposed-arguments)
	    (reduce #'append (map 'list (lambda (argument)
					  (list (intern (symbol-name argument) (symbol-package :key)) argument))
				  (third decomposed-arguments)))
	    (fourth decomposed-arguments))))

(defun decompose-single-argument (in-arg)
  "decompose-single-argument: (union symbol list) -> symbol
purpose: Consumes an argument from a function declaration and produces the name of that argument."
  (if (typep in-arg 'list)
      (first in-arg)
      in-arg))

(defun decompose-function-arguments (in-args)
  "decompose-function-arguments: Consumes a list of arguments from a function declaration, and produces a list of lists of each type of argument: (list regular-arguments optional-arguments keyword-arguments rest-arguments). The lists contain only argument names."
  (let ((arg-type :normal)
	(args-normal NIL)
	(args-optional NIL)
	(args-key NIL)
	(args-rest NIL))
    (loop for arg in in-args
	 do (cond ((eql arg '&optional) (setf arg-type :optional))
		  ((eql arg '&key) (setf arg-type :key))
		  ((eql arg '&rest) (setf arg-type :rest))
		  ((eql arg '&body) (setf arg-type :rest))
		  (T (cond ((eql arg-type :normal) (setf args-normal (append args-normal (list (decompose-single-argument arg)))))
			   ((eql arg-type :optional) (setf args-optional (append args-optional (list (decompose-single-argument arg)))))
			   ((eql arg-type :key) (setf args-key (append args-key (list (decompose-single-argument arg)))))
			   ((eql arg-type :rest) (setf args-rest (append args-rest (list (decompose-single-argument arg)))))))))
    (list args-normal args-optional args-key args-rest)))

(defmacro test-helper-call (&rest body)
  "test-helper-call: any* -> any
purpose: generates a list form containing the body of the macro call, with an added keyword parameter raw-test containing the body of the list form, excluding that additional parameter.
example: (test-helper-call foo bar baz) -> (foo bar baz :raw-test '(foo bar baz))"
  (if (member :raw-test body)
      body
      (append body (list :raw-test body))))

(defmacro def-test-helper (name parameter-list &rest body)
  "def-test-helper: symbol (listof argument) any* -> macro-definition
purpose: declares a function to be a test helper. Behaves similar to defun, but behind the scenes creates a macro instead which will call an inner function which has the behavior of the original function, but takes an additional raw-test keyword parameter. This probably does not maintain the 'supplied' flag on optional or keyword parameters. Probably does not work properly on cases with &rest."
  (let ((gen-name (gensym)))
    `(progn
       (defmacro ,name ,parameter-list
         (test-helper-call ,gen-name ,@(test-helper-proxy-arguments parameter-list)))

       (defun ,gen-name ,(append parameter-list
                             (if (member '&key parameter-list) NIL (list '&key))
                             (list '(raw-test NIL)))
         ,@body))))