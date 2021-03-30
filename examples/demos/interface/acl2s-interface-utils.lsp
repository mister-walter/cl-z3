#|
  Some utility functions built on top of the ACL2s interface.
|#
(in-package "ACL2S")

;;;; Error conditions
(define-condition acl2s-interface-error (error)
  ((desc :initarg :desc :reader acl2s-interface-error/desc)
   (query :initarg :query :reader acl2s-interface-error/query)
   (err :initarg :err :reader acl2s-interface-error/err))
  (:report
   (lambda (condition stream)
     (format
      stream
      "Error occurred when running acl2s interface call:~% error ~a ~% for query: ~a ~% ~S"
      (acl2s-interface-error/err condition)
      (acl2s-interface-error/query condition)
      (acl2s-interface-error/desc condition)))))

(define-condition unexpected-response-error (error)
  ((desc :initarg :desc :reader unexpected-response-error/desc)
   (query :initarg :query :reader unexpected-response-error/query)
   (res :initarg :res :reader unexpected-response-error/res))
  (:report
   (lambda (condition stream)
     (format
      stream
      "Unexpected response from acl2s interface call:~% error ~a ~% for query: ~a ~% ~S"
      (unexpected-response-error/res condition)
      (unexpected-response-error/query condition)
      (unexpected-response-error/desc condition)))))

;;;; Utilities for dealing with propositional expressions
(defun replace-in (x to-replace replace-with)
  "Replace to-replace with replace-with inside of x, recursively if x is a cons.
Uses 'equal to compare elements with to-replace."
  (cond ((equal x to-replace) replace-with)
        ((consp x) (cons
                    (replace-in (car x) to-replace replace-with)
                    (replace-in (cdr x) to-replace replace-with)))
        (t x)))

(defun flatten-and-step (fs)
  "Given a list of statements, expand any conjunctions."
  (cond ((endp fs) nil)
        ((or (equal 'and (caar fs)) (equal '^ (caar fs))) ;; support ACL2s ^ in addition to and
         (append (cdar fs) (flatten-and-step (cdr fs))))
        (t (cons (car fs) (flatten-and-step (cdr fs))))))

;; (assert (equal (flatten-and-step '(a b (and c d) (and e (and f g)))) '(a b c d e (and f g))))
;; (assert (equal (flatten-and-step '(a (^ b (and c d)) (and (^ e f) g))) '(a b (and c d) (^ e f) g)))

(defun flatten-and (fs &optional (ls nil))
  "Given a list of terms representing the conjunction of those terms,
flatten any 'ands inside those terms and simplify."
  (if (equal fs ls)
      fs
    (flatten-and (flatten-and-step fs) fs)))

;; (assert (equal (flatten-and '(a b (and c d) (and e (and f g)))) '(a b c d e f g)))
;; (assert (equal (flatten-and '(a (^ b (and c d)) (and (^ e f) g))) '(a b c d e f g)))

(defun conjunction-terms (t1 t2)
  "Given two terms, produce the conjunction of them, simplifying if
either of the terms has a top-level and."
  (let* ((t1-terms (if (or (equal (car t1) 'and) (equal (car t1) '^)) (cdr t1) (list t1)))
         (t2-terms (if (or (equal (car t2) 'and) (equal (car t2) '^)) (cdr t2) (list t2)))
         (terms    (append t1-terms t2-terms))
         (flat-terms (remove-duplicates (flatten-and terms) :test #'equal)))
    `(and ,@flat-terms)))

;; (assert (equal (conjunction-terms 'x 'y) '(and x y)))
;; (assert (equal (conjunction-terms '(and a b) '(^ c d)) '(and a b c d)))

(defun cnf-disjunct-to-or (expr)
  "Given a CNF disjunct, convert to an equivalent ACL2s expression."
  (if (and (consp expr) (endp (cdr expr)))
      (car expr)
    (cons 'or expr)))

;; (assert (equal (cnf-disjunct-to-or '(x)) 'x))
;; (assert (equal (cnf-disjunct-to-or '(x y)) '(or x y)))

(defun conjunction-to-list (term)
  "Convert a term to a list of conjuncts.
If the term is an and, this will return a list containing the arguments to that and.
Otherwise a list containing only the given term is returned."
  (if (and (consp term) (equal 'and (car term)))
      (cdr term)
    (list term)))

(defun get-hyps (expr)
  "Get the hypotheses of an implication, returning nil if the given expression
is not an implication"
  (if (and (consp expr) (equal (car expr) 'implies))
      (conjunction-to-list (second expr))
    nil))

(assert (equal (get-hyps '(implies (and x y) z)) '(x y)))
(assert (equal (get-hyps '(and x y)) nil))
(assert (equal (get-hyps '(implies (and x y (and z w)) q)) '(x y (and z w))))

(defun get-conc (expr)
  "Get the conclusion of an implication, returning nil if the given expression
is not an implication"
  (if (and (consp expr) (equal (car expr) 'implies))
      (third expr)
    nil))

(assert (equal (get-conc '(implies (and x y) z)) 'z))
(assert (equal (get-conc '(and x y)) nil))
(assert (equal (get-conc '(implies (and x y) (implies (and z w) q))) '(implies (and z w) q)))

(defun acl2s-query-error? (res)
  (car res))

(defun thm-query-error? (res)
  (car res))

;; The response from a call to itest? inside of acl2s-query should be of the form:
;; (t nil) if an error occurred during itest? execution
;;         (i.e. trying to test something containing an undefined function)
;; or
;; (nil (cx? cxs)) otherwise
;; where cx? is a boolean that indicates whether a counterexample was found,
;; and cxs is a nonempty list of counterexamples (variable assignments)
(defun itest?-query-res-ok? (res)
  (and (consp res)
       (>= (length res) 2)
       (consp (second res))
       (>= (length (second res)) 2)
       (or (not (car (second res)))
	   (consp (second (second res))))))

;; Returns a list where:
;; the first element indicates whether any counterexamples were found
;; the second element contains the counterexamples (which are just lists of variable assignments)
;; This will error if either the internal acl2s-query returns an unexpected response, or the query itself
;; errors out.
(defun itest?-query (q &key (num-counterexamples nil) (quiet t) (prover-step-limit 3000000))
  "Attempts to find counterexamples to the given ACL2 statement.
Returns a list where:
- the first element indicates whether any counterexamples were found
- the second element contains the counterexamples (which are just lists of variable assignments)
This will error if either the internal acl2s-query returns an unexpected response, or the query itself
errors out."
  (when num-counterexamples
      (acl2s-event `(acl2s-defaults :set num-counterexamples ,num-counterexamples) :quiet t)
      (acl2s-event `(acl2s-defaults :set num-print-counterexamples ,num-counterexamples) :quiet t))
  (let* ((query `(itest? ,q))
	 (res (acl2s-query query :quiet quiet :prover-step-limit prover-step-limit)))
    (cond ((acl2s-query-error? res)
	   (error 'acl2s-interface-error :query query
		  :desc "Error occurred running itest? query"
		  :err res))
	  ((not (itest?-query-res-ok? res))
	   (error 'unexpected-response-error :query query
		  :desc "Error occurred running itest? query"
		  :res res))
	  (t (list (car (second res)) ;; whether a ctrex was found
		   (cdr (second (second res))) ;; list of ctrexes found
		   )))))

;; TODO: double-check note about 'acl2::?
(xdoc::defxdoc-raw itest?-query
  :parents (acl2s-interface)
  :short "Find counterexamples to an ACL2s statement."
  :long "
<b>General form</b>
@({
(itest?-query
  stmt                     ;; The statement to find counterexamples to.
  :num-counterexamples ... ;; Optional. If non-nil, the number of counterexamples to request from itest?.
  :quiet ...               ;; Optional. Whether or not to suppress all ACL2 printed output. Defaults to t.
  :prover-step-limit ...   ;; Optional. Sets the prover step limit. Defaults to 3000000.
)
=>
(list ctrex? ctrexes)
})
<dl>
<dt>Returns</dt>
<dd>@('ctrex?') is nil if no counterexamples were found, or non-nil otherwise.</dd>
<dd>@('ctrexes') is a list of assignments, each of which is a counterexample returned by itest?.</dd>
</dl>

<p>
The @('stmt') argument should be an ACL2 expression. Be careful about symbol packages when using @('itest?-query') when inside a different package - you may need to fully specify the name of an ACL2 function when calling it. See @(see acl2s-interface-symbol-package-tips) for more information.
</p>

<p>
When the @(':quiet') option is set to @('t'), @('itest?-query') will attempt to suppress all ACL2 printed output while itest? is running. This temporarily overrides the current @(see quiet-mode).
</p>

<p>
@('itest?-query') evaluates itest? inside of a @(see with-prover-step-limit), where the step-limit is set to the value provided to @(':prover-step-limit'), or 3000000 if that option is not provided. See @(see set-prover-step-limit) for more information about the prover step-limit. If you don't want to limit the number of prover steps permitted for itest?, set @(':prover-step-limit') to nil.
</p>

<p>
@(':num-counterexamples') allows one to request more counterexamples from @('itest?') than @('acl2s-defaults') specifies. Note that @('itest?') will not always produce the number of counterexamples requested.
</p>

<p>
In some cases, @('itest?') may produce counterexamples where some variables are assigned the value 'acl2::?. This indicates that @('itest?') did not need to bind a value to those variables for the statement to evaluate to nil, given the other variable assignments.
</p>

<p>
See @(see test?) for more information about ACL2s' counterexample generation facilities. @('itest?') is essentially a slightly modified version of @('test?') that provides more information programatically.
</p>

<h4>Examples</h4>
@({(itest?-query '(implies (integerp x) (natp x)))})
Returns @('(T (((X -875)) ((X -472)) ((X -21))))') (note that values for x may be different)

@({(itest?-query '(implies (natp x) (integerp x)))})
Returns @('(nil nil)')

@({(itest?-query '(implies (tlp x) (< (length x) 50)))})
Returns @('(nil nil)'). This is an example of a case where counterexamples exist (i.e. a list of 50 0's) but @('itest?') cannot find any.

@({(itest?-query '(implies (and (natp x) (natp y) (natp z) (> x 0) (> y 0) (< z 100)) (!= z (+ (* x x) (* y y)))))})
Returns @('(T (((Z 61) (Y 6) (X 5)) ((Z 37) (Y 1) (X 6)) ((Z 80) (Y 4) (X 8))))').
")

;; Run a thm on the given statement.
(defun acl2s-thm (q &optional goal-hints)
  (acl2s-query
   `(thm ,q :hints (("Goal" ,@goal-hints)))))

;; A call to guard-obligation inside of an acl2s-query should return something of the form:
;; ('ctx error-message) if guard-obligation had an internal error
;; (i.e. if the expression whose guard obligations are being asked for contains an undefined function)
;; or
;; (nil (term-info obligations)) otherwise
;; where term-info is of the form (:term ...)
;; and obligations is a list of obligations
(defun guard-obligations-query-res-ok? (res)
  "Determine if `res` is of a form that we expect guard-obligation to return."
  (and (consp res)
       (>= (length res) 2)
       (consp (second res))
       (>= (length (second res)) 2)))

(defun guard-obligations-query (expr &key (debug nil) (simplify t))
  "Get the guard obligations for the given expression, possibly with debug info (depending on debug argument)
This will return a CNF of the guard obligations
This will error if the internal acl2s-query returns an unexpected value, or if an error is detected"
  (let ((res (acl2s-query `(mv-let
			    (erp val)
			    (guard-obligation ',expr nil ,debug ,simplify 'ctx state)
			    (mv erp val state)))))
    (cond ((not (guard-obligations-query-res-ok? res))
           (error "acl2s-query returned an unexpected response in guard-obligations-query: ~a" res))
	  ((or (car res) (equal (car res) 'ctx))
	   (error "Error when computing guard obligations for the expression ~S:~%~S" expr res))
	  (t (second (second res))))))

;;(assert (equal (guard-obligations-query '(app x nil)) '(((true-listp x)))))

(defun rguard-obligations-debug (expr &key (simplify t))
  "Get the guard obligations of the given expression.
This returns a list containing lists where:
- the first element is the debug info (describing where the obligation came from).
- the second element is a statement describing the obligation itself.
Obligations are converted to ors here so that they can be directly run in ACL2.
ACL2-numberp is replaced by rationalp inside of obligations."
  (let ((val (guard-obligations-query expr :debug t :simplify simplify)))
    (mapcar (lambda (x)
	      (list (second (car x))
		    (replace-in (cnf-disjunct-to-or (cdr x))
				'acl2-numberp
				'rationalp)))
	    val)))

;;(assert (equal (rguard-obligations-debug '(app x nil))
;;               '(((extra-info ':top-level '(app x nil)) (true-listp x)))))

(defun rguard-obligations (expr &key (simplify t))
  "Get the guard obligations of the given expression.
This returns a list containing statements describing the obligations themselves.
Obligations are converted to ors here so that they can be directly run in ACL2.
ACL2-numberp is replaced by rationalp inside of obligations."
  (let ((val (guard-obligations-query expr :debug nil :simplify simplify)))
    (mapcar (lambda (x) (replace-in (cnf-disjunct-to-or x)
                                    'acl2-numberp
                                    'rationalp))
            val)))

;;(assert (equal (rguard-obligations '(app x nil)) '(true-listp x)))
;;(assert (equal (rguard-obligations '(app x nil) :simplify nil) '((true-listp x) (true-listp 'nil))))

(defun acl2s-untranslate (x)
  "Untranslate an expression."
  (cadr (acl2s-query
	 `(mv nil
	      (untranslate ',x nil (w state))
	      state))))

;;(assert (equal (acl2s-untranslate '(BINARY-+ '1 (BINARY-+ '2 '3))) '(+ 1 2 3)))

(defun reset-file ()
  "Reset the ACL2 state back to before the definition of START-LOAD-FILE"
  (acl2::ld '(:ubt START-LOAD-FILE)))

(defun create-reset-point ()
  "Define a function so that one can undo back to this point in the ACL2 world."
  ;; Create this function so that we can come back to this point in ACL2's history
  (acl2s-event '(defun START-LOAD-FILE () t)))

(defun acl2s-arity (f)
  "A function that determines if f is a function defined in ACL2s and if
so, produces its arity (number of arguments). If f is not a function, then 
the arity is nil. Otherwise, the arity is a natural number. Note that f
can't be a macro."
  (second (acl2s-compute `(acl2::arity ',f (w state)))))

;;(assert (equal (acl2s-arity 'append) nil)) ;; is nil since append is a macro
;;(assert (equal (acl2s-arity 'binary-append) 2))

(declaim (ftype (function (symbol) bool) is-theorem?))
(defun is-theorem? (sym)
  "Determine if the given symbol symbol names a theorem."
  (let* ((query `(acl2::theorem-namep ',sym (w state)))
         (res (acl2s-compute query)))
    (if (acl2s-query-error? res)
        (error (format nil "Internal acl2s query error for query ~a" query))
      (second res))))

(xdoc::defxdoc-raw acl2s-interface-utils
  :parents (acl2s-interface)
  :short "Some utilities built into the ACL2s interface."
  :long "
<ul>
<li>@('(flatten-and l)'): Given a list of terms representing the conjunction of those terms, recursively flatten any conjunctions inside those terms.</li>
<li>@('(conjunction-terms x y)'): Given two terms, produce the conjunction of them, simplifying if either of the terms has a top-level conjunction.</li>
<li>@('(cnf-disjunct-to-or expr)'): Given a CNF disjunct, convert to an equivalent ACL2s expression by adding 'or.</li>
<li>@('(get-hyps expr)'): Get the hypotheses of an implication, returning nil if the given expression is not an implication.</li>
<li>@('(get-conc expr)'): Get the conclusion of an implication, returning nil if the given expression is not an implication.</li>
</ul>
")
