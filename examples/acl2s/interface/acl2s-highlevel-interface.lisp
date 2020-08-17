;;(load "acl2s-interface.lisp")
(defpackage :acl2s-interface
  (:use :cl))
(in-package :acl2s-interface)
(ql:quickload :trivia)

(import 'acl2s::(acl2s-query acl2s-event))
(import 'trivia::match)

;; Error conditions

(define-condition acl2s-query-error (error)
  ((desc :initarg :desc :reader acl2s-query-error/desc)
   (query :initarg :query :reader acl2s-query-error/query)
   (err :initarg :err :reader acl2s-query-error/err))
  (:report
   (lambda (condition stream)
     (format
      stream
      "Error occurred when running acl2s query:~% error ~a ~% for query: ~a ~% ~S"
      (acl2s-query-error/err condition)
      (acl2s-query-error/query condition)
      (acl2s-query-error/desc condition)))))

(define-condition unexpected-response-error (error)
  ((desc :initarg :desc :reader unexpected-response-error/desc)
   (query :initarg :query :reader unexpected-response-error/query)
   (res :initarg :res :reader unexpected-response-error/res))
  (:report
   (lambda (condition stream)
     (format
      stream
      "Unexpected response from acl2s query:~% error ~a ~% for query: ~a ~% ~S"
      (unexpected-response-error/res condition)
      (unexpected-response-error/query condition)
      (unexpected-response-error/desc condition)))))

(defun cnf-disjunct-to-or (expr)
  (if (and (consp expr) (endp (cdr expr)))
      (car expr)
    (cons 'acl2::or expr)))

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
(defun itest?-query (q)
  (let* ((query `(acl2s::itest? ,q))
	 (res (acl2s-query query)))
    (cond ((acl2s-query-error? res)
	   (error 'acl2s-query-error :query query
		  :desc "Error occurred running itest? query"
		  :err res))
	  ((not (itest?-query-res-ok? res))
	   (error 'unexpected-response-error :query query
		  :desc "Error occurred running itest? query"
		  :res res))
	  (t (list (car (second res)) ;; whether a ctrex was found
		   (cdr (second (second res))) ;; list of ctrexes found
		   )))))

;; Run a thm on the given statement in the current theory
(defun thm-full-theory (q &optional goal-hints)
  (acl2s-query
   `(acl2s::thm ,q
		:hints (("Goal" ,@goal-hints)))))

(defun thm-min-theory-with-rules (q rules &optional goal-hints)
  (acl2s-query
   `(acl2s::thm ,q
		:hints (("Goal"
			 :in-theory '(acl2s::min-theory ,@rules)
			 ,@goal-hints)))))

(defun thm-min-theory-with-rules-and-contracts (q rules &optional goal-hints)
  (acl2s-query
   `(acl2s::thm ,q
		:hints (("Goal"
			 :in-theory '(acl2s::contract-theory ,@rules)
			 ,@goal-hints)))))


(defun flatten-and-step (fs)
  (cond ((endp fs) nil)
        ((equal 'and (caar fs)) (append (cdar fs) (flatten-and-step (cdr fs))))
        (t (cons (car fs) (flatten-and-step (cdr fs))))))

(defun flatten-and-fix (fs ls)
  (if (equal fs ls)
      fs
    (flatten-and-fix (flatten-and-step fs) fs)))

(defun flatten-and (fs)
  (flatten-and-fix fs nil))
  
;; Given two terms, produce the conjunction of them, simplifying if
;; either of the terms has a top-level and.
(defun conjunction-terms (t1 t2)
  (let* ((t1-terms (if (equal (car t1) 'acl2::and) (cdr t1) (list t1)))
         (t2-terms (if (equal (car t2) 'acl2::and) (cdr t2) (list t2)))
         (terms    (append t1-terms t2-terms))
         (flat-terms (remove-duplicates (flatten-and terms) :test #'equal)))
    `(acl2::and ,@flat-terms)))

;; Get the hypotheses of an implication, returning nil if the given statement
;; is not an implication
(defun get-hyps (stmt)
  (match stmt
	 ((list 'acl2::implies hyps conc) (conjunction-to-list hyps))
	 (otherwise nil)))

;; Get the conclusion of an implication, returning the given statement if it
;; is not an implication
(defun get-conc (stmt)
  (match stmt
	 ((list 'acl2::implies hyps conc) conc)
	 (otherwise stmt)))

;; A call to guard-obligation inside of an acl2s-query should return something of the form:
;; ('ctx error-message) if guard-obligation had an internal error
;; (i.e. if the expression whose guard obligations are being asked for contains an undefined function)
;; or
;; (nil (term-info obligations)) otherwise
;; where term-info is of the form (:term ...)
;; and obligations is a list of obligations
(defun guard-obligations-query-res-ok? (res)
  (and (consp res)
       (>= (length res) 2)
       (consp (second res))
       (>= (length (second res)) 2)))

;; Get the guard obligations for the given expression, possibly with debug info (depending on debug argument)
;; This will return a CNF of the guard obligations
;; This will error if the internal acl2s-query returns an unexpected value, or if an error is detected
(defun guard-obligations-query (expr debug)
  (let ((res (acl2s-query `(acl2s::mv-let
			    (erp val)
			    (acl2::guard-obligation ',expr nil ,debug 'ctx acl2::state)
			    (acl2s::mv erp val acl2::state)))))
    (cond ((not (guard-obligations-query-res-ok? res))
           (error "acl2s-query returned an unexpected response in guard-obligations-query: ~a" res))
	  ((or (car res) (equal (car res) 'acl2s::ctx)) ;; TODO: make this less ugly
	   (error 'rguard-obligations-error :expr expr :err res))
	  (t (second (second res))))))

;; Get the guard obligations of the given expression, with debug info
;; This returns a list containing lists where:
;; the first element is the debug info (describing where the obligation came from)
;; the second element is a statement describing the obligations themselves
;; Obligations are converted to ors here so that they can be directly run in ACL2
;; ACL2-numberp is replaced by rationalp inside of obligations
(defun rguard-obligations-debug (expr)
  (let ((val (guard-obligations-query expr t)))
    (mapcar (lambda (x)
	      (list (second (car x))
		    (replace-in (cnf-disjunct-to-or (cdr x))
				'acl2::acl2-numberp
				'acl2s::rationalp)))
	    val)))

;; Get the guard obligations of the given expression
;; This returns a list containing statements describing the obligations themselves
;; Obligations are converted to ors here so that they can be directly run in ACL2
;; ACL2-numberp is replaced by rationalp inside of obligations
(defun rguard-obligations (expr)
  (let ((val (guard-obligations-query expr nil)))
    (mapcar (lambda (x) (replace-in (cnf-disjunct-to-or x)
				    'acl2::acl2-numberp
				    'acl2s::rationalp))
	    val)))

;; This function:
;; 1. runs test? on the given query. If a counterexample is found, errors.
;;    Otherwise, if no counterexample is found,
;; 2. runs thm in minimal theory. If this succeeds then t is returned.
;;    Otherwise, if thm in minimal theory fails,
;; 3. runs thm in current theory. If this succeeds then t is returned. Otherwise, errors.
(defun test-thm-min-then-full (q)
  (let* ((test-res (itest?-query q))
	 (found-cx? (car test-res))
	 (cxs (second test-res)))
    (if found-cx?
	(error 'test-found-ctrex-error :cxs cxs :query q)
      (let* ((min-thm-res (thm-min-theory q))
	     (min-thm-err (thm-query-error? min-thm-res)))
	(if (not min-thm-err)
	    t
	  (let* ((full-thm-res (thm-full-theory q))
		 (full-thm-err (thm-query-error? full-thm-res)))
	    (if (not full-thm-err)
		(progn (signal 'used-full-theory :query q) t)
	      (error 'full-thm-failed-error :query q))))))))

;; Run test? on the given statement, and if no ctrex is found, run a thm in minimal theory.
;; Errors if either a ctrex is found, or the thm in minimal theory fails.
;; Otherwise returns t
(defun test-thm-min (q)
  (let* ((test-res (itest?-query q))
	 (found-cx? (car test-res))
	 (cxs (second test-res)))
    (if found-cx?
	(error 'test-found-ctrex-error :cxs cxs :query q)
      (let* ((min-thm-res (thm-min-theory q))
	     (min-thm-err (thm-query-error? min-thm-res)))
	(if (not min-thm-err)
	    t
	  (error 'min-thm-failed-error :query q))))))

;; Run test? on the given statement, and if no ctrex is found, run a
;; thm in minimal theory plus the given rules, and the given goal hints.
;; Errors if either a ctrex is found, or the thm in minimal theory fails.
;; Otherwise returns t
(defun test-thm-min-with-rules (q thm-rules &optional thm-goal-hints)
  (let* ((test-res (itest?-query q))
	 (found-cx? (car test-res))
	 (cxs (second test-res)))
    (if found-cx?
	(error 'test-found-ctrex-error :cxs cxs :query q)
      (let* ((min-thm-res (thm-min-theory-with-rules q thm-rules thm-goal-hints))
	     (min-thm-err (thm-query-error? min-thm-res)))
	(if (not min-thm-err)
	    t
	  (error 'min-thm-failed-error :query q))))))

;; Run test? on the given statement, and if no ctrex is found, run a
;; thm in minimal theory plus the given rules, and the given goal hints.
;; Errors if either a ctrex is found, or the thm in minimal theory + contracts fails.
;; Otherwise returns t
(defun test-thm-min-with-rules-and-contracts (q thm-rules &optional thm-goal-hints)
  (let* ((test-res (itest?-query q))
	 (found-cx? (car test-res))
	 (cxs (second test-res)))
    (if found-cx?
	(error 'test-found-ctrex-error :cxs cxs :query q)
      (let* ((min-thm-res (thm-min-theory-with-rules-and-contracts q thm-rules thm-goal-hints))
	     (min-thm-err (thm-query-error? min-thm-res)))
	(if (not min-thm-err)
	    t
	  (error 'min-thm-failed-error :query q))))))

;; Run test? on the given statement, and if no ctrex is found, run a thm in current theory.
;; Errors if either a ctrex is found, or the thm in current theory fails.
;; Otherwise returns t
(defun test-thm-full (q)
  (let* ((test-res (itest?-query q))
	 (found-cx? (car test-res))
	 (cxs (second test-res)))
    (if found-cx?
	(error 'test-found-ctrex-error :cxs cxs :query q)
      (let* ((full-thm-res (thm-full-theory q))
	     (full-thm-err (thm-query-error? full-thm-res)))
	(if (not full-thm-err)
	    t
	  (error 'full-thm-failed-error :query q)
	  )))))

;; This returns nil if neither test? nor thm fail on the given statement
;; Otherwise it either returns a list containing the counterexamples (if test? fails)
;; or t if thm fails.
(defun test-then-thm-min-fails? (q)
  (handler-case (test-thm-min q)
    (:no-error () nil)
    (test-found-ctrex-error (condition) (test-found-ctrex-error/cxs condition))
    (min-thm-failed-error () t)))

(defun acl2s-current-theory ()
  (second (acl2s-query 'acl2::(let ((world (w state)))
				(mv nil (current-theory :here) state))
		       :pre-eval-print nil
                       :post-eval-print nil)))

(defun theories-diff (thy1 thy2)
  (second
   (acl2s-query
    `(let ((acl2::world (acl2::w acl2::state)))
	     (acl2::mv nil (acl2::set-difference-theories ',thy1 ',thy2) acl2::state))
    :pre-eval-print nil
    :post-eval-print nil)))

(defun untranslate (x)
  (cadr (acl2s-query
	 `(acl2s::mv nil
		     (acl2s::untranslate ',x nil
					 (acl2s::w acl2s::state))
		     acl2s::state))))

(defun define-thm-ruleclasses-nil (name thm-stmt)
  (acl2s-event `(acl2::defthm ,name ,thm-stmt :rule-classes nil)))

;; Include a book in the world
(defun include-book-event (book)
  (acl2s-event `(acl2s::include-book ,book)))

;; Reset the ACL2 state back to before the definition of START-LOAD-FILE
(defun reset-file ()
  (acl2::ld 'acl2s::(:ubt START-LOAD-FILE)))

(defun create-reset-point ()
  ;; Create this function so that we can come back to this point in ACL2's history
  (acl2s-event 'acl2s::(defun START-LOAD-FILE () t)))
