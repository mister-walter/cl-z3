(in-package "ACL2S")

;;
;; An example showing off the basic use of itest?
;;

(include-book "../top")

;; An example from Harsh's thesis on defdata
(defdata triple (list pos pos pos))
(definec trianglep (v :all) :boolean
  (and (triplep v)
       (< (third v)  (+ (first v)  (second v)))
       (< (first v)  (+ (second v) (third v)))
       (< (second v) (+ (first v)  (third v)))))

(defun triangle-enum (n)
  (let ((x (nth-triple n)))
    (if (trianglep x)
        x
      '(1 1 1))))

(register-type triangle
  :predicate trianglep
  :enumerator triangle-enum)

(defdata triangle-kind
  (oneof :equilateral :isosceles :scalene))

(definec shape (v :triangle) :triangle-kind
  (cond ((equal (first v) (second v))
         (if (equal (second v) (third v))
             :equilateral
           :isosceles))
        ((equal (second v) (third v)) :isosceles)
        ((equal (first v) (third v)) :isosceles)
        (t :scalene)))

:q

;;;
;;; These are some helper functions
;;;

(defun acl2s-query-error? (res)
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
  (let* ((query `(itest? ,q))
	 (res (acl2s-query query :prover-step-limit nil :ld-pre-eval-print t)))
    (cond ((acl2s-query-error? res)
	   (error "Error occurred running itest? query ~S" query))
	  ((not (itest?-query-res-ok? res))
           (error "itest? query ~S resulted in an unexpected response ~S" query res))
	  (t (list (car (second res)) ;; whether a ctrex was found
		   (cdr (second (second res))) ;; list of ctrexes found
		   )))))

(defun get-let-binding-value (var binding)
  (let ((pair (assoc var binding)))
    (if (not pair)
        nil
      (cadr pair))))

;;;
;;; Say we want to generate a triangle of a given kind. We can do that
;;; by asking test? whether it can find a counterexample to the
;;; statement that "x is a triangle implies x cannot be of type
;;; <type>"
;;;

(defun get-triangle-of-kind (kind)
  (when (not (triangle-kindp kind)) (error "~S is not a known triangle kind!" kind))
  (let ((res (itest?-query `(implies (trianglep x) (not (equal (shape x) ,kind))))))
    (if (car res)
        (get-let-binding-value 'x (car (second res)))
      nil)))

(get-triangle-of-kind :isosceles)
(get-triangle-of-kind :equilateral)
(get-triangle-of-kind :scalene)

;;;
;;; Note that this is a little inefficient if we want to generate many
;;; triangles - we're making a call to itest? each time we want a
;;; triangle.
;;;
;;; Instead, one can ask test? to generate more counterexamples, and
;;; use them.
;;;

(defun get-triangles-of-kind-v1 (kind num-triangles)
  (when (not (triangle-kindp kind)) (error "~S is not a known triangle kind!" kind))
  (acl2s-event `(acl2s-defaults :set num-counterexamples ,num-triangles))
  (acl2s-event `(acl2s-defaults :set num-print-counterexamples ,num-triangles))
  (let ((res (itest?-query `(implies (trianglep x) (not (equal (shape x) ,kind))))))
    (if (car res)
        (mapcar #'(lambda (ctrex) (get-let-binding-value 'x ctrex)) (second res))
      nil)))

;;;
;;; Note that you may start to see counterexamples that don't satisfy (implies (trianglep x) (equal (shape x) <kind>)).
;;; test? will sometimes generate counterexamples that don't satisfy the top level form being tested.
;;; Currently the information as to which counterexamples are not consistent is not provided by itest?, but it may be provided in the future.
;;;

(get-triangles-of-kind-v1 :isosceles 5)
(get-triangles-of-kind-v1 :equilateral 5)
(get-triangles-of-kind-v1 :scalene 5)
