(in-package "ACL2S")

;;
;; An example showing off the use of itest?
;;

(include-book "../top")

:q

(defun acl2s-query-error? (res)
  (car res))

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
	 (res (acl2s-query query)))
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

(defun get-random-point-in-radius (radius &optional (retries 10))
  (let ((query `(implies (and (integerp x) (integerp y)) (not (< (+ (* x x) (* y y)) (* ,radius ,radius))))))
    (loop for i below retries
          for res = (itest?-query query)
          when (car res) return (list (get-let-binding-value 'x (car (second res)))
                                      (get-let-binding-value 'y (car (second res)))))))

(quiet-mode-on)

(defun run-for-radius (radius &optional (runs 500))
  (let ((cts (make-array (list (* 2 radius) (* 2 radius)) :element-type 'integer)))
    (loop for i below runs
          for (x y) = (get-random-point-in-radius radius)
          do (setf (aref cts (+ radius x) (+ radius y)) (1+ (aref cts (+ radius x) (+ radius y)))))
    (format t "~%   ")
    (loop for i below (* 2 radius)
          do (format t "~a~a " (if (minusp (- i radius)) "" " ") (- i radius)))
    (format t "~%")
    (loop for xi below (* 2 radius)
          do (format t "~a~a " (if (minusp (- xi radius)) "" " ") (- xi radius))
          do (loop for yi below (* 2 radius)
                   for cell = (aref cts xi yi)
                   do (format t (cond ((zerop cell) "   ")
                                      ((< cell (/ (* runs) (* 4 radius radius))) " . ")
                                      ((< cell (/ (* runs) (* 2 radius radius))) " + ")
                                      (t " # "))))
          do (format t "~%"))))

(run-for-radius 5)
