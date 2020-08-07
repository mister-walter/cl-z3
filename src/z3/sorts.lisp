(in-package :z3)

(defvar *sorts* (make-hash-table))

(defun register-sort (name sort-producer)
  "Register a sort."
  (setf (gethash name *sorts*) sort-producer))

;; TODO: optimization - memoize calls within a context
;; i.e. (z3-mk-int-sort ctx) will always return the same value for the same value of ctx (AFAIK unless ctx is reset)
;; (but in that case all pointers that we have into Z3 are invalid so ¯\_(ツ)_/¯)
(defun get-sort (name context)
  "Get the sort associated with a name"
  (funcall (gethash name *sorts*) context))

(register-sort :int #'z3-mk-int-sort)
(register-sort :bool #'z3-mk-bool-sort)
