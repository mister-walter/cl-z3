(in-package "ACL2S")
(include-book "../top")
:q

(defmacro assert-error (stmt &optional (error-type 'error))
  `(handler-case ,stmt
     (,error-type (_) t)
     (error (condition) (error "Expected ~S to produce error of type ~S, but instead produced error of type ~S" ',stmt ',error-type condition))
     (:no-error (val) (error "Expected ~S to produce error of type ~S, but instead evaluated to ~S" ',stmt ',error-type val))))

(defmacro assert-equal (stmt1 stmt2)
  `(let ((res1 ,stmt1)
         (res2 ,stmt2))
     (if (equal res1 res2)
         t
       (error "Expected~%~S and~%~S~% to evaluate to equal values. Instead evaluated to ~%~S and ~%~S~% respectively." ',stmt1 ',stmt2 res1 res2))))

(assert-equal (acl2s-compute '(+ 1 2))
              '(nil 3))

(assert-equal (acl2s-compute '(append '(1 2) '(3 4)))
              '(nil (1 2 3 4)))

(assert-equal (acl2s-compute '(+ 1 '(1 2)))
              '(t nil))


(assert-equal (acl2s-query '(value (+ 1 2)))
              '(nil 3))

(assert-equal (acl2s-query '(thm (implies (natp x) (integerp x))))
              '(nil :invisible))

(assert-equal (acl2s-query '(mv nil t state))
              '(nil t))

(assert-equal (acl2s-query '(trans-eval '(+ 1 2) 'my-ctx state nil))
              '(nil ((nil) . 3)))

(assert-equal (acl2s-event 'acl2s::(definec f (x :int) :int (* x x)))
              '(nil :eof))

(assert-equal (acl2s-compute '(f 5))
              '(nil 25))
