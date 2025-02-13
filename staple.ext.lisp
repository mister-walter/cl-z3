(asdf:load-system :staple-markdown)

(defclass staple::my-page (staple:simple-page) ())

;; Don't strip hyphens from page titles.
(defmethod shared-initialize :after ((page staple::my-page) slots &key staple:system staple:title)
  (declare (ignore staple:title))
  (setf (staple:title page) (asdf:component-name (asdf:find-system (or staple:system (staple:system page))))))

;;(defmethod staple:page-type ((system (eql (asdf:find-system :cl-z3))))
;;  'my-page)

(defmethod staple:page-type ((system (eql (asdf:find-system :cl-z3/ffi))))
  'staple::my-page)

(defmethod staple:page-type ((system (eql (asdf:find-system :cl-z3/z3))))
  'staple::my-page)

(defmethod staple:format-documentation ((docstring string) (page staple::my-page))
  (let ((*package* (first (staple:packages page))))
    (staple:markup-code-snippets ;;-ignoring-errors
     (staple:compile-source docstring :markdown))))

(defmethod staple:subsystems ((system (eql (asdf:find-system :cl-z3))))
  (list (asdf:find-system :cl-z3/z3)
        (asdf:find-system :cl-z3/ffi)))
