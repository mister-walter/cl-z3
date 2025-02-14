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

(defmethod staple::resolve-source-link (source (page staple::my-page))
  (if (pathname-utils:subpath-p (truename (getf source :file))
                                (truename (asdf:system-source-directory (staple::system page))))
      (let* ((system (asdf:find-system :cl-z3))
             (homepage (asdf:system-homepage system)))
        (if (search "github" homepage)
            (format NIL "~a/blob/~a/~a~@[#L~a~]"
                    (staple::github-project-root homepage)
                    (staple::current-commit system)
                    (staple::enough-namestring (getf source :file)
                                               (asdf:system-source-directory system))
                    (getf source :row))
            (call-next-method)))
      (call-next-method)))

(defmethod staple:format-documentation ((docstring string) (page staple::my-page))
  (let ((*package* (first (staple:packages page))))
    (staple:markup-code-snippets ;;-ignoring-errors
     (staple:compile-source docstring :markdown))))

(defmethod staple:subsystems ((system (eql (asdf:find-system :cl-z3))))
  (list (asdf:find-system :cl-z3/z3)
        (asdf:find-system :cl-z3/ffi)))
