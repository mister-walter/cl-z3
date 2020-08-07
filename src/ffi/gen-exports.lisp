;; Generate (:exports ...) entries for the z3-api defcfuns
(pushnew (truename "/home/drew/lisp-z3/") ql:*local-project-directories* )
(ql:register-local-projects)
(ql:quickload :lisp-z3)

(defvar foo
  (with-open-file (s "/home/drew/lisp-z3/src/ffi/z3-api.lisp" :direction :input)
                  (loop for v = (read s nil :eof)
                        while (not (equal v :eof))
                        collect v)))

(with-open-file (s "exports.lisp" :direction :output)
                (loop for item in foo
                      when (and (consp item) (equal (car item) 'defcfun))
                      do (format s "#:~S~%" (cffi:translate-name-from-foreign (second item) *package*))))
