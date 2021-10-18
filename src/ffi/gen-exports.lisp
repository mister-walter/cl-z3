;; Generate (:exports ...) entries for the z3-api defcfuns

(load "~/quicklisp/setup.lisp")
(ql:quickload :cffi)

(defvar foo
  (with-open-file (s "./z3-api.lisp" :direction :input)
                  (loop for v = (read s nil :eof)
                        while (not (equal v :eof))
                        collect v)))

(with-open-file (s "exports.lisp" :direction :output)
                (loop for item in foo
                      when (and (consp item) (equal (car item) 'defcfun))
                      do (format s "#:~S~%" (cffi:translate-name-from-foreign (second item) *package*))))
