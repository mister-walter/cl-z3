;; Generate (:exports ...) entries for the z3-api defcfuns

(load "~/quicklisp/setup.lisp")
(ql:quickload :cffi)

(defvar z3-api-forms
  (with-open-file (s "./z3-api.lisp" :direction :input)
                  (loop for v = (read s nil :eof)
                        while (not (equal v :eof))
                        collect v)))

(with-open-file (s "exports.lisp" :direction :output)
                (loop for item in z3-api-forms
                      when (and (consp item) (or (equal (car item) 'defcfun) (equal (car item) 'defcfun?)))
                      do (format s "#:~S~%" (cffi:translate-name-from-foreign (second item) *package*))))
