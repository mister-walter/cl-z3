(in-package :z3)

(trivia:defpattern sym-name (val)
                   (let ((val-to-match
                          (if (symbolp val) (symbol-name val) val))
                         (it (gensym)))
                     `(trivia:guard1 (,it :type symbol) (symbolp ,it) (symbol-name ,it) ,val-to-match)))

#|
(trivia:match 'cl-user::foo
              ((sym-name foo) t)
              ((sym-name "baz") t)
              (otherwise nil))

(trivia:match :foo
              ((sym-name foo) t)
              ((sym-name "baz") t)
              (otherwise nil))

(trivia:match :baz
              ((sym-name foo) t)
              ((sym-name "baz") t)
              (otherwise nil))

(trivia:match :foo
              ((sym-name 'foo) t)
              ((sym-name "baz") t)
              (otherwise nil))
|#
