(defpackage #:z3
  (:use #:cl #:z3-c)
  (:shadow #:sort #:optimize)
  (:import-from :trivia :match)
  (:import-from :cffi :translate-to-foreign))
