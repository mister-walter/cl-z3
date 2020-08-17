;; (load "~/quicklisp/setup.lisp")
(pushnew (truename "../") ql:*local-project-directories*)
(ql:register-local-projects)
(ql:quickload :lisp-z3)

(defpackage :z3-sudoku-arbitrary-size
  (:use :cl :z3))

(in-package :z3-sudoku-arbitrary-size)

;; This is a built-in Z3 function
(import 'z3::(distinct))

;; We represent values of an finite-domain type as (fd-val <type> <value>)
;; This is a temporary solution until we determine a better way to represent them in CL
(import 'z3::(fd-val))

(defun idx-to-cell-symbol (idx)
  (intern (concatenate 'string "C" (write-to-string idx))))

(defun val-to-cell-value (val)
  `(fd-val :cell ,val))

(defun cell-vars (n)
  (loop for idx below (* n n n n)
        append (list (idx-to-cell-symbol idx) :cell)))

;; The values in each row must be distinct
(defun row-distinct-constraints (n)
  (loop for row below (* n n)
        collect (cons 'distinct
                      (loop for col below (* n n)
                            collect (idx-to-cell-symbol (+ (* n n row) col))))))

;; The values in each column must be distinct
(defun col-distinct-constraints (n)
  (loop for col below (* n n)
        collect (cons 'distinct
                      (loop for row below (* n n)
                            collect (idx-to-cell-symbol (+ (* n n row) col))))))

(defun box-cell-offsets (n)
  (loop for col below n
        append (loop for row below n
                     collect (+ (* n n row) col))))

(defun box-starts (n)
  (mapcar (lambda (offset) (* n offset)) (box-cell-offsets n)))
              
;; The values in each 3x3 box must be distinct
(defun box-distinct-constraints (n)
  ;; These numbers are the indices of the top-left square of each box
  (loop for box-start in (box-starts n)
        collect (cons 'distinct
                      ;; These numbers are the offsets of each square in a box from the index of the box's top-left square
                      (loop for box-offset in (box-cell-offsets n)
                            collect (idx-to-cell-symbol (+ box-start box-offset))))))

(defun input-grid-constraints (n grid)
  (assert (eq (length grid) (* n n n n)))
  (loop for entry in grid
        for idx below (* n n n n)
        when (not (equal entry '_))
        collect `(= ,(idx-to-cell-symbol idx) ,(val-to-cell-value entry))))

;; TODO: there's definitely a way to do this without eval.
(defun assert-computed-with-cell-vars (n stmt)
  (eval `(z3-assert ,(cell-vars n) ,stmt)))

(defun solve-grid (n input-grid)
  ;; We need to do solver-init here because we want to wipe out the old sort definition for :cell
  (solver-init)
  (register-finite-domain-sort :cell (* n n))
  (assert-computed-with-cell-vars n (cons 'and (row-distinct-constraints n)))
  (assert-computed-with-cell-vars n (cons 'and (col-distinct-constraints n)))
  (assert-computed-with-cell-vars n (cons 'and (box-distinct-constraints n)))
  (let ((input-constraints (input-grid-constraints n input-grid)))
    (when input-constraints (assert-computed-with-cell-vars n (cons 'and input-constraints))))
  ;;(print (z3::z3-solver-to-dimacs-string *default-context* *default-solver* t))
  (check-sat))

(defmacro define-symbol-conversions (&rest cases)
  `(progn
     (defun convert-symbol-to-num (sym)
       (trivia:match (symbol-name sym)
                     ,@cases))
     (defun convert-num-to-string (num)
       (ecase num
         ,@(mapcar #'reverse cases)))))

(define-symbol-conversions
    ("A" 10)
    ("B" 11)
    ("C" 12)
    ("D" 13)
    ("E" 14)
    ("F" 15)
    ("G" 16)
    ("H" 17)
    ("I" 18)
    ("J" 19)
    ("K" 20)
    ("L" 21)
    ("M" 22)
    ("N" 23)
    ("O" 24)
    ("P" 25)
    ("Q" 26)
    ("R" 27)
    ("S" 28)
    ("T" 29)
    ("U" 30)
    ("V" 31)
    ("W" 32)
    ("X" 33)
    ("Y" 34)
    ("Z" 35)
    ("a" 36)
    ("b" 37)
    ("c" 38)
    ("d" 39)
    ("e" 40)
    ("f" 41)
    ("g" 42)
    ("h" 43)
    ("i" 44)
    ("j" 45)
    ("k" 46)
    ("l" 47)
    ("m" 48)
    ("n" 49)
    ("o" 50)
    ("p" 51)
    ("q" 52)
    ("r" 53)
    ("s" 54)
    ("t" 55)
    ("u" 56)
    ("v" 57)
    ("w" 58)
    ("x" 59)
    ("y" 60)
    ("z" 61))

(defun preprocess-grid (grid &optional (number-offset -1))
  (loop for entry in grid
        collect (cond ((equal entry '_) '_)
                      ((numberp entry) (+ number-offset entry))
                      ((symbolp entry) (convert-symbol-to-num entry))
                      (t (error "Unsupported grid.")))))

(defun grid-value-to-string (val)
  (if (< val 9)
      (write-to-string (1+ val))
    (convert-num-to-string val)))

;; Don't worry about the pretty-print definitions below, just some 
;; TODO: I'm sure there's a way to do this using just (format) and macros.
(defmacro pretty-print-sudoku-solution-helper (n assignment)
  `(let ,assignment
     ,@(loop for row below (* n n)
             collect '(terpri)
             append (loop for col below (* n n)
                          collect `(format t "~A " (grid-value-to-string ,(idx-to-cell-symbol (+ col (* n n row)))))
                          when (equal (mod col n) (1- n))
                          collect '(format t "  "))
             when (equal (mod row n) (1- n))
             collect '(terpri))))

(defun pretty-print-sudoku-solution (n assignment)
  (eval `(pretty-print-sudoku-solution-helper ,n ,assignment)))


(defun blank-grid (n)
  (loop for i below (* n n n n)
        collect '_))

;; Formatted for readability.
(defconstant a-hard-sudoku-grid
  '(6 _ _   3 _ 1   _ 8 4
    _ _ _   _ 6 9   _ _ 7
    _ _ _   _ _ 7   _ 1 3

    4 _ _   6 9 _   _ _ 8
    _ _ _   _ 1 5   _ _ _
    _ _ 8   _ _ _   _ 6 _

    _ _ _   _ _ _   _ _ _
    _ _ _   1 _ _   7 _ _
    2 _ 4   _ _ 3   1 _ _))

(pretty-print-sudoku-solution 3 (solve-grid 3 (preprocess-grid a-hard-sudoku-grid)))

;; need a different definition for this for hexadoku
(defun grid-value-to-string (val)
  (if (<= val 9)
      (write-to-string val)
    (convert-num-to-string val)))

(defconstant hexadoku-grid
  '(1 _ 2 _   _ _ A 6   8 _ _ 5   4 B _ _
    D _ _ A   _ 8 0 _   B _ 9 _   _ 7 _ _
    _ B _ _   _ _ _ _   _ 2 7 _   _ E _ 9
    _ _ 9 _   _ 7 _ _   _ _ 0 _   D 8 5 A

    _ 0 _ F   1 _ _ 2   A 4 _ 6   _ _ _ 8
    _ 3 _ _   _ _ _ 9   _ _ _ _   _ _ _ _
    A 9 4 2   _ 5 _ D   _ _ C _   3 _ _ _
    _ _ _ _   _ _ 8 F   3 9 2 _   _ 4 _ _

    _ _ 1 _   _ 0 E 3   9 6 _ _   _ _ _ _
    _ _ _ 6   _ 9 _ _   F _ B _   2 A 4 D
    _ _ _ _   _ _ _ _   2 _ _ _   _ _ 1 _
    2 _ _ _   F _ D 8   1 _ _ C   B _ 6 _

    5 4 E 1   _ D _ _   _ _ 6 _   _ F _ _
    6 _ F _   _ 2 5 _   _ _ _ _   _ _ B _
    _ _ B _   _ F _ 0   _ 7 1 _   C _ _ 4
    _ _ 0 D   E _ _ A   4 3 _ _   _ 5 _ 1))

(pretty-print-sudoku-solution 4 (solve-grid 4 (preprocess-grid hexadoku-grid 0)))

(defconstant hexadoku-grid2
  '(0 1 2 3   4 5 6 7   8 9 A B   C D E F
    _ _ _ _   _ _ _ _   _ _ _ _   _ _ _ _
    _ _ _ _   _ _ _ _   _ _ _ _   _ _ _ _
    _ _ _ _   _ _ _ _   _ _ _ _   _ _ _ _

    _ _ _ _   _ _ _ _   _ _ _ _   _ _ _ _
    _ _ _ _   _ _ _ _   _ _ _ _   _ _ _ _
    _ _ _ _   _ _ _ _   _ _ _ _   _ _ _ _
    _ _ _ _   _ _ _ _   _ _ _ _   _ _ _ _

    _ _ _ _   _ _ _ _   _ _ _ _   _ _ _ _
    _ _ _ _   _ _ _ _   _ _ _ _   _ _ _ _
    _ _ _ _   _ _ _ _   _ _ _ _   _ _ _ _
    _ _ _ _   _ _ _ _   _ _ _ _   _ _ _ _

    _ _ _ _   _ _ _ _   _ _ _ _   _ _ _ _
    _ _ _ _   _ _ _ _   _ _ _ _   _ _ _ _
    _ _ _ _   _ _ _ _   _ _ _ _   _ _ _ _
    _ _ _ _   _ _ _ _   _ _ _ _   _ _ _ _))

;; very slow... (~260s)
;;(time (solve-grid 4 (preprocess-grid hexadoku-grid2 0)))

(pretty-print-sudoku-solution 3 (time (solve-grid 3 (blank-grid 3))))

;; very slow...
;;(pretty-print-sudoku-solution 4 (time (solve-grid 4 (blank-grid 4))))

;; strap in, things are about to get interesting
;;(setf (readtable-case *readtable*) :PRESERVE)

;;(SETF (READTABLE-CASE *READTABLE*) :UPCASE)
