(pushnew (truename "../") ql:*local-project-directories*)
(ql:register-local-projects)
(ql:quickload :lisp-z3)

(defpackage :z3-sudoku
  (:use :cl :z3))

(in-package :z3-sudoku)

(import 'z3::(distinct))

(defun idx-to-cell-symbol (idx)
  (assert (and (>= idx 0) (<= idx 81)))
  (intern (concatenate 'string "C" (write-to-string idx))))

;; We'll encode the sudoku grid in the simplest way possible, 81 integers
(defconstant +cell-vars+
  (loop for idx below 81
        append (list (idx-to-cell-symbol idx) :int)))

;; We limit the integers to values between 1 and 9, inclusive
(defconstant cell-range-constraints
  (loop for idx below 81
        append `((<= 1 ,(idx-to-cell-symbol idx))
                 (>= 9 ,(idx-to-cell-symbol idx)))))

;; The values in each row must be distinct
(defconstant row-distinct-constraints
  (loop for row below 9
        collect (cons 'distinct
                      (loop for col below 9
                            collect (idx-to-cell-symbol (+ (* 9 row) col))))))

;; The values in each column must be distinct
(defconstant col-distinct-constraints
  (loop for col below 9
        collect (cons 'distinct
                      (loop for row below 9
                            collect (idx-to-cell-symbol (+ (* 9 row) col))))))

;; The values in each 3x3 box must be distinct
(defconstant box-distinct-constraints
  ;; These numbers are the indices of the top-left square of each box
  (loop for box-start in '(0 3 6 27 30 33 54 57 60)
        collect (cons 'distinct
                      ;; These numbers are the offsets of each square in a box from the index of the box's top-left square
                      (loop for box-offset in '(0 1 2 9 10 11 18 19 20)
                            collect (idx-to-cell-symbol (+ box-start box-offset))))))

;; This generates constraints based on a "starting grid".
;; This starting grid is simply a length-81 list representation of the 9x9 sudoku grid in row-major order.
;; The list should have a _ in cells where no initial value is given.
;; Some examples of input grids can be seen below in `a-hard-sudoku-grid` and `a-very-hard-sudoku-grid`.
(defun input-grid-constraints (grid)
  (assert (eq (length grid) 81))
  (loop for entry in grid
        for idx below 81
        when (not (equal entry '_))
        collect `(= ,(idx-to-cell-symbol idx) ,entry)))

;; TODO: there's definitely a way to do this without eval.
(defun assert-computed-with-cell-vars (stmt)
  (eval `(z3-assert ,+cell-vars+ ,stmt)))

(defun solve-grid (input-grid)
  (solver-init)
  (assert-computed-with-cell-vars (cons 'and cell-range-constraints))
  (assert-computed-with-cell-vars (cons 'and row-distinct-constraints))
  (assert-computed-with-cell-vars (cons 'and col-distinct-constraints))
  (assert-computed-with-cell-vars (cons 'and box-distinct-constraints))
  (assert-computed-with-cell-vars (cons 'and (input-grid-constraints input-grid)))
  (check-sat))

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

(solve-grid a-hard-sudoku-grid)

(defconstant a-very-hard-sudoku-grid
  '(_ _ _   _ _ _   _ 1 2
    _ _ _   _ _ _   _ _ 3
    _ _ 2   3 _ _   4 _ _

    _ _ 1   8 _ _   _ _ 5
    _ 6 _   _ 7 _   8 _ _
    _ _ _   _ _ 9   _ _ _

    _ _ 8   5 _ _   _ _ _
    9 _ _   _ 4 _   5 _ _
    4 7 _   _ _ 6   _ _ _))

(solve-grid a-very-hard-sudoku-grid)

;; Don't worry about the pretty-print definitions below, just some 
;; TODO: I'm sure there's a way to do this using just (format) and macros.
(defmacro pretty-print-sudoku-solution-helper (assignment)
  `(let ,assignment
     ,@(loop for row below 9
             collect '(terpri)
             append (loop for col below 9
                          collect `(format t "~A " ,(idx-to-cell-symbol (+ col (* 9 row))))
                          when (equal (mod col 3) 2)
                          collect '(format t "  "))
             when (equal (mod row 3) 2)
             collect '(terpri))))

(defun pretty-print-sudoku-solution (assignment)
  (eval `(pretty-print-sudoku-solution-helper ,assignment)))

(pretty-print-sudoku-solution (solve-grid a-very-hard-sudoku-grid))
