;; SPDX-FileCopyrightText: Copyright (c) 2020 Andrew T. Walter <me@atwalter.com>
;; SPDX-License-Identifier: MIT
(in-package :z3)

(import 'z3-c::(Z3-del-config Z3-set-param-value
                Z3-global-param-set Z3-global-param-get Z3-global-param-reset-all))

(defun make-config ()
  (make-instance 'config))

(defun del-config (config)
  (Z3-del-config config))

(defun set-config-param (config id value)
  (Z3-set-param-value config id value))

(defun reset-global-params ()
  (Z3-global-param-reset-all))

(defun set-global-param (id value)
  "Set the global parameter with the given id to the given value.
   Prints a warning if no such parameter exists or if the value is of an incorrect type."
  (Z3-global-param-set id value))

(defun get-global-param (id)
  "Get a string representing the current value of the global parameter with the given id.
   Returns nil and prints a warning if no such parameter exists."
  (with-foreign-pointer (retval 1)
                        (Z3-global-param-get id retval)
                        (values (foreign-string-to-lisp (mem-ref retval :pointer)))))
