;; This file is intended to be run from an ACL2s image
:q

(load "../acl2s-interface.lisp")

(ql:quickload :fiasco)

(fiasco:define-test-package :acl2s-interface-tests
                            (:use :cl :acl2 :acl2s :fiasco))

(in-package :acl2s-interface-tests)

(import 'acl2s::(acl2s-compute acl2s-query acl2s-event acl2s-arity))

(defmacro compute-result-is (stmt result)
  `(is (equal (acl2s-compute ',stmt) (list nil ,result))))

(defmacro compute-result-error (stmt)
  `(is (car (acl2s-compute ',stmt))))

(deftest acl2s-calculate-results-ok ()
  (compute-result-is (+ 1 2) 3)
  (compute-result-is 'a 'a)
  (compute-result-error (+ 1 a))
  (compute-result-error (+ 1 'a)))

;; -------------------------------------------
;; from books/centaur/bridge/bridge-raw.lsp
(defmacro with-acl2-channels-bound (channel &rest forms)
  `(progv
       (list (acl2::global-symbol 'acl2::proofs-co)
             (acl2::global-symbol 'acl2::standard-co)
             (acl2::global-symbol 'acl2::trace-co))
       (list ,channel ,channel ,channel)
     (progn . ,forms)))

(defmacro with-output-to (stream &rest forms)
  (let ((channel (gensym)))
    `(let* ((,channel        (gensym)))
       (setf (get ,channel acl2::*open-output-channel-type-key*) :character)
       (setf (get ,channel acl2::*open-output-channel-key*) ,stream)
       (unwind-protect
           (let ((*standard-output* ,stream)
                 (*trace-output*    ,stream)
                 (*debug-io*        ,stream)
                 (*error-output*    ,stream)
                 (acl2::*standard-co*     ,channel))
             (with-acl2-channels-bound ,channel . ,forms))
         ;; Invalidate the symbol's stream so the garbage collector can
         ;; reclaim the stream.
         (setf (get ,channel acl2::*open-output-channel-key*) nil)
         (setf (get ,channel acl2::*open-output-channel-type-key*) nil)))))
;; -------------------------------------------

(defmacro form-produces-output (form output)
  `(is (equal
        (let ((s (make-string-output-stream)))
          (unwind-protect
              (progn (with-output-to s ,form)
                     (get-output-stream-string s))
            (close s)))
        ,output)))

(deftest quiet-mode-works ()
  (form-produces-output (acl2s-compute '(+ 1 2) :quiet t) ""))

(fiasco:run-package-tests)
