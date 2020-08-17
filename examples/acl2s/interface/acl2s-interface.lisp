;; This file is intended to be run from an ACL2s image that has been
;; dropped into raw lisp (i.e. by running :q)

(in-package "ACL2S")

(ld '((set-ignore-ok t)
      (assign acl2s-result nil)))

#| 
 This is a super-ugly hack to prevent ACL2s from printing some summary
 information when a function is defined.

 Essentially ACL2s uses the macro cw ("comment window") directly to
 print out a summary after a function is defined. For whatever reason
 ld's :standard-co option does not redirect these calls, and there is
 no other way to stop ACL2s from calling them.

 The way we solve this is to save the definition of one of the
 functions that the cw macro calls and to redefine it, having it do
 nothing if a parameter is set. This is potentially dangerous but
 seems to work.
|#
(defvar *fmt-to-comment-window-old* #'fmt-to-comment-window)
(defparameter *disable-comment-window-printing* nil)

(defun fmt-to-comment-window (&rest args)
  (if *disable-comment-window-printing*
      nil
    (apply *fmt-to-comment-window-old* args)))

;; Call ld with the given query and options
;; This allows us to set keyword options using a list
;;
;; We also locally disable a subset of SBCL redefinition
;; warnings. This is needed because ACL2s will sometimes redefine
;; something in the Cgen package, causing SBCL to emit a warning. This
;; can be bad when calling test? many times for example.
(defun ld-options (q options)
  (declare #+sbcl(sb-ext:muffle-conditions sb-kernel:redefinition-with-defun))
  (handler-bind (#+sbcl(sb-kernel:redefinition-with-defun #'muffle-warning))
    (eval `(ld ',q ,@options))))

;; Removes a property with a given key from a property list
;; non-destructively
(defun remove-prop (plist key)
  (cond
   ((endp plist) nil)
   ((endp (cdr plist)) nil) ;; just in case it's not a plist
   ((eq key (car plist)) (remove-prop (cddr plist) key))
   (t (cons (car plist)
	    (cons (cadr plist) (remove-prop (cddr plist) key))))))

(defun remove-props (plist keys)
  (if (endp keys)
      plist
    (remove-props (remove-prop plist (car keys))
		  (cdr keys))))

;; Flags for ld that turn off most of its output
(defconstant LD-QUIET-FLAGS '(:ld-pre-eval-print nil
			      :ld-post-eval-print nil
			      :ld-redefinition-action nil
			      :ld-prompt nil
			      :standard-co "/dev/null"
			      :proofs-co "/dev/null"
			      :ld-verbose nil))

;; Get the value associated with the given key in the given table
(declaim (ftype (function (symbol symbol) *) get-table-value))
(defun get-table-value (table key)
  (cdr (assoc-equal key (getpropc table 'table-alist nil (w state)))))

;; These variables save the values of settings at the time quiet mode
;; is enabled, so that we can restore them when quiet mode is
;; disabled. The default values are the default values for these
;; settings when this file is run.
(defvar *saved-inhibit-output-list* (@ acl2::inhibit-output-lst))
(defvar *saved-verbosity-level* (acl2s-defaults :get verbosity-level))
(defvar *saved-gag-mode* (gag-mode))
(defvar *saved-defunc-print-summary* (get-table-value 'defunc-defaults-table :print-summary))

;; Keep track of whether or not we're in quiet mode already
(defvar *quiet-mode-state* nil)

#|
 Enable quiet mode, turning off as much ACL2[s] output as possible.
|#
(defun quiet-mode-on ()
  (unless *quiet-mode-state*
    (let ((state acl2::*the-live-state*))
      (setf *saved-inhibit-output-list* (@ acl2::inhibit-output-lst))
      (setf *saved-verbosity-level* (acl2s-defaults :get verbosity-level))
      (setf *saved-gag-mode* (@ gag-mode))
      (setf *saved-defunc-print-summary* (table defunc-defaults-table :print-summary))
      (setf *disable-comment-window-printing* t)
      (ld-options '((set-inhibit-output-lst acl2::*valid-output-names*)
                    (acl2s-defaults :set verbosity-level 0)
                    ;; Set defunc to not print out summaries by default
                    (table defunc-defaults-table :print-summary nil)
                    (set-gag-mode t))
                  LD-QUIET-FLAGS)
      (setf *quiet-mode-state* t))))

#|
 Disable quiet mode, trying to restore settings to as close as
 possible reflect the state prior to quiet-mode-on (if it was called
 previously), or defaults otherwise
|#
(defun quiet-mode-off ()
  (when *quiet-mode-state*
    (let ((state acl2::*the-live-state*))
      (ld-options `((set-inhibit-output-lst ',*saved-inhibit-output-list*)
                    (acl2s-defaults :set verbosity-level ,*saved-verbosity-level*)
                    (table defunc-defaults-table :print-summary ,*saved-defunc-print-summary*)
                    (set-gag-mode ,*saved-gag-mode*))
                  LD-QUIET-FLAGS)
      (setf *disable-comment-window-printing* nil)
      (setf *quiet-mode-state* nil))))

#| 
 The point of the next few forms is that we can use ACL2s from within
 lisp. That will be useful to check that your code works.
|#

(defun acl2s-last-result ()
  (let ((state acl2::*the-live-state*))
    (@ acl2s::acl2s-result)))

(defun save-result (val)
  (ld-options `((assign acl2s::acl2s-result ,val))
	      LD-QUIET-FLAGS))

#|

 If c is acl2s computation such as 

 (+ 1 2)
 (append '(1 2) '(3 4))

 etc.

 then the following form will ask ACL2 to evaluate c and will update
 the ACL2 global result to contain a list whose car is a flag
 indicating whether an error occurred, so nil means no error, and whose
 second element is the result of the computation, if no error occurred.

 The keyword argument 'quiet' will turn off as much ACL2s output as
 possible.

 Note that any additional arguments will be passed to ld. This can be
 used to provide keyword arguments that customize ld's behavior.

Here's an older definition

(defun acl2s-compute (c)
  (let ((state acl2::*the-live-state*))
    (multiple-value-bind (erp val state)
        (ld `((assign acl2s::acl2s-result ,c)))
      (if (equal val :eof)
          (ld `((assign acl2s::acl2s-result (list nil (@ acl2s::acl2s-result)))))
        (ld `((assign acl2s::acl2s-result (list t nil))))))
    (acl2s-last-result)))

|#

(defun acl2s-compute (c &rest args &key (quiet nil) &allow-other-keys)
  (let ((turned-quiet-mode-on (and quiet (not *quiet-mode-state*))))
    (if turned-quiet-mode-on (quiet-mode-on) nil)
    (let ((state acl2::*the-live-state*))
      (multiple-value-bind (erp val state)
			   (ld-options `((assign acl2s::acl2s-result ,c))
				       (append (remove-prop args :quiet) (if quiet LD-QUIET-FLAGS nil)))
			   (if (equal val :eof)
			       (save-result `(list nil (@ acl2s::acl2s-result)))
			     (save-result `(list t nil))))
      (progn (if turned-quiet-mode-on (quiet-mode-off) nil)
	     (acl2s-last-result)))))

#|

Here are some examples

(acl2s-compute '(+ 1 2))
(acl2s-compute '(+ 1 2) :ld-pre-eval-print t :ld-post-eval-print t)
(acl2s-compute '(append '(1 2) '(3 4)))
(acl2s-compute '(+ 1  '(1 2)))

|#


#|

 If q is acl2s query that returns an error-triple such as 

 (pbt 0)
 (test? (equal x x))
 (thm (equal x x))

 etc.

 then the following form will ask ACL2 to evaluate q and will update
 the ACL2 global result to contain a list whose car is a flag
 indicating whether an error occurred, so nil means no error, and whose
 second element is nil.

 The prover-step-limit is set to a default value, which may need to be
 updated, based on the application. This can be done by providing the
 prover-step-limit keyword argument, for example:
 (acl2s-query '(thm (implies (and (natp x) (natp y)) 
                             (>= (+ (abs x) (abs y)) (abs (+ x y))))) 
              :prover-step-limit 10)

 The above query should return (t nil), indicating that the proof
 failed due to the prover exceeding the step limit. Removing the
 :prover-step-limit argument allows the proof to go through.

 The keyword argument 'quiet' will turn off as much ACL2s output as
 possible.

 Note that any additional arguments will be passed to ld. This can be
 used to provide keyword arguments that customize ld's behavior.

 Here is a previous version of the function.

(defun acl2s-query (q)
  (let ((state acl2::*the-live-state*))
    (ld `((mv-let
           (erp val state)
           ,q
           (assign result (list erp nil)))))
    (acl2s-last-result)))

|#

(defun acl2s-query (q &rest args &key (quiet nil) (prover-step-limit 3000000) (ld-error-action :continue) &allow-other-keys)
  (let ((turned-quiet-mode-on (and quiet (not *quiet-mode-state*))))
    (if turned-quiet-mode-on (quiet-mode-on) nil)
    (let ((state acl2::*the-live-state*))
      (ld-options `((set-prover-step-limit ,prover-step-limit)
		    (mv-let
		     (erp val state)
		     ,q
		     (assign acl2s::acl2s-result (list erp val))))
		  (append (remove-props args '(:quiet :prover-step-limit))
			  (append (if (eq ld-error-action :continue) nil (list :ld-error-action ld-error-action))
				  (if quiet LD-QUIET-FLAGS nil))))
      (progn (if turned-quiet-mode-on (quiet-mode-off) nil)
	     (acl2s-last-result)))))

#|

 Here are some examples you can try to see how acl2s-query works.

 (acl2s-query '(pbt 0))
 (acl2s-query '(pbt 0) :post-eval-print nil)
 (acl2s-query '(pbt 0) :pre-eval-print nil)
 (acl2s-query '(pbt 0) :pre-eval-print nil :post-eval-print nil)
 (acl2s-query '(test? (equal x y)))
 (acl2s-query '(thm (equal x x)))
 (acl2s-query '(thm (iff (implies p q)
                         (or (not p) q))))
 (acl2s-query '(thm (implies (and (natp x) (natp y)) 
                             (>= (+ (abs x) (abs y)) (abs (+ x y))))) 
              :prover-step-limit 10)

|#

#|

 A function that determines if f is a function defined in ACL2s and if
 so, its arity (number of arguments). If f is not a function, then the
 arity is nil. Otherwise, the arity is a natural number. Note that f
 can't be a macro.

|#

(defun acl2s-arity (f)
  (second (acl2s-compute `(acl2::arity ',f (w state)))))

#|
 Some examples

 (acl2s-arity 'append)        ; is nil since append is a macro
 (acl2s-arity 'binary-append) ; is 2

|#


#|

 If e is acl2s event such as 

 (definec f (x :int) :int 
    (* x x))

 then the following form will ask ACL2 to process the event and will
 update the ACL2 global result to contain a list whose car is a flag
 indicating whether an error occurred, so nil means no error, and
 whose second element is the value returned (can be ignored).

 The prover-step-limit is set to a default value, which may need to be
 updated, based on the application. See the documentation for
 acl2s-query for more information and examples on how to do this.

 The keyword argument 'quiet' will turn off as much ACL2s output as
 possible.

 Note that any additional arguments will be passed to ld. This can be
 used to provide keyword arguments that customize ld's behavior.

 Here is a previous version of the function.

|#

(defun acl2s-event (e &rest args &key (quiet nil) (prover-step-limit 3000000) &allow-other-keys)
  (let ((turned-quiet-mode-on (and quiet (not *quiet-mode-state*))))
    (if turned-quiet-mode-on (quiet-mode-on) nil)
    (let ((state acl2::*the-live-state*))
      (multiple-value-bind (erp val state)
			   (ld-options `((set-prover-step-limit ,prover-step-limit)
					 ,e)
				       (append (remove-props args '(:quiet :prover-step-limit))
					       (if quiet LD-QUIET-FLAGS nil)))
			   (setf erp (not (equal val :eof)))
			   (save-result `(list ',erp ',val))
			   (progn (if turned-quiet-mode-on (quiet-mode-off) nil)
				  (list erp val))))))

#|
 Some examples

 (acl2s-event 'acl2s::(definec f (x :int) :int (* x x)))
 (acl2s-event 'acl2s::(definec g (x :int) :int (* 5 x)) :quiet t)
 (acl2s-event 'acl2s::(defthm triangle-inequality
                              (implies (and (natp x) (natp y)) 
                                       (>= (+ (abs x) (abs y)) (abs (+ x y)))))
              :prover-step-limit 1000)
 (acl2s-query '(pbt 0))

|#
