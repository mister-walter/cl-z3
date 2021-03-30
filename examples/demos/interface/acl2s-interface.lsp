(in-package "ACL2S")

;; Borrwed from centaur/vl/server/server-raw.lsp, which itself was
;; borrowed from the SBCL manual's section on "Defining Constants"
;; This is neccesary under SBCL, but it may or may not be neccesary
;; under other CL implementations.
(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

#| 
 This is a super-ugly hack to prevent ACL2s from printing some summary
 information when a function is defined.

 Essentially ACL2s uses the macro cw ("comment window") directly to
 print out a summary after a function is defined. ld's :standard-co
 option does not redirect these calls, and there is no other way to
 stop ACL2s from calling them. See
 https://github.com/acl2/acl2/issues/1008 for more information as to
 why this is the case.

 We solve this by saving the original definition of one of the
 functions that the cw macro calls, and defining our own version of
 the function that does nothing if a parameter is set. This is
 potentially dangerous but is effective.
|#
(define-constant *fmt-to-comment-window-old* #'fmt-to-comment-window)
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
;;
;; We also use ACL2's "with-suppression" macro to avoid package lock
;; errors when a user submits a query with a symbol in the function
;; call position that does not have a function definition. This is
;; used in `lp` for the same purpose.
(defun ld-options (q options)
  (declare #+sbcl(sb-ext:muffle-conditions sb-kernel:redefinition-with-defun))
  (handler-bind (#+sbcl(sb-kernel:redefinition-with-defun #'muffle-warning))
    (eval `(acl2::with-suppression (ld ',q ,@options)))))

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
(define-constant LD-QUIET-FLAGS
  '(:ld-pre-eval-print nil
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

(xdoc::defxdoc-raw acl2s-interface
  :parents (acl2-sedan)
  :short "An interface for interacting with ACL2s from Common Lisp."
  :long "
<p>WARNING: Loading this book will result in the redefinition of @(see
fmt-to-comment-window). This is unfortunately the only way we found to
control comment-window printing.</p>")

(xdoc::defxdoc-raw quiet-mode
  :parents (acl2s-interface)
  :short "Control whether or not the acl2s-interface attempts to suppress ACL2 printed output"
  :long "
Examples:
@({
 (quiet-mode-on)    ; from this point forward all acl2s-interface functions will
                    ; attempt to all suppress ACL2 printed output
 (quiet-mode-off)   ; (default) from this point forward all acl2s-interface
                    ; functions will print any ACL2 output as normal
})
<p>
Most of the ACL2s interface functions also take a @(':quiet') argument for
locally controlling quiet mode without affecting the global setting.
</p>
")

(xdoc::defxdoc-raw acl2s-interface-symbol-package-tips
  :parents (acl2s-interface)
  :short "Tips for dealing with symbol package issues when using the ACL2s interface"
  :long "
<p>When using any of the ACL2s interface functions from inside of a package other than \"ACL2\" or \"ACL2s\", you may run into problems because the symbols inside of any query literals will default to being interned in that package. This is mainly problematic when it comes to writing expressoions that call ACL2 functions.
</p>
<p>
The simplest way to solve this problem while still working in a non-ACL2-or-ACL2s package is to fully specify the names of all functions inside your queries. For example, one would write <tt>(acl2s-compute '(acl2::expt 2 3))</tt> instead of <tt>(acl2s-compute '(expt 2 3))</tt>.
</p>
<p>
This gets painful when writing queries containing many function calls. In this case, depending on your host Lisp, you may be able to use <a href=\"http://www.sbcl.org/manual/#Extended-Package-Prefix-Syntax\">extended package prefix notation</a>. For example, in SBCL, one could write <tt>(acl2s-compute 'acl2::(expt 2 (expt 2 3)))</tt> instead of <tt>(acl2s-compute '(acl2::expt 2 (acl2::expt 2 3)))</tt>.
</p>

<p>
Note that using extended package prefix notation with a backquoted value will also affect any evaluated subexpressions. The following example shows one way of getting around this, by providing the full name of the argument @('foo::x').</p>
@({
(in-package :foo) ;; assume we've defined a package named foo in Common Lisp
(defun baz-ok-1 (x)
  (acl2s-compute `(acl2::expt 2 (acl2::expt 2 ,x))))
(defun baz-bad (x)
  (acl2s-compute `acl2::(expt 2 (expt 2 ,x)))) ;; ,x here will evaluate acl2::x instead of foo::x.
(defun baz-ok-2 (x)
  (acl2s-compute `acl2::(expt 2 (expt 2 ,foo::x)))) ;; we can just override the default package to fix this
})
")

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
    (when turned-quiet-mode-on (quiet-mode-on))
    (let ((state acl2::*the-live-state*))
      (multiple-value-bind (erp val state)
			   (ld-options `((assign acl2s::acl2s-result ,c))
				       (append (remove-prop args :quiet) (when *quiet-mode-state* LD-QUIET-FLAGS)))
			   (if (equal val :eof)
			       (save-result `(list nil (@ acl2s::acl2s-result)))
			     (save-result `(list t nil))))
      (when turned-quiet-mode-on (quiet-mode-off))
      (acl2s-last-result))))

(xdoc::defxdoc-raw acl2s-compute
  :parents (acl2s-interface)
  :short "Run a single-value ACL2 computation from Common Lisp"
  :long "
<b>General form</b>
@({
(acl2s-compute
  form       ;; The form to evaluate. Should return a single value.
  :quiet ... ;; Optional. Whether or not to suppress all ACL2 printed output. Defaults to nil.
  ...)       ;; Any additional arguments will be passed to ld.
=>
(list erp val)
})
<dl>
<dt>Returns</dt>
<dd>@('erp') is @('t') if an error occurred during execution of @('form'), and is @('nil') otherwise.</dd>
<dd>@('val') is the single value that @('form') evaluated to.</dd>
</dl>

<p>
The @('form') argument should be an ACL2 expression that evaluates to a single value. Be careful about symbol packages when using @('acl2s-compute') when inside a different package - you may need to fully specify the name of an ACL2 function when calling it. See @(see acl2s-interface-symbol-package-tips) for more information.
</p>

<p>
When the @(':quiet') option is set to @('t'), @('acl2s-compute') will attempt to suppress all ACL2 printed output during evaluation of @('form'). This temporarily overrides the current @(see quiet-mode).
</p>

<h4>Examples</h4>

@({(acl2s-compute '(+ 1 2))})
Returns (nil 3)
@({(acl2s-compute '(+ 1 2) :quiet nil :ld-pre-eval-print t)})
Returns (nil 3), does not attempt to suppress any ACL2 printed output, and passes @(':ld-pre-eval-print t') to @(see ld)
@({(acl2s-compute '(append '(1 2) '(3 4)))})
Returns (nil (1 2 3 4))
@({(acl2s-compute '(+ 1 '(1 2)))})
Returns (t nil) and prints out a guard violation error message
@({(acl2s-compute '(mv nil t state))})
")

#|
General form:
@({
 (acl2s-compute form   ; The form to evaluate. Should return a single value.
                :quiet ; nil or t. If t, try to suppress as much ACL2 printed
                       ; output as possible. Default nil. Overrides current
                       ; @(see quiet-mode)
                ...)   ; any additional arguments will be passed to ld
})
|#
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

;; TODO add better error handling. In particular, the current code may return whatever the value of acl2s-result was before this call to acl2s-query
;; TODO I (Drew) changed set-prover-step-limit to with-prover-step-limit to ensure that we don't trample any existing prover-step-limit. Probably faster too. (should benchmark)
(defun acl2s-query (q &rest args &key (quiet nil) (prover-step-limit 3000000) (ld-error-action :continue) &allow-other-keys)
  (let ((turned-quiet-mode-on (and quiet (not *quiet-mode-state*))))
    (when turned-quiet-mode-on (quiet-mode-on))
    (let ((state acl2::*the-live-state*))
      (ld-options `((with-prover-step-limit
                     ,prover-step-limit
		     (mv-let
		      (erp val state)
		      ,q
		      (assign acl2s::acl2s-result (list erp val)))))
		  (append (remove-props args '(:quiet :prover-step-limit))
			  (append (unless (eq ld-error-action :continue) (list :ld-error-action ld-error-action))
				  (when *quiet-mode-state* LD-QUIET-FLAGS))))
      (when turned-quiet-mode-on (quiet-mode-off))
      (acl2s-last-result))))

(xdoc::defxdoc-raw acl2s-query
  :parents (acl2s-interface)
  :short "Run a multiple-value ACL2 computation from Common Lisp"
  :long "
<b>General form</b>
@({
(acl2s-query
  form               ;; The form to evaluate. Should return an error triple.
  :quiet ...         ;; Optional. Whether or not to suppress all ACL2 printed output. Defaults to nil.
  :prover-step-limit ;; Optional. Sets the prover step limit. Defaults to 3000000.
  :ld-error-action   ;; Optional. Defaults to :continue. See LD's doc topic for more information.
  ...)       ;; Any additional arguments will be passed to ld.
=>
(list erp val)
})
<dl>
<dt>Returns</dt>
<dd>@('erp') is the first value of the error triple that @('form') evaluated to.</dd>
<dd>@('val') is the second value of the error triple that @('form') evaluated to.</dd>
</dl>

<p>
The @('form') argument should be an ACL2 expression that evaluates to an @(see error-triple). Be careful about symbol packages when using @('acl2s-query') when inside a different package - you may need to fully specify the name of an ACL2 function when calling it. See @(see acl2s-interface-symbol-package-tips) for more information.
</p>

<p>
When the @(':quiet') option is set to @('t'), @('acl2s-query') will attempt to suppress all ACL2 printed output during evaluation of @('form'). This temporarily overrides the current @(see quiet-mode).
</p>

<p>
@('acl2s-query') evaluates @('form') inside of a @(see with-prover-step-limit), where the step-limit is set to the value provided to @(':prover-step-limit'), or 3000000 if that option is not provided. See @(see set-prover-step-limit) for more information about the prover step-limit. If you don't want to limit the number of prover steps permitted for an event, set @(':prover-step-limit') to nil.
</p>

<h4>Examples</h4>
@({(acl2s-query '(value (+ 1 2)))})
Returns (nil 3)

@({(acl2s-query '(thm (implies (natp x) (integerp x))))})
Returns (nil :invisible)

@({(acl2s-query '(mv nil t state))})
Returns (nil t)

@({(acl2s-query '(value (+ 1 2)) :quiet nil :ld-pre-eval-print t)})
Returns (nil 3), does not attempt to suppress any ACL2 printed output, and passes @(':ld-pre-eval-print t') to @(see ld)

@({(acl2s-query '(trans-eval '(+ 1 2) 'my-ctx state nil))})
Returns (nil ((nil) . 3)). See @(see trans-eval) for more information
about trans-eval's return value.

@({(acl2s-query '(test? (implies (integerp x) (natp x))))})
Returns (t nil)
")

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

;; TODO: how is this different from acl2s-query? Do we need both?
(defun acl2s-event (e &rest args &key (quiet nil) (prover-step-limit 3000000) &allow-other-keys)
  (let ((turned-quiet-mode-on (and quiet (not *quiet-mode-state*))))
    (when turned-quiet-mode-on (quiet-mode-on))
    (let ((state acl2::*the-live-state*))
      (multiple-value-bind (erp val state)
	  (ld-options `((with-prover-step-limit
                         ,prover-step-limit
			 ,e))
		      (append (remove-props args '(:quiet :prover-step-limit))
			      (when *quiet-mode-state* LD-QUIET-FLAGS)))
	(setf erp (not (equal val :eof)))
	(save-result `(list ',erp ',val))
	(when turned-quiet-mode-on (quiet-mode-off))
	(list erp val)))))

(xdoc::defxdoc-raw acl2s-event
  :parents (acl2s-interface)
  :short "Install an ACL2 event from Common Lisp"
  :long "
<b>General form</b>
@({
(acl2s-event
  form               ;; The form to evaluate. Should return an error triple.
  :quiet ...         ;; Optional. Whether or not to suppress all ACL2 printed output. Defaults to nil.
  :prover-step-limit ;; Optional. Sets the prover step limit. Defaults to 3000000.
  ...)       ;; Any additional arguments will be passed to ld.
=>
(list erp val)
})
<dl>
<dt>Returns</dt>
<dd>@('erp') is `nil` if ld indicates that the form was successfully evaluated..</dd>
<dd>@('val') is the ``reason'' that ld gives for its termination. See @(see ld), in particular the discussion of its return value, for more information. This can be safely ignored in favor of checking @('erp') in most applications.</dd>
</dl>

<p>
The @('form') argument should be an ACL2 expression that evaluates to an @(see error-triple). Be careful about symbol packages when using @('acl2s-event') when inside a different package - you may need to fully specify the name of an ACL2 function when calling it. See @(see acl2s-interface-symbol-package-tips) for more information.
</p>

<p>
When the @(':quiet') option is set to @('t'), @('acl2s-event') will attempt to suppress all ACL2 printed output during evaluation of @('form'). This temporarily overrides the current @(see quiet-mode).
</p>

<p>
@('acl2s-event') evaluates @('form') inside of a @(see with-prover-step-limit), where the step-limit is set to the value provided to @(':prover-step-limit'), or 3000000 if that option is not provided. See @(see set-prover-step-limit) for more information about the prover step-limit. If you don't want to limit the number of prover steps permitted for an event, set @(':prover-step-limit') to nil.
</p>

<h4>Examples</h4>
@({(acl2s-event '(definec f (x :int) :int (* x x)))})
Returns (nil :eof), and defines the function @('f') in the ACL2 world.

@({(acl2s-event '(definec g (x :int) :int (* 5 x)) :quiet t)})
Returns (nil :eof), and defines the function @('g') in the ACL2 world. Tries to suppresses all ACL2 printed output during this process.

@({(acl2s-event '(defthm triangle-inequality
                              (implies (and (natp x) (natp y))
                                       (>= (+ (abs x) (abs y)) (abs (+ x y)))))
              :prover-step-limit 200)})
Returns (t (:stop-ld 1)), indicating that the @('defthm') did not successfully execute. This is because the @('defthm') was limited to 200 prover steps, and more prover steps than that are required to prove it.

@({(acl2s-event '(defthm triangle-inequality
                              (implies (and (natp x) (natp y))
                                       (>= (+ (abs x) (abs y)) (abs (+ x y)))))
              :prover-step-limit 1000)})
Returns (nil :eof), and defines the theorem @('triangle-inequality') in the ACL2 world. It limits the @('defthm') call to 1000 prover steps.
")

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
