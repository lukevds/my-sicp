; -*- lexical-binding: t -*-
; `outline-minor-mode' recommended


;;; important information
(defconst mylisp-obarray (obarray-make)
  "obarray to hold special values for my lisp.
Examples are the symbols for the values true and false")

(defconst mylisp-true (intern "true" mylisp-obarray)
  "Symbol to hold the true value in `mylisp-obarray'.")

(defconst mylisp-false (intern "false" mylisp-obarray)
  "Symbol to hold the false value in `mylisp-obarray'.")

;;; support
(defun tagged-value? (value tag)
  (and (consp value)
       (eq (car value) tag)))

(defun make-ok (value)
  (cons 'ok value))

(defun ok? (value)
  (tagged-value? value 'ok))

;; values as errors babeee
(defun make-error (value)
  (cons 'error value))

(defun error? (value)
  (tagged-value? value 'error))

;;; env
; is it working? if a change an assignment in a frame, will children
; frames see it?
(defun make-env (bindings parent)
  (cons bindings parent))

(defun env-bindings (env)
  (car env))

(defun env-parent (env)
  (cdr env))

(defun lookup-value (name env)
  "Looks for value in env, walking up ancestor tree.
If a value is found, returns a ok-value whose car is `name' and cdr
is the value of `name'. Otherwise, returns an error-value whose car is
`name' and cdr is the string \"not found\"."
  (let ((current-env env)
	(values (env-bindings env))
	(value) (result 'error))
    ;; bindings in env could be nil, so sets values and env to first
    ;; non nil encountered
    (unless values
      (while (and (null values) current-env)
	(setq current-env (env-parent current-env)
	      values (env-bindings current-env))))
    (while values
      (if (eq (caar values) name)
	  (setq value (car values)
		result 'ok
		values nil)
        (if (cdr values)
	    (setq values (cdr values))
	  (setq current-env (env-parent current-env)
		values (env-bindings current-env)))))
    (cons result (or value (cons name "not found")))))

(defun lookup-value-no-ancestors (name env)
  (lookup-value name (make-env (env-bindings env) nil)))

(defun define-value (name value env)
  "Creates a binding in `env' if it does not exist or update it.
Only consider `env's bindings, does not access parents."
  (let* ((already-exists (lookup-value-no-ancestors name env)))
    (if (ok? already-exists)
	(setcdr (cdr already-exists) value)
      (setcar env (cons (cons name value) (car env))))))

(defun assign-value (name value env)
  "If `name' is bound in `env', sets its value to `value'.
Otherwise, returns an error."
  (let ((already-exists (lookup-value name env)))
    (if (ok? already-exists)
	(progn
	  (setcdr (cdr already-exists) value)
	  (make-ok (cons name value)))
      (make-error (cons name "is undefined, therefore a value cannot be assigned to it")))))

(defun remove-binding (name env)
  "Removes a binding with `name' in `env'."
  (let ((bindings (env-bindings env)))
    (if (eq (caar bindings) name)
	(let ((oldcadr (cadr bindings))
	      (oldcddr (cddr bindings)))
	  (if (and (eq oldcadr nil) (eq oldcddr nil))
	      (setcar env nil)
	    (setcar bindings oldcadr)
	    (setcdr bindings oldcddr)))
      (while bindings
					;(car (car (cdr bindings)))
	(if (eq (caadr bindings) name)
	    (progn (setcdr bindings (cddr bindings))
		   (setq bindings nil))
	  (setq bindings (cdr bindings)))))))


(setq teste-env (make-env nil nil))
(define-value 'oi 12 teste-env)
teste-env
(env-bindings teste-env)
(lookup-value 'oi teste-env)
(setq outro-env (make-env nil teste-env))
(define-value 'asdf 5040 outro-env)
outro-env
(lookup-value 'asdf outro-env)
(lookup-value 'oi outro-env)
(lookup-value 'nonsense outro-env)
(assign-value 'asdf 123 outro-env)
(assign-value 'xyz 123 outro-env)
(lookup-value 'asdf outro-env)
(define-value 'xyz 9090 outro-env)
(define-value 'aaa 111 outro-env)
(remove-assign 'asdf outro-env)
(remove-assign 'xyz outro-env)
(remove-assign 'aaa outro-env)
(remove-assign 'oi outro-env)

;;; procedures
(defun lambda-parameters (exp)
  (cadr exp))

(defun lambda-body (exp)
  (cddr exp))

(defun make-procedure (parameters body env)
  (let ((procedure-env (make-env nil env)))
    (cons
     (list parameters body procedure-env)
     env)))

;;; eval
(defun self-evaluating? (exp)
  (or
   (numberp exp)
   (stringp exp)))

(defun variable? (exp)
  (symbolp exp))

(defun eval-lookup (name env)
  (let ((lookup-result (lookup-value name env)))
    (make-ok
     (cons
      (when (ok? lookup-result)
	(cddr lookup-result))
      env))))

(defun quoted? (exp)
  (tagged-value? exp 'quote))

(defun text-of-quotation (exp env)
  (make-ok (cons (cadr exp) env)))

(defun assignment? (exp)
  (and (tagged-value? exp 'myset)
       (variable? (cadr exp))))

(defun eval-assignment (exp env)
  (let* ((name (cadr exp))
	 (evaluation-result (myeval (caddr exp) env))
	 (evaluation-value (cadr evaluation-result)))
    (if (ok? evaluation-result)
	(let ((assignment-result (assign-value name evaluation-value env)))
	  (if (ok? assignment-result)
	      ; (cddr (ok name . value)) => value
	      (make-ok (cons (cddr assignment-result) env))
	    ; if this happens, we should undo any mutations done by
	    ; `evaluation-result'
	    ; how to achieve this:
	    ; - remove existence check in `assign-value'
	    ; - make existence check before evaluating `(caddr exp)'
	    (make-error (cons name "could not be assigned a value"))))
      (make-error (cons (caddr exp) "could not be evaluated")))))

(defun definition? (exp)
  (and (tagged-value? exp 'mydefine)
       (variable? (cadr exp))))

(defun eval-definition (exp env)
  (let* ((name (cadr exp))
	 (evaluation-result (myeval (caddr exp) env))
	 (value (car evaluation-result))
	 (resulting-env (cdr evaluation-result)))
    (define-value name value resulting-env)
    (cons value resulting-env)))

(defun if? (exp)
  (tagged-value? exp 'if))

(defun eval-if (exp env)
  (let* ((condition-result (myeval (cadr exp) env))
	 (condition-value (car condition-result))
	 (cond-val-is-true
	  (and (ok? condition-value)
	       (eq (cddr condition-value) mylisp-true))))
    (myeval
     (if cond-val-is-true
	 (caddr exp)
       (cadddr exp))
     (cdr condition-result))))

(defun lambda? (exp)
  (tagged-value? exp 'lambda))

(defun begin? (exp)
  (tagged-value? exp 'begin))

(defun cond? (exp)
  (tagged-value? exp 'cond))

(defun application? (exp)
  (and (consp exp)
       (variable? (car exp))))

;; fazer retornar (valor . env-talvez-modificado)
(defun myeval (exp env)
  (cond ((self-evaluating? exp) (make-ok (cons exp env)))
        ((variable? exp) (eval-lookup exp env))
        ((quoted? exp) (text-of-quotation exp env))
        ((assignment? exp) (eval-assignment exp env)) ;; stopped here

        ((definition? exp) (eval-definition exp env))

        ((if? exp) (eval-if exp env))

        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (myeval (cond->if exp) env))
        ((application? exp)
         (myapply (myeval (operator exp) env)
                (list-of-values (operands exp) env)))
        (:else
         (error "Unknown expression type -- EVAL" exp))))

(myeval 13 outro-env)

(myeval "abcd" outro-env)

(myeval 'oi outro-env)
(myeval 'teste outro-env)
(myeval '(quote (1 2 3)) outro-env)

(myeval '(myset asdf 13) teste-env)
(myeval '(myset oi 18) teste-env)
(myeval '(myset oi 12) teste-env)
(myeval '(mydefine olar 2020) teste-env)
(myeval '(myset olar 400) teste-env)
(myeval '(quote shrek) teste-env)

(myeval 'false teste-env)

(setq teste-resultado (myeval 'false teste-env))

(myeval '(if 'false 1 2) teste-env)

(define-value 'true mylisp-true teste-env)

(myeval '(lambda () 123) teste-env)


;;; apply
(defun myapply (procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (:else
         (error
	  "Unknown procedure type -- APPLY" procedure))))

(defun list-of-values (exps env)
  (if (no-operands? exps)
      '()
    (cons (myeval (first-operand exps) env)
          (list-of-values (rest-operands exps) env))))

(defun eval-if (exp env)
  (if (true? (myeval (if-predicate exp) env))
      (myeval (if-consequent exp) env)
    (myeval (if-alternative exp) env)))

(defun eval-sequence (exps env)
  (cond ((last-exp? exps) (myeval (first-exp exps) env))
        (:else (myeval (first-exp exps) env)
               (eval-sequence (rest-exps exps) env))))

(defun eval-assignment (exp env)
  (set-variable-value! (assignment-variable exp)
                       (myeval (assignment-value exp) env)
                       env)
  'ok)

(defun eval-definition (exp env)
  (define-variable! (definition-variable exp)
                    (myeval (definition-value exp) env)
                    env)
  'ok)
