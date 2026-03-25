;;; B1 Exercises: map, factorial, fast-exp, reverse
;;; Implemented: cons, null, car, cdr, cond, define for functions

;;; ***************
;;; tags and tables
;;; ***************

(define table '())

(define (set-table tag proc)
	(set! table (cons (cons tag proc) table)))

(define (get-table tag)
	(define (get-from tag table)
		(cond ((null? table) 'unfound)
			((equal? tag (caar table)) (cdar table))
			(else (get-from tag (cdr table)))))
	(get-from tag table))

(define (set-tag tag contents)
	(cons tag contents))

(define (get-tag exp)
	(car exp))

(define (get-contents exp)
	(cdr exp))


;;; ***************
;;; evaluation
;;; ***************

(define (pre-eval exp)
	(cond ((number? exp) (list 'self-evaluating exp))
		((string? exp) (list 'self-evaluating exp))
		((symbol? exp) (list 'variable exp))
		(else exp)))

(define (new-eval exp env)
	(if (equal? (get-table (get-tag (pre-eval exp))) 'unfound)
		(new-eval (cons 'application exp) env)
		((get-table (get-tag (pre-eval exp))) (pre-eval exp) env)))

(define (eval-sequence exps env)
	(cond ((null? exps) 'unspecified-return-value)
		((null? (cdr exps)) (new-eval (car exps) env))
		(else (begin (new-eval (car exps) env) (eval-sequence (cdr exps) env)))))

(define (install-eval-package)

	(set-table 'self-evaluating (lambda (exp env) (cadr exp)))
	(set-table 'variable (lambda (exp env) (lookup-var-value (cadr exp) env)))
	(set-table 'define (lambda (exp env) 
		(if (pair? (cadr exp))
			(define-var! (caadr exp) (make-procedure (cdadr exp) (cddr exp) env) env)
			(define-var! (cadr exp) (new-eval (caddr exp) env) env))))
	(set-table 'set! (lambda (exp env) (set-var-value! (cadr exp) (new-eval (caddr exp) env) env)))
	(set-table 'quote (lambda (exp env) (cadr exp)))

	(define (eval-begin exp env)
		(cond ((equal? (car exp) 'begin) (eval-begin (cdr exp) env))
			(else (eval-sequence exp env))))
	(set-table 'begin eval-begin)

	(define (lambda-vars exp)
		(cadr exp))
	(define (lambda-body exp)
		(cddr exp))
	(define (eval-lambda exp env)
		(make-procedure (lambda-vars exp) (lambda-body exp) env))
	(set-table 'lambda eval-lambda)

	(define (application-proc exp)
		(cadr exp))
	(define (application-args exp)
		(cddr exp))
	(define (eval-application exp env)
		(new-apply (new-eval (application-proc exp) env)
			(map (lambda (x) (new-eval x env)) (application-args exp))))
	(set-table 'application eval-application)

	(define (predicate exp)
		(cadr exp))
	(define (consequent exp)
		(caddr exp))
	(define (has-alternate? exp)
		(if (null? (cdddr exp))
			False
			True))
	(define (alternate exp)
		(cadddr exp))
	(define (true? p)
		(if (eq? p 'true-object)
			#t
			#f))
	(define (false? p)
		(if (eq? p 'false-object)
			True
			False))
	(define (eval-if exp env)
		(if (true? (new-eval (predicate exp) env))
			(new-eval (consequent exp) env)
			(if (has-alternate? exp)
				(new-eval (alternate exp) env)
				'unspecified-return-value)))

	(set-table 'if eval-if)
	(define (new-cond exp env)
		(cond ((null? (cdr exp)) 'unspecified-return-value)
				((eq? (caadr exp) 'else) (eval-sequence (cdadr exp) env))
				((true? (new-eval (caadr exp) env)) (eval-sequence (cdadr exp) env))
				(else (new-cond (cons 'cond (cddr exp)) env))))
	(set-table 'cond new-cond)

	(define (let-to-lambda exp env)
		(define (constructor vars vals)
			(new-eval (cons (list 'lambda vars (cons 'begin (cddr exp))) vals) env))
		(constructor (map (lambda (x) (car x)) (cadr exp))
					(map (lambda (x) (cadr x)) (cadr exp))))
	(set-table 'let let-to-lambda)
	)

;;; ***************
;;; procedures & apply
;;; ***************

(define (make-procedure params body env)
	(list 'procedure params body env))

(define (procedure-vars proc)
	(cadr proc))

(define (procedure-body proc)
	(caddr proc))

(define (procedure-env proc)
	(cadddr proc))

(define (new-apply proc values)
	(if (primitive? proc)
		(apply (primitive-procedure-code proc) values)
	(eval-sequence (procedure-body proc) (extend-environment (procedure-vars proc) values (procedure-env proc))))
	)

(define (make-primitive-procedure code)
	(list 'primitive code))

(define (primitive-procedure-code proc)
	(cadr proc))

(define (primitive? proc)
	(if (equal? (car proc) 'primitive)
		#t
		#f))
;;; ***************
;;; frames & environments
;;; ***************

(define (make-empty-environment) (list (cons '() '())))

(define (top-frame env) (car env))

(define (enclosing-env env) (cdr env))

(define (frame-vars frame) (car frame))

(define (frame-values frame) (cdr frame))

(define (lookup-var-value var env)
	(define (find-in-frame var frame)
		(cond ((null? (car frame)) #f)
			((equal? (caar frame) var) (cadr frame))
			(else (find-in-frame var (cons (cdr (frame-vars frame)) (cdr (frame-values frame)))))))
	(cond ((null? env) error)
		((not (find-in-frame var (top-frame env))) (lookup-var-value var (enclosing-env env)))
		(else (find-in-frame var (top-frame env)))))


(define (define-var! var value env)
	(define (frame-del var vars-searched vals-searched vars-left vals-left)
		(cond ((null? vars-left) (set-car! env (cons (cons var vars-searched) (cons value vals-searched))) var)
			((equal? var (car vars-left)) (set-car! env (cons (append (list var) vars-searched (cdr vars-left)) (append (list value) vals-searched (cdr vals-left)))) var)
			(else (frame-del var (cons (car vars-left) vars-searched) (cons (car vals-left) vals-searched) (cdr vars-left) (cdr vals-left)))))
	(frame-del var '() '() (frame-vars (top-frame env)) (frame-values (top-frame env)))
	)

(define (set-var-value! var value env)
	(define (set-in-frame var value vars-searched vals-searched vars-left vals-left)
		(cond ((null? vals-left) (set-var-value! var value (cdr env)))
			((equal? var (car vars-left)) (begin (set-car! env (cons (append vars-searched (list var) (cdr vars-left)) (append vals-searched (list value) (cdr vals-left)))) (car vals-left)))
			(else (set-in-frame var value (cons (car vars-left) vars-searched) (cons (car vals-left) vals-searched) (cdr vars-left) (cdr vals-left)))))
	(if (null? env)
		error
		(set-in-frame var value '() '() (frame-vars (top-frame env)) (frame-values (top-frame env)))))

(define (extend-environment vars args base-env)
	(define new-env (cons (cons vars args) base-env))
	new-env
)

;;; ***********************
;;; printing
;;; ***********************

(define (sanitize-output exp)
    (if (and (pair? exp) (eq? 'procedure (get-tag exp)))
        (list 'procedure (procedure-vars exp) (procedure-body exp) 'enclosing-env)
        exp))

(define (print-environment env)
    (define (print-frame vars values)
        (if (null? vars)
            (begin (newline) (display "--------"))
            (begin (newline) (display (car vars)) (display ": ") (display (sanitize-output (car values)))
                (print-frame (cdr vars) (cdr values)))))
    (if (null? env)
        'done
        (begin
            (print-frame (car (top-frame env)) (cdr (top-frame env)))
            (print-environment (enclosing-env env)))))

;;; ***************
;;; setup
;;; ***************

(define (setup-global-environment)
	(define primitives '())
	(define (add-primitive name code)
		(set! primitives (cons (cons name (make-primitive-procedure code)) primitives)))
	(define (add-unary-primitive-predicate name code)
		(add-primitive name (lambda (var) (if (false? (code var))
												'false-object
												'true-object))))
	(define (add-primitive-predicate name code)
		(add-primitive name (lambda (var1 var2) (if (false? (code var1 var2))
													'false-object
													'true-object))))
	(define (new-null exp)
		(if (null? exp)
			#t
			#f))
	(define (make-null)
		'())
	(add-primitive '+ +)
	(add-primitive '- -)
	(add-primitive '* *)
	(add-primitive '/ /)
	(add-primitive-predicate '= =)
	(add-unary-primitive-predicate 'even? even?)
	(add-unary-primitive-predicate 'null? null?)
	(add-primitive 'make-null make-null)
	(add-primitive 'cons cons)
	(add-primitive 'car car)
	(add-primitive 'cdr cdr)
	(add-primitive 'list list)
	(define global (make-empty-environment))
	(define (bind-primitives env)
		(define (bind-list prim-list)
			(if (null? (cdr prim-list))
				(define-var! (caar prim-list) (cdar prim-list) env)
				(begin (define-var! (caar prim-list) (cdar prim-list) env) (bind-list (cdr prim-list)))))
		(bind-list primitives))
	(bind-primitives global)
	global
	)
(define global (setup-global-environment))
(install-eval-package)

;;; ***************
;;; testing: jason
;;; ***************


;;; testing frame and environment API
(define frame1 (cons '(a b c) '(1 2 3)))
(define frame2 (cons '(d e c) '(4 5 6)))
(define globalframe (cons '(x y z) '(10 20 30)))
(define test-environment (list frame2 frame1 globalframe))

(assert (= 6 (lookup-var-value 'c test-environment))) ; test lookup in first frame
(assert (= 10 (lookup-var-value 'x test-environment))) ; test lookup in later frame

(set-var-value! 'a 100 test-environment) ; change var in later frame and test
(assert (= 100 (lookup-var-value 'a test-environment)))

(define-var! 'b 17 test-environment) ; define var in first frame that's already bound in a later frame
(assert (= 17 (lookup-var-value 'b test-environment)))
(assert (= 2 (lookup-var-value 'b (cdr test-environment))))


;;; test factorial
(new-eval '(define fact (lambda (n) (if (= n 1) 1 (* n (fact (- n 1)))))) global)
(assert (= 720 (new-eval '(fact 6) global))) ; test factorial




;;; test new-define
(new-eval '(define x 13) global)
(assert (= 4 (new-eval 4 global)))
(assert (= 13 (new-eval 'x global)))

;;; test cond
(new-eval '(define g (lambda (x) (cond ((= x 0) 0) ((= x 1) 2 1) ((= x 10) 10) (else -1)))) global)
(assert (= 0 (new-eval '(g 0) global)))
(assert (= 10 (new-eval '(g 10) global)))
(assert (= 1 (new-eval '(g 1) global)))
(assert (= -1 (new-eval '(g 2) global)))
(new-eval '(define g-1 (lambda (x) (cond ((= x 0) 0) ((= x 1) 2 1) ((= x 10) 10) ))) global)
;; test cond with no else clause
(assert (= 10 (new-eval '(g-1 10) global)))
(new-eval '(g-1 20) global) ; should return 'unspecified-return-value.

;;; test let
(new-eval '(define h (lambda (x) (let ((y (+ x x)) (z (* x x))) (+ y z)))) global)
(assert (= 120 (new-eval '(h 10) global)))

;;; test map
(new-eval
    '(define my-map (lambda (proc l) 
                    (if (null? l)
                        (make-null) 
                        (cons (proc (car l)) (my-map proc (cdr l)))))) global)
(new-eval '(my-map (lambda (n) (+ 1 n)) (list 1 2 3)) global) ;;; should be the list (2 3 4)

;;; ***************
;;; testing: mira
;;; ***************
(print-environment global)
(new-eval '(+ 2 3) global)
(new-eval '(= 2 3) global)
(new-eval '(even? 4) global)
(new-eval '(define (factorial n) (if (= 1 n)
										1
										(* n (factorial (- n 1))))) global)
(new-eval '(factorial 6) global)
(new-eval '(make-null) global)
(new-eval '(null? (make-null)) global)
(new-eval '(cond ('true-object 5) ((= 1 2) 5) (else 1)) global)
(new-eval '(let ((a 1) (b 2)) (+ a b) (- a b)) global)
