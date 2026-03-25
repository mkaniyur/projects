;;; ***************
;;; objects
;;; ***************

;;; register
(define maxdepth 0)
(define depth 0)
(define (make-reg)
	(let ((contents 'undefined))
	(define (set-contents val)
		(set! contents val))
	(define (get-contents)
		contents)
	(define (dispatch m)
		(cond ((eq? m 'set-contents) set-contents)
			((eq? m 'get-contents) get-contents)))
	dispatch))

;;; stack

(define (make-stack)
	(let ((contents '()))
		(define (empty?)
			(if (null? contents)
				#t
				#f))
		(define (push val)
			(set! depth (+ 1 depth))
			(if (> depth maxdepth)
				(set! maxdepth depth))
			(set! contents (cons val contents)))
		(define (pop reg)
			(set! depth (- depth 1))
			(define temp (car contents))
			(set! contents (cdr contents))
			((reg 'set-contents) temp))
		(define (dispatch m)
			(cond ((eq? m 'empty?) empty?)
				((eq? m 'push) push)
				((eq? m 'pop) pop)))
		dispatch))

;;; ***************
;;; tags and tables
;;; ***************

;;; code table: stores code at point in program
(define c-table '())

;;; function table: assign, push, pop
(define f-table '())
(define (set-f-table tag proc)
	(set! f-table (cons (cons tag proc) f-table)))
(define (get-f-table tag)
	(define (get-from tag f-table)
		(cond ((null? f-table) 'unfound)
			((equal? tag (caar f-table)) (cdar f-table))
			(else (get-from tag (cdr f-table)))))
	(get-from tag f-table))

;;; label table
(define l-table '())
(define (set-l-table tag proc)
	(set! l-table (cons (cons tag proc) l-table)))
(define (get-l-table tag)
	(define (get-from tag l-table)
		(cond ((null? l-table) 'unfound)
			((equal? tag (caar l-table)) (cdar l-table))
			(else (get-from tag (cdr l-table)))))
	(get-from tag l-table))

;;; eval table: reg, const, op
(define e-table '())
(define (set-e-table tag proc)
	(set! e-table (cons (cons tag proc) e-table)))
(define (get-e-table tag)
	(define (get-from tag e-table)
		(cond ((null? e-table) 'unfound)
			((equal? tag (caar e-table)) (cdar e-table))
			(else (get-from tag (cdr e-table)))))
	(get-from tag e-table))

;;; op table
(define o-table '())
(define (set-o-table tag proc)
	(set! o-table (cons (cons tag proc) o-table)))
(define (get-o-table tag)
	(define (get-from tag o-table)
		(cond ((null? o-table) 'unfound)
			((equal? tag (caar o-table)) (cdar o-table))
			(else (get-from tag (cdr o-table)))))
	(get-from tag o-table))

;;; register table
(define r-table '())
(define (set-r-table tag proc)
	(set! r-table (cons (cons tag proc) r-table)))
(define (get-r-table tag)
	(define (get-from tag r-table)
		(cond ((null? r-table) 'unfound)
			((equal? tag (caar r-table)) (cdar r-table))
			(else (get-from tag (cdr r-table)))))
	(get-from tag r-table))

;;; ***************
;;; procedures & apply
;;; ***************

(define (new-apply line)
	((get-f-table (car line)) (cdr line)))

;;; procedures: assign, push, pop, goto, branch, test
(define (install-apply-package)

	(define (assign line)
		(((get-r-table (cadar line)) 'set-contents) (new-eval (cdr line))))
	(set-f-table 'assign assign)

	(define (push line)
		((stack 'push) (new-eval line)))
	(set-f-table 'push push)

	(define (pop line)
		((stack 'pop) (get-r-table (cadar line))))
	(set-f-table 'pop pop)

	(define (goto line)
		(set! c-table (new-eval line)))
	(set-f-table 'goto goto)

	(define (branch line)
		(set! c-table (new-eval line)))
	(set-f-table 'branch branch)

	(define (test line)
		(if (new-eval line)
			continue
			(set! c-table (cdr c-table))
			))
	(set-f-table 'test test)
	
	)

(install-apply-package)

;;; ***************
;;; evaluation
;;; ***************

;;; evaluation: op, label, reg, const
(define (install-eval-package)
	(set-e-table 'const (lambda (num) num))
	(set-e-table 'reg (lambda (a) (((get-r-table a) 'get-contents))))
	(set-e-table 'label (lambda (labelname) (get-l-table labelname)))
	(set-e-table 'op (lambda (opname) (get-o-table opname))))

(install-eval-package)

(define (new-eval line)
	(define prevaled-line (map (lambda (item) ((get-e-table (car item)) (cadr item))) line))
	(if (equal? (caar line) 'op)
		(apply (car prevaled-line) (cdr prevaled-line))
		(car prevaled-line)))

;;; ***************
;;; set-up
;;; ***************

(define (label-tabler code)
	(cond ((null? code) continue)
		((pair? (car code)) (label-tabler (cdr code)))
		(else (begin (set-l-table (car code) code) (label-tabler (cdr code))))))

(define (register-tabler reg-list)
	(if (null? reg-list)
		continue
		(begin (set-r-table (car reg-list) (make-reg)) (register-tabler (cdr reg-list)))))

(define (op-tabler op-list)
	(if (null? op-list)
		continue
		(begin (set-o-table (caar op-list) (cdar op-list))
			(op-tabler (cdr op-list)))))

;;; ***************
;;; main APIs
;;; ***************

(define (make-machine reg-names op-list instructions)
	(register-tabler reg-names)
	(op-tabler op-list)
	(label-tabler instructions)
	(set! c-table instructions)
	instructions)

(define stack (make-stack))

(define (run code)
	(define (iter)
		(cond ((null? c-table) 'done)
			((pair? (car c-table)) (begin 
				(new-apply (car c-table)) (set! c-table (cdr c-table)) (iter)))
			(else (begin (set! c-table (cdr c-table)) (iter)))))
	(iter))

(define (run-with-monitoring code)
	(define (iter)
		(cond ((null? c-table) 'done)
			((pair? (car c-table)) (begin (display (car c-table))
				(new-apply (car c-table)) (newline) (set! c-table (cdr c-table)) (iter)))
			(else (begin (set! c-table (cdr c-table)) (iter)))))
	(iter)
	(display 'maxdepth:)
	(display maxdepth))

(define (set-register-contents machine regname val)
	(((get-r-table regname) 'set-contents) val))

(define (get-register-contents machine regname)
	(((get-r-table regname) 'get-contents)))

;;; ***************
;;; testing
;;; ***************

;;; mira testing
(define tester-code (make-machine '(r b a x) (list (cons '+ +)
				 (cons '- -)
				 (cons '/ /)
				 (cons '* *)
				 (cons '= =))
			'(start-label
			(assign (reg r) (op +) (const 1) (reg r))
			(test (op =) (const 5) (const 4))
			(goto (label done-label))
			(assign (reg x) (label done-label))
			(goto (reg x))
			(assign (reg r) (const 100))
			done-label
			(push (const 0))
			(push (const 1))
			(push (const 1))
			(push (const 1))
			(push (const 1))
			(pop (reg a))
			(assign (reg r) (op +) (const 1) (reg r)))))

(set-register-contents tester-code 'r 200)

(run tester-code)

(get-register-contents tester-code 'a)
(get-register-contents tester-code 'r)

;;; jason testing
(define gcd-machine (make-machine '(a b t c d)
	(list (cons 'rem modulo) (cons '= =))
	'((assign (reg c) (label done-label))
		(assign (reg d) (label test-label))
		test-label
		(test (op =) (reg b) (const 0))
		(branch (reg c))
		(assign (reg t) (op rem) (reg a) (reg b))
		(assign (reg a) (reg b))
		(assign (reg b) (reg t))
		(branch (reg d))
		done-label)))

(set-register-contents gcd-machine 'a 511)
(set-register-contents gcd-machine 'b 371)

(run-with-monitoring gcd-machine)

(get-register-contents gcd-machine 'a)