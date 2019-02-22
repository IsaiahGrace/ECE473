;;; (map-reduce (formula-eval phi (truth-assign (get-vars phi))))
;;; Break down input string into set of propositions
;;; construct a truth binding for the propositions (truth table)
;;; map-reduce the truth table with formula-eval
;;; construct the truth table

;;; We are building a satisfiable function
;;; We will return a list of rows of truth assignments
;;;    that cause the entire expression phi to be true
;;; We must recursively break down AND and OR statements
;;; There will be 2^n possible truth assignments for a phi with n propositions
;;; We are only keeping track of truth-ness, maybe there is some way to limit
;;; recursion when
;;; we know that there is no way to satisfy the sub-string
(define (test)
 (test2 '(AND (AND #t (OR P3 P2 P3 P1) (OR #F (NOT (OR P2 P4)) P4)))))
					;'(AND P1 P2 P3 P4)))
					;'(OR P1 P2)))

(define (test2 phi)
 (write
  (map (lambda (row)
	(list row (evaluate? phi row)))
       (truth-assign '() (set-create (get-vars phi)))))
 (newline))

(define (truth-table phi)
 (map (lambda (row)
       (list row (evaluate? phi row)))
      (truth-assign '() (set-create (get-vars phi)))))

;;; The evaluate function takes a string phi in the propositional logic language
;;;     and a complete, non-redundant, consistent truth assignment i, and
;;;     evaluates the string, returning either #t or #f
(define (evaluate? phi row)
 (cond ((symbol? phi) (lookup? phi row))
       ((boolean? phi) phi)
       ((eq? (first phi) 'AND)
	(reduce (lambda (x y) (and x y))
		(map (lambda (x) (evaluate? x row)) (rest phi)) #t))
       ((eq? (first phi) 'OR)
	(reduce (lambda (x y) (or x y))
		(map (lambda (x) (evaluate? x row)) (rest phi)) #f))
       ((eq? (first phi) 'NOT) (not (evaluate? (second phi) row)))
       (else (lookup? phi row))))

;;; returns the truth value assigned to proposition prop by truth assignment i
(define (lookup? prop row)
 (cond ((null? row) ((write prop) (write row) (panic "prop not found in row")))
       ((eq? prop (first (first row))) (second (first row)))
       (else (lookup? prop (rest row)))))

;;; truth-assign will be given a set of n propositions and will return a list
;;;   of 2^n rows of a truth table in the form below
(define (truth-assign row props)
 (cond ((null? props) (list row))
       ((null? row)
	(append
	 (truth-assign
	  (list (list (first props) #f)) (rest props))
	 (truth-assign
	  (list (list (first props) #t)) (rest props))))
       (else
	(append
	 (truth-assign
	  (append row (list (list (first props) #f))) (rest props))
	 (truth-assign
	  (append row (list (list (first props) #t))) (rest props))))))

;;; extracts a list (with repetitions) of the variables in the formula phi
(define (get-vars phi)
 (cond ((null? phi) phi)
       ((boolean? phi) '())
       ((symbol? phi) (list phi))
       ((boolean? (first phi)) (get-vars (rest phi)))
       ((list? (first phi))
	(append (get-vars (first phi)) (get-vars (rest phi))))
       ((eq? (first phi) 'AND) (get-vars (rest phi)))
       ((eq? (first phi) 'OR) (get-vars (rest phi)))
       ((eq? (first phi) 'NOT) (get-vars (rest phi)))
       ((= (length phi) 1) phi)
       (else (cons (first phi) (get-vars (rest phi))))))

(define (set-member? e s)
 (cond ((null? s) #f)
       ((null? e) #f)
       ((eq? (first s) e) #t)
       (else (set-member? e (rest s)))))

(define (set-create s)
 (cond ((null? s) s)
       ((set-member? (first s) (rest s)) (set-create (rest s)))
       (else (cons (first s) (set-create (rest s))))))
