;;; Problem set 3: Symbolic manipulation

;;; Impliment the following re-write rules:
;;;                                    (not #t) -> #f                                              (good)
;;;                                    (not #f) -> #t                                              (good)
;;;                                (not (notΦ)) -> Φ                                               (good)

;;;                                       (and) -> #t                                              (good)
;;;                                     (and Φ) -> Φ
;;;                  (and Φ1...Φm #t Φm+1...Φn) -> (andΦ1...Φm Φm+1...Φn)
;;;                  (and Φ1...Φm #f Φm+1...Φn) -> #f
;;;     (and Φ1...Φl (and Φl+1...Φm) Φm+1...Φn) -> (andΦ1...Φl Φl+1...Φm Φm+1...Φn)
;;;       (and Φ1...Φl Φ Φl+1...Φm Φ Φm+1...Φn) -> (and Φ1...Φl Φ Φl+1...Φm Φm+1...Φn)
;;; (and Φ1...Φl (not Φ) Φl+1...Φm Φ Φm+1...Φn) -> #f
;;; (and Φ1...Φl Φ Φl+1...Φm (not Φ) Φm+1...Φn) -> #f

;;;                                        (or) -> #f
;;;                                      (or Φ) -> Φ
;;;                   (or Φ1...Φm #f Φm+1...Φn) -> (or Φ1...Φm Φm+1...Φn)
;;;                   (or Φ1...Φm #t Φm+1...Φn) -> #t
;;;       (or Φ1...Φl (or Φl+1...Φm) Φm+1...Φn) -> (or Φ1...Φl Φl+1...Φm Φm+1...Φn)
;;;        (or Φ1...Φl Φ Φl+1...Φm Φ Φm+1...Φn) -> (or Φ1...Φl Φ Φl+1...Φm Φm+1...Φn)
;;;  (or Φ1...Φl (not Φ) Φl+1...Φm Φ Φm+1...Φn) -> #t
;;;   (or Φ1...Φl Φ Φl+1...Φm (notΦ) Φm+1...Φn) -> #t


;;; boolean-simplify Φ
;;; Φ is a formula. Repeatedly applies the above rewrite rules to Φ and its
;;;     subformulas until no more rules are applicable and then returns the
;;;     resulting formula
(define (boolean-simplify phi)
 (display "boolean-simplify, phi= ") (write phi) (newline)
 (cond ((null? phi) '())
       ((symbol? phi) phi)
       ((and (or (eq? (first phi) 'AND)
		 (eq? (first phi) 'OR)
		 (eq? (first phi) 'NOT))     
	     (truth-table-tautology? (truth-table phi))) #t)
       ((and (or (eq? (first phi) 'AND)
		 (eq? (first phi) 'OR)
		 (eq? (first phi) 'NOT))
	     (truth-table-contradiction? (truth-table phi))) #f)
       ((= (length phi) 1) (boolean-simplify (first phi)))
       ((eq? (first phi) 'AND)
	(append (list (first phi))
		(boolean-simplify
		 (if (symbol? (rest phi)) (rest phi) (set-create (expand-and (rest phi)))))))
       ((eq? (first phi) 'OR)
	(append (list (first phi))
		(boolean-simplify
		 (if (symbol? (rest phi)) (rest phi) (set-create (expand-or (rest phi)))))))
       ((eq? (first phi) 'NOT)
	(if (and (list? (second phi)) (eq? (first (second phi)) 'NOT))
	    (boolean-simplify (second (second phi)))
	    (cons (first phi (boolean-simplify (rest phi))))))
       ((list? phi) (list (first phi) (boolean-simplify (rest phi))))
       (else (panic "need more inductive cases"))))

(define (test-boolean-simplify)
 ;; PASSING
 ;; '(NOT #t)
 ;; '(NOT #f)
 ;; '(NOT (NOT (AND P1 P2)))
 ;; '(NOT (NOT P1))
 ;; '(and)
 ;; FAILING

 ;; '(OR P1 P2 P3 (AND P4 P5 P2 P4) P3 P1 (OR P3 P4) (AND P2 (AND P3 (NOT P4))))))
 (let ((phi '(AND P1)))
  (print-truth-table (truth-table phi))
  (boolean-simplify phi)))

;;; Takes a list of propositions and if one of them is a list with first element AND, expands it out
(define (expand-and phi)
					;(display "expand-and, phi= ") (write phi) (newline)
 (cond ((null? phi) '())
       ((symbol? phi) phi)
       ((and (list? (first phi)) (eq? (first (first phi)) 'AND))
	(expand-and (append (rest (first phi)) (rest phi))))
       (else (cons (first phi) (expand-and (rest phi))))))

(define (test-expand-and)
 ;;'(P1 P2 P3 (AND P4 P5 P2 P4) P3 P1 (OR P3 P4) (NOT P1) (AND P2 (AND P3 P4)))))
 (expand-and '((AND P2 (AND P3 (NOT P4))))))

;;; Takes a list of propositions and if one of them is a list with first element OR, expands it out
(define (expand-or phi)
					;(display "expand-or, phi= ") (write phi) (newline)
 (cond ((null? phi) '())
       ((symbol? phi) phi)
       ((and (list? (first phi)) (eq? (first (first phi)) 'OR))
	(expand-or (append (rest (first phi)) (rest phi))))
       (else (cons (first phi) (expand-or (rest phi))))))

(define (test-expand-or)
 (expand-or '(P1 P2 P3 (OR P4 P5 P2 P4) P3 P1 (AND P3 P4) (NOT P1) (OR P2 (OR P3 P4)))))


;;; truth-tables-match? Φ Φ'
;;; Φ and Φ' are formulas. Φ' will be the result of (boolean-simplify Φ).
;;;     Compares the truth tables of Φ and Φ' and returns #t if they are the
;;;     same and #f if they are different.
(define (truth-tables-match? phi phi-simple)
					;(display "truth-tables-match?") (newline)
					;(display "phi= ") (write phi) (display " phi-simple= ") (write phi-simple) (newline)
 (cond ((truth-table-tautology? (truth-table phi))
	(if (eq? phi-simple #t) #t #f)) ; phi is a tautology, so phi-simple should just be #t
       ((truth-table-contradiction? (truth-table phi)) 
	(if (eq? phi-simple #f) #t #f)) ; phi is a contradiction, so phi-simple should just be #f
       ;; phi is not a tautology nor a contradiction, phi-simple not just a boolean
       (else (truth-tables-equivalent? (truth-table phi) (truth-table phi-simple)))))


(define (test-truth-tables-match?)
 (let ((phi '(OR P1 (NOT P2) (AND P3 (NOT P3)))))
  (let ((phi-simple '(NOT (AND P2 (NOT P1)))))
   (display "phi") (newline)
   (print-truth-table (truth-table phi)) (newline)
   (display "phi-simple") (newline)
   (print-truth-table (truth-table phi-simple)) (newline)
   (truth-tables-match? phi phi-simple))))

;;; Takes two truth tables and returns true if they are logically equivalent, otherwise #f.
;;; Table-simple must contain variables that are a subset of table
;;; The variables in phi-simple are a subset of the variables in phi.
;;; So if we look at one row of phi-simple and search for the variables in phi,
;;; then we are guaranteed to find at least one row of phi that matches phi-simple.
(define (truth-tables-equivalent? table table-simple)
 (cond ((null? table-simple) #t)
       ((row-equivelence? (first table-simple) (get-matching-rows table (first table-simple) '()))
	(truth-tables-equivalent? table (rest table-simple)))
       (else #f)))

;;; returns a list of all rows in table that have the same logic assignments as row
;;; note: table may have more variables than row. This is because some varaibles might be
;;; reduced out of our formula by boolean-simplify
(define (get-matching-rows table row matching-rows)
					;(display "get-matching-rows") (newline)
					;(display "row= ") (write row) (newline)
					;(display "table: ") (newline)
					;(print-truth-table table)
					;(display "matching-rows ") (write matching-rows) (newline) 
 (cond ((null? table) matching-rows)
       ((row-match? (first row) (first (first table)))
	(get-matching-rows (rest table) row (append matching-rows (list (first table)))))
       (else (get-matching-rows (rest table) row matching-rows))))
       

;;; returns true if the truth assignments in row1 are a subset of the truth assignments in row2
(define (row-match? row1 row2)
					;(newline) (display "row-match?") (newline)
					;(display "row1= ") (write row1) (newline)
					;(display "row2= ") (write row2) (newline)
 (cond ((null? row1) #t)
       ((set-member? (first row1) row2) (row-match? (rest row1) row2))
       (else #f)))
	
;;; returns true if every truth value in row-list is the same as the truth value of row
(define (row-equivelence? row row-list)
 (cond ((null? row-list) #t)
       ((eq? (second row) (second (first row-list)))
	(row-equivelence? row (rest row-list)))
       (else #f)))

;;; Takes a truth table and returns true if the formula is a tautology (always true)
(define (truth-table-tautology? table)
					;(display "truth-table-tautology? ") (write table) (newline)
 (cond ((null? table) #t)
       ((eq? (second (first table)) #t) (truth-table-tautology? (rest table)))
       (else #f)))

;;; Takes a truth table and returns true if the formula is a contradiction (always false)
(define (truth-table-contradiction? table)
					;(display "truth-table-contradiction? ") (write table) (newline)
 (cond ((null? table) #t)
       ((eq? (second (first table)) #f) (truth-table-contradiction? (rest table)))
       (else #f)))

;;; Helper function that prints out a nicely formatted truth table
(define (print-truth-table truth-table)
 (cond ((null? truth-table) '())
       (else (write (first truth-table)) (newline) (print-truth-table (rest truth-table)))))

;;; Code from p2.sc
;;; ===========================================================================
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


;;; Code from p1.sc
;;; ===========================================================================
(define (set-member? e s)
 (cond ((null? s) #f)
       ((null? e) #f)
       ((equal? (first s) e) #t)
       (else (set-member? e (rest s)))))

(define (set-create s)
 (cond ((null? s) s)
       ((set-member? (first s) (rest s)) (set-create (rest s)))
       (else (cons (first s) (set-create (rest s))))))

(define (set-union-recurse a b)
 (cond ((null? a) b)
       ((set-member? (first a) b) (set-union-recurse (rest a) b))
       (else (set-union-recurse (rest a) (cons (first a) b)))))

(define (set-union a b)
 (set-union-recurse (set-create a) (set-create b)))

(define (set-intersection-recurse a b)
 (cond ((null? a) a)
       ((set-member? (first a) b)
	(cons (first a) (set-intersection-recurse (rest a) b)))
       (else (set-intersection-recurse (rest a) b))))
 
(define (set-intersection a b)
 (set-intersection-recurse (set-create a) (set-create b)))

(define (set-minus-recurse a b)
 (cond ((null? a ) a)
       ((set-member? (first a) b) (set-minus-recurse (rest a) b))
       (else (cons (first a) (set-minus-recurse (rest a) b)))))
       
(define (set-minus a b)
 (set-minus-recurse (set-create a) (set-create b)))

