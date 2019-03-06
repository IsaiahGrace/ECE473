;;; Problem set 3: Symbolic manipulation


;;; Impliment the following re-write rules:
;;;                                    (not #t) -> #f
;;;                                    (not #f) -> #t
;;;                                (not (notΦ)) -> Φ

;;;                                       (and) -> #t
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
 (display "boolean-simplify phi=") (write phi) (newline))

;;; truth-tables-match? Φ Φ'
;;; Φ and Φ' are formulas. Φ' will be the result of (boolean-simplify Φ).
;;;     Compares the truth tables of Φ and Φ' and returns #t if they are the
;;;     same and #f if they are different.
(define (truth-table-match? phi phi_simple)
 (display "truth-table-match?") (newline)
 (display "phi= ") (write phi) (display " phi_simple= ") (write phi_simple) (newline)
 (display "(truth-table phi)= ") (write (truth-table phi)) (newline)
 (display "(truth-table phi_simple)= ") (write (truth-table phi_simple)) (newline)
 
 (cond ((null? (first (first phi_simple))) ; First base case, phi_simple is not dependent on any variables
 )

;;; Takes two rows from two differnt truth tables and returns true if the bindings are the same for each
;;; This mihgt not work because we don't need to find identical rows, we need to find logical equivaence...
(define (row-binding-equal?)
 #t)

;;; Test function
(define (test-truth-table-match?)
 (truth-table-match? '(AND P1 (NOT P1)) '#f))

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
       ((eq? (first s) e) #t)
       (else (set-member? e (rest s)))))

(define (set-create s)
 (cond ((null? s) s)
       ((set-member? (first s) (rest s)) (set-create (rest s)))
       (else (cons (first s) (set-create (rest s))))))
