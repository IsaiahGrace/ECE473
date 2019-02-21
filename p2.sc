;;; (map-reduce formula-eval (truth-assignments (get-vars phi)))
;;; Break down input string into set of propositions
;;; construct a truth binding for the propositions (truth table)
;;; map-reduce the truth table with formula-eval
;;; construct the truth table

;;; We are building a satisfiability fuction
;;; We will return a list of rows of truth assignments
;;;    that cause the entire expression phi to be true
;;; We must recursivly break down AND and OR statements
;;; There will be 2^n possible truth assignments for a phi with n propositions
;;; We are only keeping track of truth-ness, maybe there is some way to limit recursion when
;;;    we know that there is no way to satisfy the sub-string
(define(test)
 (write (reduce (get-vars '(AND (AND #F (OR P3 P2 P3 P1) (OR #F (NOT P3) P4)))))))

(define (truth-table phi)
 (display "phi=") (write phi) (newline)
 (write (get-vars phi)) (newline)
 '((((p0 #f) (p1 #f)) #f)
   (((p0 #f) (p1 #t)) #f)
   (((p0 #t) (p1 #f)) #f)
   (((p0 #t) (p1 #t)) #t)))

;;; The evaluate function takes a string phi in the propositional logic language
;;;     and a complete, non-redundant, consistant truth assignment i, and evaluates
;;;     the string, returning either #t or #f
(define (evaluate? phi i)
 (cond ((= phi #t) #t)
       ((= phi #f) #t)
       ((= phi "a proposition") (lookup? phi i))
       ((= phi "AND statement") 
	(and (evaluate? (first phi) i) (evaluate? (third phi) i))) ; Is it the third? or second?
       ((= phi "OR statement") 
	(or (evaluate? (first phi) i) (evaluate? (third phi) i))))) ; Is it the third? or second?
	

;;; returns the truth value assigned to proposition prop by truth assignment i
(define (lookup? prop i)
 (cond ((null? i) #f)
       ((= prop (first (first i)) (first (second i))))
       (else (lookup? prop (rest i)))))

(define (get-vars phi)
 (display "else= ") (write phi) (newline)
 (cond ((null? phi) phi)
       ((eq? (first phi) 'AND) (get-vars (rest phi)))
       ((eq? (first phi) 'OR) (get-vars (rest phi)))
       ((eq? (first phi) 'NOT) (get-vars (rest phi)))
       ((eq? (first phi) #T) (get-vars (rest phi)))
       ((eq? (first phi) #F) (get-vars (rest phi)))
       ((list? (first phi)) (cons (get-vars (first phi)) (get-vars (rest phi))))
       (else (cons (first phi) (get-vars (rest phi))))))


