;;; We are building a satisfiability fuction
;;; We will return a list of rows of truth assignments
;;;    that cause the entire expression phi to be true
;;; We must recursivly break down AND and OR statements
;;; There will be 2^n possible truth assignments for a phi with n propositions
;;; We are only keeping track of truth-ness, maybe there is some way to limit recursion when
;;;    we know that there is no way to satisfy the sub-string
(define (truth-table phi)
 
 )

;;; The evaluate function takes a string phi in the propositional logic language
;;;     and a complete, non-redundant, consistant truth assignment i, and evaluates
;;;     the string, returning either #t or #f
(define (evaluate? phi i)
 (cond ((= phi #t) #t)
       ((= phi #f) #t)
       ((= phi "a proposition") (lookup? phi i))))

(define (lookup? prop i)
 (cond (null? prop) #f
  
  
