(define (set-member? e s)
 (cond ((null? s) #f)
       ((null? e) #f)
       ((= (first s) e) #t)
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

