(load "../util.scm")

(define (variable? e)
    (symbol? e)
)

(define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2))
)

(define (sum? e)
    (and (pair? e) (eq? '+ (cadr e)))
)

(define (addend e)
    (car e)
)

(define (product? e)
    (and (pair? e) (eq? '* (cadr e)))
)

(define (multiplier e)
    (car e)
)

(define (=number? exp num)
    (and (number? exp) (= exp num))
)

(define (make-sum v1 v2)
    (cond   ((=number? v1 0) v2)
            ((=number? v2 0) v1)
            ((and (number? v1) (number? v2)) (+ v1 v2))
            (else (list v1 '+ v2)))
)

(define (make-product v1 v2)
    (cond   ((or (=number? v1 0) (=number? v2 0)) 0)
            ((=number? v1 1) v2)
            ((=number? v2 1) v1)
            ((and (number? v1) (number? v2)) (* v1 v2))
            (else (list v1 '* v2)))
)

; 2_56
(define (exponentiation? exp)
    (and (pair? exp) (eq? (car exp) '**))
)

(define (base exp)
    (cadr exp)
)

(define (exponent exp)
    (caddr exp)
)

(define (make-exponentiation b e)
    (cond   ((=number? e 0) 1)
            ((=number? e 1) b)
            ((and (number? b) (number? e))
                (expt b e))
            (else (list '** b e)))
)

;2_57
(define (augend e)
    (caddr e)
)

(define (multiplicand e)
    (caddr e)
)

(define (deriv exp var)
    (cond   ((number? exp) 0)
            ((variable? exp)
                (if (same-variable? exp var) 1 0))
            ((sum? exp)
                (make-sum   (deriv (addend exp) var)
                            (deriv (augend exp) var)))
            ((product? exp)
                (make-sum
                    (make-product (multiplier exp) (deriv (multiplicand exp) var))
                    (make-product (multiplicand exp) (deriv (multiplier exp) var))))
            ((exponentiation? exp)
                (make-product
                    (make-product
                        (exponent exp)
                        (make-exponentiation
                            (base exp)
                            (make-sum (exponent exp) -1))) 
                    (deriv (base exp) var))
            )
            (else
                (error "unknown expression type -- deriv" exp)))
)

(display (deriv '(x + 3) 'x))
(newline)
(display (deriv '(x * y) 'x))
(newline)
(display (deriv '((x * y) * (x + 3)) 'x))
(newline)
(display (deriv '(x + (3 * (x + (y + 2)))) 'x))
(newline)
(display (sum? '(x + 3)))
(newline)
(display (product? '(x + 3)))
(newline)
(display (augend '(x + 3)))
(newline)
(display (multiplicand '(x * 3)))
(newline)