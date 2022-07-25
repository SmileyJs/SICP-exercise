(load "../util.scm")
(load "p186_make_table.scm")

(define (attach-tag type-tag contents)
    (cons type-tag contents)
)

(define (type-tag data)
    (if (pair? data)
        (car data)
        (error "Bad tagged data --TYPE-TAG--" data))
)

(define (contents data)
    (if (pair? data)
        (cdr data)
        (error "Bad tagged data --CONTENTS--" data)))

(define same-variable? eq?)

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (variable? e)
    (symbol? e)
)

(define (deriv exp var)
    (cond   ((number? exp) 0)
            ((variable? exp)
                (if (eq? exp var) 1 0))
            (else ((get 'deriv (operator exp)) (operands exp) var)))
)

(define (install-sum-package)
    (define (addend e)
        (car e)
    )

    (define (augend e)
        (cadr e)
    )

    (define (=number? exp num)
        (and (number? exp) (= exp num))
    )

    (define (make-sum v1 v2)
        (cond   ((=number? v1 0) v2)
                ((=number? v2 0) v1)
                ((and (number? v1) (number? v2)) (+ v1 v2))
                (else (list '+ v1 v2)))
    )

    (define (deriv-sum exp var)
        (make-sum (deriv (addend exp) var)
                  (deriv (augend exp) var)))

    (put 'deriv '+ deriv-sum)
'done)

(define (install-product-package)
    (define (multiplier e)
        (car e)
    )

    (define (multiplicand e)
        (cadr e)
    )

    (define (=number? exp num)
        (and (number? exp) (= exp num))
    )

    (define (make-sum v1 v2)
        (cond   ((=number? v1 0) v2)
                ((=number? v2 0) v1)
                ((and (number? v1) (number? v2)) (+ v1 v2))
                (else (list '+ v1 v2)))
    )

    (define (make-product v1 v2)
        (cond   ((or (=number? v1 0) (=number? v2 0)) 0)
                ((=number? v1 1) v2)
                ((=number? v2 1) v1)
                ((and (number? v1) (number? v2)) (* v1 v2))
                (else (list '* v1 v2)))
    )

    (define (deriv-product exp var)
        (make-sum
            (make-product (multiplier exp)
                            (deriv (multiplicand exp) var))
            (make-product (deriv (multiplier exp) var)
                            (multiplicand exp))))

    (put 'deriv '* deriv-product)
'done)

(define (install-exp-package)
    (define (exponentiation? exp)
        (and (pair? exp) (eq? (car exp) '**))
    )

    (define (base exp)
        (car exp)
    )

    (define (exponent exp)
        (cadr exp)
    )

    (define (=number? exp num)
        (and (number? exp) (= exp num))
    )

    (define (make-sum v1 v2)
        (cond   ((=number? v1 0) v2)
                ((=number? v2 0) v1)
                ((and (number? v1) (number? v2)) (+ v1 v2))
                (else (list '+ v1 v2)))
    )

    (define (make-product v1 v2)
        (cond   ((or (=number? v1 0) (=number? v2 0)) 0)
                ((=number? v1 1) v2)
                ((=number? v2 1) v1)
                ((and (number? v1) (number? v2)) (* v1 v2))
                (else (list '* v1 v2)))
    )

    (define (make-exponentiation b e)
        (cond   ((=number? e 0) 1)
                ((=number? e 1) b)
                ((and (number? b) (number? e))
                    (expt b e))
                (else (list '** b e)))
    )

    (define (deriv-exp exp var)
        (make-product
            (make-product
                (exponent exp)
                (make-exponentiation
                    (base exp)
                    (make-sum (exponent exp) -1))) 
            (deriv (base exp) var))
    )

    (put 'deriv '** deriv-exp)
'done)

(install-sum-package)
(install-product-package)
(install-exp-package)

(define (apply-generic op . args)
    (let ((type-tags (map type-tag args)))
        (let ((proc (get op type-tags)))
            (if proc
                (aply proc (map contents args))
                (error "No method for these types --APPLY-GENERIC"
                (list op type-tags)))))
)

; test
(display (deriv '(+ x 3) 'x))
(newline)
(display (deriv '(* 3 x) 'x))
(newline)
(display (deriv '(** x 5) 'x))