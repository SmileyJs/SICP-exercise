; 1_33.scm

(load-option 'format)
(load "1_28.scm")
(load "1_21.scm")

(define (filter-accumulate combiner null-value term a next b filter)
    (define (iter i result)
        (cond ((> i b) result)
              ((filter i) (iter (next i) (combiner (term i) result)))
              (else (iter (next i) result))))

    (iter a null-value)
)

(define (filter-accumulate-recursive combiner null-value term a next b filter)
    (if (> a b)
        null-value
        (let ((rest-terms (filter-accumulate-recursive combiner null-value term (next a) next b filter)))
             (if (filter a)
                 (combiner (term a) rest-terms)
                 rest-terms)))
)

(define (sum a b filter)
    (filter-accumulate-recursive +
                        0
                        (lambda (x) x)
                        a
                        (lambda (x) (+ x 1))
                        b
                        filter)
)

(define (product a b filter)
    (filter-accumulate *
                        1
                        (lambda (x) x)
                        a
                        (lambda (x) (+ x 1))
                        b
                        filter)
)

(define (filter-true a)
    #t
)

(display (sum 1 10 prime?))
(newline)


(define (coprime? i n)
    (and (< i n)
         (= 1 (gcd i n)))
)

(define (product-coprimes n)
    (filter-accumulate *
                        1
                        (lambda (x) x)
                        1
                        (lambda (x) (+ x 1))
                        n
                        (lambda (x) (coprime? x n)))
)

(display (product-coprimes 10))