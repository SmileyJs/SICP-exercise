;calculate each element of the pascal triangle

(define (pascal row col)
    (cond ((or (= col 0) (= row col)) 1)
          (else (+ (pascal (- row 1) (- col 1)) (pascal (- row 1) col)))
    )
)

; iter formula (row col) = (row!) / (col! * (row−col)!) , ! means factorial

(define (fact_iter product counter max_count)
    (if (> counter max_count)
        product
        (fact_iter (* product counter) (+ counter 1) max_count)
    )
)

(define (factorial n)
    (fact_iter 1 1 n)
)

(define (pascal_iter m n)
    (display "pascal_iter")
    (/ (factorial m)
       (* (factorial n) (factorial (- m n)))
    )
)

(write (pascal 3 3))
(write (pascal 5 3))

(write (factorial 5))

(write (pascal_iter 3 3))
(write (pascal_iter 5 3))
