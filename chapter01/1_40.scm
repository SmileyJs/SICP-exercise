(load "1_35.scm")

(define (sqrt x)
    (fixed-point (average-damp (lambda (y) (/ x y))) 1.0)
)

(newline)
(display "1_35 ----------------")
(newline)
(display (sqrt 4))
(newline)

(define dx 0.000001)

(define (deriv g)
    (lambda (x) (/ (- (g (+ x dx)) (g x))
                   dx))
)

(define (newton-transform g)
    (lambda (x) (- x
                   (/ (g x)
                      ((deriv g) x))))
)

(define (newton-method g guess)
    (fixed-point (newton-transform g) guess)
)

(define (sqrt-newton x)
    (newton-method (lambda (y) (- (square y) x)) 1.0)
)

(display (sqrt-newton 9))
(newline)

; 1_40
(define (cubic a b c)
    (lambda (x) (+  (cube x)
                    (* a (square x))
                    (* b x)
                    c))
)

(display (newton-method (cubic 2 5 5) 1.0))