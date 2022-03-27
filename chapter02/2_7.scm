(load "../util.scm")

(define (make-interval a b)
    (cons a b)
)

(define (upper-bound x)
    (max (car x) (cdr x))
)

(define (lower-bound x)
    (min (car x) (cdr x))
)

(define (add-interval x y)
    (make-interval  (+ (lower-bound x) (lower-bound y))
                    (+ (upper-bound x) (upper-bound y)))
)

(define (old-mul-interval x y)
    (let ((p1 (* (lower-bound x) (lower-bound y)))
          (p2 (* (lower-bound x) (upper-bound y)))
          (p3 (* (upper-bound x) (lower-bound y)))
          (p4 (* (upper-bound x) (upper-bound y))))

        (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4)))
)

(define (div-interval x y)
    (old-mul-interval x
                    (if (> 0 (* (lower-bound y) (upper-bound y)))
                        (error "Division error (interval span 0)")
                        (make-interval  (/ 1.0 (upper-bound y))
                                        (/ 1.0 (lower-bound y)))))
)

(define (print-interval v)
    (display "{lower: ")
    (display (lower-bound v))
    (display ", upper: ")
    (display (upper-bound v))
    (display "}")
)

;2_8
(define (sub-interval a b)
    (make-interval  (- (upper-bound a) (lower-bound b))
                    (- (lower-bound a) (upper-bound b)))
)

;2-9 new-mutiplication
(define (mul-interval a b)
    ; 1 means the elements are positive,
    ; -1 means the elements are negative
    ; and 0 means the elements span 0
    (define (sign-interval i)
        (cond
            ((>= (lower-bound i) 0) 1)
            ((< (upper-bound i) 0) -1)
            (else 0)))

    (define (signs sa sb)
        (and (= sa (sign-interval a)) (= sb (sign-interval b)))
    )

    (let    ((x0 (lower-bound a))
            (x1 (upper-bound a))
            (y0 (lower-bound b))
            (y1 (upper-bound b)))

        (cond
            ((signs  1 1) (make-interval (* x0 y0) (* x1 y1)))
            ((signs -1 -1) (make-interval (* x0 y0) (* x1 y1)))
            ((signs 1 -1) (make-interval (* x1 y0) (* x0 y1)))
            ((signs -1 1) (make-interval (* x0 y1) (* x1 y0)))
            ((signs 1 0) (make-interval (* x1 y0) (* x1 y1)))
            ((signs -1 0) (make-interval (* x0 y1) (* x0 y0)))
            ((signs 0 1) (make-interval (* x0 y1) (* x1 y1)))
            ((signs 0 -1) (make-interval (* x1 y0) (* x0 y0)))
            ((signs 0 0) (make-interval (min (* x0 y1) (* x1 y0)) (max (* x0 y0) (* x1 y1))))
        )
    )
)

(define (make-center-percent center percent)
    (make-interval (- center (* center percent)) (+ center (* center percent)))
)

(define (make-center-width center width)
    (make-interval (- center width) (+ center width))
)

(define (center i)
    (/ (+ (upper-bound i) (lower-bound i)) 2)
)

(define (width i)
    (/ (- (upper-bound i) (lower-bound i)) 2)
)

(define (percent i)
    (/ (width i) (center i))
)

; test

(define x (make-interval 5 6))
(define y (make-interval 2 4))

(print-interval x)
(newline)
(print-interval y)
(newline)
(print-interval (add-interval x y))
(newline)
(print-interval (sub-interval x y))
(newline)
(print-interval (old-mul-interval x y))
(newline)
(print-interval (div-interval x y))

; error test
; (define y0 (make-interval 2 -4))
; (newline)
; (print-interval (div-interval x y0))

; test 2-9, copied from http://community.schemewiki.org/?sicp-ex-2.11
(define (eql-interval? a b)
    (and    (= (upper-bound a) (upper-bound b)) 
            (= (lower-bound a) (lower-bound b)))) 
  
 ;; Fails if the new mult doesn't return the same answer as the old 
 ;; naive mult. 
(define (ensure-mult-works aH aL bH bL) 
    (let ((a (make-interval aL aH)) 
         (b (make-interval bL bH))) 
    (if (eql-interval? (old-mul-interval a b) 
                      (mul-interval a b)) 
        true 
        (error "new mult returns different value!"  
              a  
              b  
              (old-mul-interval a b) 
              (mul-interval a b))))) 
  
  
;; The following is overkill, but it found some errors in my 
;; work.  The first two #s are the endpoints of one interval, the last 
;; two are the other's.  There are 3 possible layouts (both pos, both 
;; neg, one pos one neg), with 0's added for edge cases (pos-0, 0-0, 
;; 0-neg). 
  
(ensure-mult-works  +10 +10   +10 +10) 
(ensure-mult-works  +10 +10   +00 +10) 
(ensure-mult-works  +10 +10   +00 +00) 
(ensure-mult-works  +10 +10   +10 -10) 
(ensure-mult-works  +10 +10   -10 +00) 
(ensure-mult-works  +10 +10   -10 -10) 

(ensure-mult-works  +00 +10   +10 +10) 
(ensure-mult-works  +00 +10   +00 +10) 
(ensure-mult-works  +00 +10   +00 +00) 
(ensure-mult-works  +00 +10   +10 -10) 
(ensure-mult-works  +00 +10   -10 +00) 
(ensure-mult-works  +00 +10   -10 -10) 

(ensure-mult-works  +00 +00   +10 +10) 
(ensure-mult-works  +00 +00   +00 +10) 
(ensure-mult-works  +00 +00   +00 +00) 
(ensure-mult-works  +00 +00   +10 -10) 
(ensure-mult-works  +00 +00   -10 +00) 
(ensure-mult-works  +00 +00   -10 -10) 

(ensure-mult-works  +10 -10   +10 +10) 
(ensure-mult-works  +10 -10   +00 +10) 
(ensure-mult-works  +10 -10   +00 +00) 
(ensure-mult-works  +10 -10   +10 -10) 
(ensure-mult-works  +10 -10   -10 +00) 
(ensure-mult-works  +10 -10   -10 -10) 

(ensure-mult-works  -10 +00   +10 +10) 
(ensure-mult-works  -10 +00   +00 +10) 
(ensure-mult-works  -10 +00   +00 +00) 
(ensure-mult-works  -10 +00   +10 -10) 
(ensure-mult-works  -10 +00   -10 +00) 
(ensure-mult-works  -10 +00   -10 -10) 

(ensure-mult-works  -10 -10   +10 +10) 
(ensure-mult-works  -10 -10   +00 +10) 
(ensure-mult-works  -10 -10   +00 +00) 
(ensure-mult-works  -10 -10   +10 -10) 
(ensure-mult-works  -10 -10   -10 +00) 
(ensure-mult-works  -10 -10   -10 -10) 

(newline)
(define pi (make-center-percent 100 0.05))
(print-interval pi)
(newline)
(display (percent pi))

;2_14
(define (par1 r1 r2)
    (div-interval (mul-interval r1 r2) (add-interval r1 r2))
)

(define (par2 r1 r2)
    (let ((one (make-interval 1 1)))
        (div-interval one
                      (add-interval (div-interval one r1)
                                    (div-interval one r2))))
)

(define r1 (make-center-percent 10 0.1))
(define r2 (make-center-percent 10 0.1))

(newline)
(print-interval (par1 r1 r2))
(newline)
(print-interval (par2 r1 r2))