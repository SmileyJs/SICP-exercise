(load "../util.scm")
(load-option 'format)

(define (random-in-range low high)
    (let ((range (- high low)))
        (+ low (random(exact->inexact range))))
)

(define (estimate-integral func x1 x2 y1 y2 trials)
     (* 4
        (monte-carlo 
            trials
            (lambda ()
                (func (random-in-range x1 x2)
                    (random-in-range y1 y2)))))
)

;用单位半径,方便计算
(define (estimate-pi trials)
    (exact->inexact
        (estimate-integral 
            (lambda (x y)
                (< (+ (square x) (square y)) 1.0))
            -1.0
            1.0
            -1.0
            1.0
            trials))
)

(define (monte-carlo trials experiment)
    (define (iter trials-remaining trials-passed)
        (cond ((= trials-remaining 0)
                    (/ trials-passed trials))
              ((experiment)
                    (iter (- trials-remaining 1) (+ trials-passed 1)))
              (else
                    (iter (- trials-remaining 1) trials-passed))))
    (iter trials 0)
)


(display (estimate-pi 10000))