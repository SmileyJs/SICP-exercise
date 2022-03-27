(load "2_2.scm")
(load-option 'format)

(define (seg-length seg)
    (let ((start-point (start-segment seg))
          (end-point (end-segment seg)))
            (sqrt (+ (square (- (x-point end-point)
                                (x-point start-point)))
                     (square (- (y-point end-point)
                                (y-point start-point))))))
)

(define (make-rect length-seg width-seg)
    (cons length-seg width-seg)
)

(define (rect-length rect)
    (let ((seg-1 (car rect))
          (seg-2 (cdr rect)))
        (let ((length-1 (seg-length seg-1))
              (length-2 (seg-length seg-2)))
            (if (> length-1 length-2)
                seg-1
                seg-2)))
)

(define (rect-width rect)
    (let ((seg-1 (car rect))
          (seg-2 (cdr rect)))
        (let ((length-1 (seg-length seg-1))
              (length-2 (seg-length seg-2)))
            (if (< length-1 length-2)
                seg-1
                seg-2)))
)

(define (rect-perimeter rect)
    (*  2
        (+  (seg-length (rect-length rect))
            (seg-length (rect-width rect))))
)

(define (rect-area rect)
    (*  (seg-length (rect-length rect))
        (seg-length (rect-width rect)))
)

(define (print-rect rect)
    (let ((seg-1 (car rect))
          (seg-2 (cdr rect)))
        (newline)
        (display "print-rect:")
        (print-point (start-segment seg-1))
        (print-point (start-segment seg-2))
        (print-point (end-segment seg-1))
        (print-point (end-segment seg-2))
    )
)

; test

(define point-1 (make-point 2 1))
(define point-2 (make-point 4 3))
(define point-3 (make-point 2 5))

(define seg-1 (make-segment point-1 point-2))
(define seg-2 (make-segment point-2 point-3))

(define rect-1 (make-rect seg-1 seg-2))

(print-rect rect-1)
(newline)
(display "rect-perimeter:")
(display (rect-perimeter rect-1))
(newline)
(display "rect-area:")
(display (rect-area rect-1))