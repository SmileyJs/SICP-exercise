(load "../util.scm")

(define (make-vect x y)
    (cons x y)
)

(define (xcor-vect vect)
    (car vect)
)

(define (ycor-vect vect)
    (cdr vect)
)

(define (add-vect v1 v2)
    (make-vect  (+  (xcor-vect v1)
                    (xcor-vect v2))
                (+  (ycor-vect v1)
                    (ycor-vect v2)))
)

(define (sub-vect v1 v2)
    (make-vect  (-  (xcor-vect v1)
                    (xcor-vect v2))
                (-  (ycor-vect v1)
                    (ycor-vect v2)))
)

(define (scale-vect vect factor)
    (make-vect (* factor (xcor-vect vect))
               (* factor (ycor-vect vect)))
)

;2-47
(define (make-frame origin edge1 edge2)
    (cons origin (cons edge1 edge2))
)

(define (frame-origin frame)
    (car frame)
)

(define (frame-edge1 frame)
    (cadr frame)
)

(define (frame-edge2 frame)
    (cddr frame)
)

(define (make-segment v1 v2)
    (cons v1 v2)
)

(define (start-segment seg)
    (car seg)
)

(define (end-segment seg)
    (cdr seg)
)

(define (transform-painter painter origin corner1 corner2)
    (lambda (frame)
        (let ((m (frame-coord-map frame)))
            (let ((new-origin (m origin)))
                (painter
                    (make-frame new-origin
                                (sub-vect (m corner1) new-origin)
                                (sub-vect (m corner2) new-origin))))))
)

(define (flip-vert painter)
    (transform-painter
        painter
        (make-vect 0.0 1.0)
        (make-vect 1.0 1.0)
        (make-vect 0.0 0.0))
)

(define (flip-horiz painter)
    (transform-painter
        painter
        (make-vect 1.0 0.0)
        (make-vect 0.0 0.0)
        (make-vect 1.0 1.0))
)

(define (shrink-to-upper-right painter)
    (transform-painter
        painter
        (make-vect 0.5 0.5)
        (make-vect 1.0 0.5)
        (make-vect 0.5 1.0))
)

(define (rotate90 painter)
    (transform-painter
        painter
        (make-vect 1.0 0.0)
        (make-vect 1.0 1.0)
        (make-vect 0.0 0.0))
)

(define (squash-inwards painter)
    (transform-painter
        painter
        (make-vect 0.0 0.0)
        (make-vect 0.65 0.35)
        (make-vect 0.35 0.65))
)

(define (besides painter1 painter2)
    (let ((split-point (make-vect 0.5 0.0)))
        (let ((painter-left
                (transform-painter
                    painter1
                    (make-vect 0.0 0.0)
                    split-point
                    (make-vect 0.0 1.0)))
               (painter-right
                (transform-painter
                    painter1
                    split-point
                    (make-vect 1.0 1.0)
                    (make-vect 0.5 1.0))))

    (lambda (frame)
        (painter-left frame)
        (painter-right frame))))
)

(define (below painter1 painter2)
    (let ((split-point (make-vect 0.0 0.5)))
        (let ((painter-up
                (transform-painter
                    painter1
                    (make-vect 0.0 0.0)
                    (make-vect 0.0 1.0)
                    split-point))
               (painter-low
                (transform-painter
                    painter1
                    split-point
                    (make-vect 1.0 0.5)
                    (make-vect 1.0 1.0))))

    (lambda (frame)
        (painter-up frame)
        (painter-low frame))))
)


;test
(define (show-vect vect)
    (display "x: ")
    (display (xcor-vect vect))
    (display " y: ")
    (display (ycor-vect vect))
    (newline)
)

(define (show-frame frame)
    (display "origin: ")
    (display (frame-origin frame))
    (display " edge1: ")
    (display (frame-edge1 frame))
    (display " edge2: ")
    (display (frame-edge2 frame))
)

(define (show-segment seg)
    (display "start seg:")
    (newline)
    (show-vect (start-segment seg))
    (display "end seg:")
    (newline)
    (show-vect (end-segment seg))
)

(define vect-a (make-vect 2 2))
(define vect-b (make-vect 1 1))

(show-vect vect-a)
(show-vect vect-b)

(show-vect (add-vect vect-a vect-b))
(show-vect (sub-vect vect-a vect-b))
(show-vect (scale-vect vect-b 3))
(show-vect (scale-vect vect-b 0.5))

(define frame-a (make-frame 4 5 6))

(show-frame frame-a)

(define seg-a (make-segment vect-a vect-b))
(define seg-b (make-segment (make-vect 5 6) (make-vect 7 8)))

(show-segment seg-a)
(show-segment seg-b)