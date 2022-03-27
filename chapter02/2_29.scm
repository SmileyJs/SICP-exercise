(load "../util.scm")

(define (make-mobile left right)
    (list left right)
)

(define (make-branch length structure)
    (list length structure)
)

(define left-branch car)
(define right-branch cadr)

(define (branch-length branch)
    (left-branch branch)
)

(define (branch-structure branch)
    (right-branch branch)
)

(define (hangs-another-mobile? branch)
    (pair? (branch-structure branch))
)

(define (branch-weight branch)
    (if (hangs-another-mobile? branch)
        (total-weight (branch-structure branch))
        (branch-structure branch))
)

(define (branch-torque branch)
    (*  (branch-length branch)
        (branch-weight branch))
)

(define (total-weight items)
    (+ (branch-weight (left-branch items))
       (branch-weight (right-branch items)))
)

(define (equal-torque left right)
    (=  (branch-torque left)
        (branch-torque right))
)

(define (balanced-branch? branch)
    (if (hangs-another-mobile? branch)
        (is-balanced? (branch-structure branch))
        #t)
)

(define (is-balanced? items)
    (if (null? items)
        #t
        (and (equal-torque (left-branch items) (right-branch items))
             (balanced-branch? (left-branch items))
             (balanced-branch? (right-branch items))))
)

(define x (make-branch 6 10))
(define y (make-branch 3 20))
(define tree (make-mobile x y))

(define z (make-branch 4 tree))

(define tree-b (make-mobile z y))

(define w (make-branch 4 30))
(define tree-c (make-mobile z w))

(display tree)
(newline)
(display (branch-length (left-branch tree)))
(newline)
(display (branch-length (right-branch tree)))
(newline)
(display tree-b)
(newline)
(display (branch-length (left-branch tree-b)))
(newline)
(display (branch-length (right-branch tree-b)))
(newline)
(display (total-weight tree))
(newline)
(display (total-weight tree-b))
(newline)
(display (is-balanced? tree))
(newline)
(display (is-balanced? tree-b))
(newline)
(display (is-balanced? tree-c))