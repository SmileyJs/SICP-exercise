(load "../util.scm")

(define (entry tree)
    (car tree)
)

(define (left-branch tree)
    (cadr tree)
)

(define (right-branch tree)
    (caddr tree)
)

(define (make-tree entry left right)
    (list entry left right)
)

(define (element-of-set? x set)
    (cond   ((null? set) #f)
            ((= x (entry set)) #t)
            ((> x (entry set))
                (element-of-set? x (right-branch set)))
            ((< x (entry set))
                (element-of-set? x (left-branch set))))
)

(define (adjion-set x set)
    (cond   ((null? set) (make-tree x nil nil))
            ((= x (entry set)) set)
            ((> x (entry set))
                (make-tree (entry set) (left-branch set) (adjion-set x (right-branch set))))
            ((< x (entry set))
                (make-tree (entry set) (adjion-set x (left-branch set)) (right-branch set))))
)

(define (tree->list-1 tree)
    (if (null? tree)
        nil
        (append (tree->list-1 (left-branch tree))
                (cons (entry tree)
                    (tree->list-1 (right-branch tree)))))
)

(define (tree->list-2 tree)
    (define (copy-to-list tree result-list)
        (if (null? tree)
            result-list
            (copy-to-list (left-branch tree)
                (cons (entry tree)
                    (copy-to-list (right-branch tree) result-list)))))

    (copy-to-list tree nil)
)

(define tree-a
    (make-tree 7
        (make-tree 3
            (make-tree 1 nil nil)
            (make-tree 5 nil nil))
        (make-tree 9
            nil
            (make-tree 11 nil nil))))

(display tree-a)
(newline)
(display (tree->list-1 tree-a))
(newline)
(display (tree->list-2 tree-a))