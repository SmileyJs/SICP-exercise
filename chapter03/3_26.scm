(load "../util.scm")

(display "3_26 \n")

(define (make-tree key value left-branch right-branch)
    (list key value left-branch right-branch)
)

(define (tree-empty? tree)
    (null? tree)
)

(define (tree-key tree)
    (car tree)
)

(define (tree-value tree)
    (cadr tree)
)

(define (tree-left-branch tree)
    (caddr tree)
)

(define (tree-right-branch tree)
    (cadddr tree)
)

(define (tree-set-key! key tree)
    (set-car! tree key)
)

(define (tree-set-value! value tree)
    (set-car! (cdr tree) value)
)

(define (tree-set-left-branch! new-left-branch tree)
    (set-car! (cddr tree) new-left-branch)
)

(define (tree-set-right-branch! new-right-branch tree)
    (set-car! (cdddr tree) new-right-branch)
)

(define (tree-insert! tree key value compare)
    (cond ((tree-empty? tree)
            (make-tree key value nil nil))
          (else
            (let ((result (compare key (tree-key tree))))
                (cond ((= 0 result)
                        (display "insert =\n")
                        (tree-set-value! value tree)
                        tree)
                      ((< 0 result)
                        (tree-set-left-branch!
                          (tree-insert! (tree-left-branch tree) key value compare) tree)
                        tree)
                      ((> 0 result)
                        (tree-set-right-branch!
                          (tree-insert! (tree-right-branch tree) key value compare) tree)
                        tree)
                ))))
)

(define (tree-search tree key compare)
    (if (tree-empty? tree)
        nil
        (let ((result (compare key (tree-key tree))))
            (cond ((= 0 result)
                     tree)
                  ((< 0 result)
                    (tree-search (tree-left-branch tree) key compare))
                  ((> 0 result)
                    (tree-search (tree-right-branch tree) key compare))
            )))
)

(define (compare-number x y)
    (cond ((= x y)
            0)
          ((< x y)
            -1)
          (else
            1))
)

(define (compare-string x y)
    (cond ((string=? x y)
            0)
          ((string<? x y)
            -1)
          (else
            1))
)

(define (compare-symbol x y)
    (compare-string (symbol->string x) (symbol->string y))
)

(define (make-table compare)
	(let ((root '()))
		(define (empty?)
			(tree-empty? root))

		(define (insert! key value)
			(set! root (tree-insert! root key value compare))
			'OK
		)

		(define (lookup key)
			(let ((result (tree-search root key compare)))
				(if (null? result)
					#f
					(tree-value result)))
		)

		(define (dispatch m)
			(cond ((eq? m 'empty?)
					empty?)
				  ((eq? m 'insert!)
				  	insert!)
				  ((eq? m 'lookup)
				  	lookup)
				  (else
				  	(error "unsupported operation: " m))		
			)
		)

	dispatch)
)

(define test-tree (tree-insert! '() 'jack 111 compare-symbol))
(display test-tree)
(newline)
(display (tree-search test-tree 'jack compare-symbol))
(newline)

(define test-tree2 (tree-insert! test-tree 'haha 222 compare-symbol))
(display test-tree2)
(newline)

(define test-tree3 (tree-insert! test-tree2 'aaa 3333 compare-symbol))
(display test-tree3)
(newline)

(define test (make-table compare-symbol))
(newline)

((test 'insert!) 'aa 11)
((test 'insert!) 'bbb 22)
((test 'insert!) 'cc 33)

(display ((test 'lookup) 'bbb))
(newline)
(display ((test 'lookup) 'bb))
(newline)
(display ((test 'empty?)))