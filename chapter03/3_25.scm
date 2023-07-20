(load "../util.scm")

(define (make-table same-key?)
    (let ((local-table (list '*table*)))

    (define (assoc key records)
        (cond ((null? records) #f)
            ((same-key? key (caar records)) (car records))
            (else (assoc key (cdr records))))
    )

    (define ele-key 'xxx)

    (define (lookup key-list)
        (define (iter keys table)
            (if (null? keys)
                (let ((record (assoc ele-key (cdr table))))
                    (if (eq? record #f)
                        #f
                        (cdr record)))
                (let ((record (assoc (car keys) (cdr table))))
                    (if (eq? record #f)
                        #f
                        (iter (cdr keys) (cdr record))))
            )        
        )

        (if (list? key-list)
            (iter key-list local-table)
            (iter (list key-list) local-table)
        )
    )

    (define (insert! key-list value)
        (define (iter keys value table)
            (if (null? keys)
                (let ((record (assoc ele-key (cdr table))))
                    (cond ((eq? record #f)
                            (set-cdr! table (cons (cons ele-key value) (cdr table))))
                        (else
                            (set-cdr! record value))))
                (let ((record (assoc (car keys) (cdr table))))
                    (cond ((eq? record #f)
                            (set-cdr! table (cons (cons (car keys) (list '*sub-tree*)) (cdr table)))
                            (iter (cdr keys) value (cdadr table)))
                          (else
                            (iter (cdr keys) value (cdr record)))))
            )
        )

        (if (list? key-list)
            (iter key-list value local-table)
            (iter (list key-list) value local-table)
        )
        
    )

    (define (show) 
        (display local-table)
    )

    (define (dispatch m)
        (cond ((eq? m 'lookup-proc) lookup)
              ((eq? m 'insert-proc!) insert!)
              ((eq? m 'show-proc) show)
              (else (error "Unknown operation -- TABLE"))
        )
    )

    dispatch)
)


(define operation-table (make-table equal?))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))
(define show (operation-table 'show-proc))

(put 't1 'a)
(put 't1 'b)
(put (list 't2 'k1) 'aa)
(put (list 't2 'k2) '22)
(put (list 't3 'k2) 'bb)
(put (list 't4 'k3) 'cc)
(put (list 't5 'k4 'k5 'k6) 'dd)
(put (list 't5 'k4) 'ee)
(show)
(newline)

(display (get 't1))
(newline)
(display (get (list 't2 'k2)))
(newline)
(display (get (list 't2 'k3)))
(newline)
(display (get (list 't5 'k4 'k5)))
(newline)
(display (get (list 't5 'k4 'k5 'k6)))
(newline)
(put (list 't5 'k4 'k5) 'qq)
(display (get (list 't5 'k4 'k5)))