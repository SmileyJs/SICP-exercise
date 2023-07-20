(load "../util.scm")

(define (make-table same-key?)
    (let ((local-table (list '*table*)))

    (define (assoc key records)
        (cond ((null? records) false)
            ((same-key? key (caar records)) (car records))
            (else (assoc key (cdr records))))
    )

    (define (lookup key1 key2)
        (let ((subtable (assoc key1 (cdr local-table))))
            (if subtable
                (let ((record (assoc key2 (cdr local-table))))
                    (if record
                        (cdr record)
                        false))
                false))
    )

    (define (insert! key1 key2 value)
        (let ((subtable (assoc key1 (cdr local-table))))
            (if subtable
                (let ((record (assoc key2 (cdr subtable))))
                    (if record
                        (set-cdr! record value)
                        (set-cdr! subtable (cons (cons key2 value) (cdr subtable)))))
                (set-cdr! local-table (cons (list key1 (cons key2 value)) (cdr local-table)))))
        'ok
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

(put 't1 'a 'aa)
(show)
(newline)

(put 't1 'b 'bb)
(show)
(newline)

(put 't2 'c 'cc)
(show)
(newline)