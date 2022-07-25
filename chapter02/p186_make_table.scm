(define (make-table)
    (let ((local-table (list '*table*)))
        (define (lookup key-1 key-2)
            (display "lookup key-1: ")
            (display key-1)
            (newline)
            (display "key-2: ")
            (display key-2)
            (newline)
            (let ((subtable (assoc key-1 (cdr local-table))))
                (display subtable)
                (newline)
                (if subtable
                    (let ((record (assoc key-2 (cdr subtable))))
                        (display record)
                        (newline)
                        (if record
                            (cdr record)
                            #f))
                    #f)))
        (define (insert! key-1 key-2 value)
            (display "intert key-1: ")
            (display key-1)
            (newline)
            (display "key-2: ")
            (display key-2)
            (newline)
            (display "value: ")
            (display value)
            (newline)
            (let ((subtable (assoc key-1 (cdr local-table))))
                (if subtable
                    (let ((record (assoc key-2 (cdr subtable))))
                        (if record
                            (set-cdr! record value)
                            (set-cdr! subtable
                                      (cons (cons key-2 value)
                                            (cdr subtable)))))
                    (set-cdr! local-table
                              (cons (list key-1
                                          (cons key-2 value))
                                    (cdr local-table)))))
            'ok)
        (define (dispatch m)
            (cond ((eq? m 'lookup-proc) lookup)
                  ((eq? m 'insert-proc!) insert!)
                  (else 
                    (error "Unknown operation -- TABLE" m))))
        dispatch)
)

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))
