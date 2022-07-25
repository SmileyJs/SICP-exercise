(load "../util.scm")

(define (make-leaf symbol weight)
    (list 'leaf symbol weight)
)

(define (leaf? object)
    (eq? 'leaf (car object))
)

(define (symbol-leaf x)
    (cadr x)
)

(define (weight-leaf x)
    (caddr x)
)

(define (symbols tree)
    (if (leaf? tree)
        (list (symbol-leaf tree))
        (caddr tree))
)

(define (weight tree)
    (if (leaf? tree)
        (weight-leaf tree)
        (cadddr tree))
)

(define (left-branch tree)
    (car tree)
)

(define (right-branch tree)
    (cadr tree)
)

(define (make-code-tree left right)
    (list   left
            right
            (append (symbols left) (symbols right))
            (+ (weight left) (weight right)))
)

(define (choose-branch bit tree)
    (cond   ((= bit 0) (left-branch tree))
            ((= bit 1) (right-branch tree))
            (else (error "bad bit- CHOOSE-BRANCH" bit)))
)

(define (decode bits tree)
    (define (decode-helper bits current-branch)
        (if (null? bits)
            '()
            (let ((next-branch (choose-branch (car bits) current-branch)))
                (if (leaf? next-branch)
                    (cons (symbol-leaf next-branch) (decode-helper (cdr bits) tree))
                    (decode-helper (cdr bits) next-branch)))))

    (decode-helper bits tree)
)

;2-68
(define (element-of-set? set x)
    (cond   ((null? set) #f)
            ((equal? x (car set)) #t)
            (else (element-of-set? (cdr set) x)))
)

(define (encode-symbol s tree)
    (cond   ((leaf? tree) '())
            ((element-of-set? (symbols (left-branch tree)) s) (cons 0 (encode-symbol s (left-branch tree))))
            ((element-of-set? (symbols (right-branch tree)) s) (cons 1 (encode-symbol s (right-branch tree))))
            (else (error "encode failed! bad symbol -ENCODE-SYMBOL" s)))
)

(define (encode message tree)
    (if (null? message)
        '()
        (append (encode-symbol (car message) tree)
                (encode (cdr message) tree)))
)

; 2-69
(define (adjoin-set x set)
    (cond   ((null? set) (list x))
            ((< (weight x) (weight (car set))) (cons x set))    
            (else (cons (car set) (adjoin-set x (cdr set)))))
)

(define (make-leaf-set pairs)
    (if (null? pairs)
        '()
        (let ((pair (car pairs)))
            (adjoin-set (make-leaf (car pair) (cadr pair))
                        (make-leaf-set (cdr pairs)))))
)

(define (successive-merge leaf-set)
    (cond   ((null? leaf-set) '())
            ((= 1 (length leaf-set)) (car leaf-set))
            (else (let  ((rest (cddr leaf-set))
                         (first (car leaf-set))
                         (second (cadr leaf-set)))
                        (successive-merge (adjoin-set   (make-code-tree first second)
                                                        rest)))))
)

(define (generate-huffman-tree pairs)
    (successive-merge (make-leaf-set pairs))
)

; test 2-67
(define sample-tree (make-code-tree (make-leaf 'A 4)
                                    (make-code-tree (make-leaf 'B 2)
                                                    (make-code-tree (make-leaf 'C 1)
                                                                    (make-leaf 'D 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(display (decode sample-message sample-tree))
(newline)

; test 2-68
(display (encode (list 'a 'c 'a 'b 'b 'd 'a) sample-tree))
(newline)

; test 2-69
(define sym-freq-set (list (list 'A 4) (list 'B 2) (list 'C 1) (list 'D 1)))
(define sample-tree-2 (generate-huffman-tree sym-freq-set))

(display (encode (list 'a 'c 'a 'b 'b 'd 'a) sample-tree-2))
(newline)
(display (decode '(0 1 1 1 0 1 0 1 0 1 1 0 0) sample-tree-2))
(newline)

;test 2-70
(define freq-set-2 (list    (list 'A 2)
                            (list 'Na 16)
                            (list 'BOOM 1)
                            (list 'SHA 3)
                            (list 'GET 2)
                            (list 'YIP 9)
                            (list 'JOB 2)
                            (list 'WAH 1)))
                        
(define freq-set-3 '((A 2) (Na 16) (BOOM 1) (SHA 3) (GET 2) (YIP 9) (JOB 2) (WAH 1)))

(define sample-tree-3 (generate-huffman-tree freq-set-3))
(display sample-tree-3)
(newline)
(display (encode (list 'Get 'a 'job) sample-tree-3))
(newline)
(display (encode (list 'Sha 'na 'na 'na 'na 'na 'na 'na 'na) sample-tree-3))
(newline)
(display (encode (list 'Get 'a 'job) sample-tree-3))
(newline)
(display (encode (list 'Sha 'na 'na 'na 'na 'na 'na 'na 'na) sample-tree-3))
(newline)
(display (encode (list 'Wah 'yip 'yip 'yip 'yip 'yip 'yip 'yip 'yip 'yip) sample-tree-3))
(newline)
(display (encode (list 'Sha 'boom) sample-tree-3))
(newline)