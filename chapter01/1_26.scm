(load "1_24.scm")

(define (expmod base exp m)
    (format #t "base: ~A, exp: ~A, m: ~A ~%" base exp m)
    (cond ((= exp 0) 1)
          ((even? exp)
            (remainder (* (expmod base (/ exp 2) m)
                          (expmod base (/ exp 2) m))
                        m))
          (else
            (remainder (* base (expmod base (- exp 1) m)) m)
          )
    )
)

(search-for-prime 1000 3)