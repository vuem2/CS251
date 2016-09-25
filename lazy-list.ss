;;; Team Camembert / PEPS Squad
;;; Michael Vue, Simon Orlovsky, Martin Green

;; Construct a lazy list from a to b
(define lazy-range
  (lambda (a b)
    (if (> a b)
        '()
        (cons a
              (lambda () (lazy-range (+ a 1) b))))))

;; Construct a lazy list from a to infinity
(define lazy-infinite-range
  (lambda (a)
    (if (not (number? a))
        '()
    (cons a (lambda () (lazy-infinite-range (+ a 1)))))))

;; Construct a regular list containing the first n values in the lazy list LL
(define first-n
  (lambda (LL n)
    (cond
      [(equal? n 0)
       '()]
      [(null? LL)
       '()]
      [(cons (car LL) (first-n ((cdr LL)) (- n 1)))])))

;; Helper for nth function
(define nth-helper
  (lambda (LL n)
    (cond [(< (length LL) n) #f]
          [(= 1 n) (car LL)]
          [else (nth-helper (cdr LL) (- n 1))])))

;; Compute the nth value in the lazy list LL
(define nth
  (lambda (LL n)
    (nth-helper (first-n LL n) n)))

;; Construct a new lazy list where the elements of LL are true given f
(define filter-lazy-list
  (lambda (f LL)
    (if (null? LL)
        '()
        (if (f (car LL))
            (cons (car LL) (lambda () (filter-lazy-list f ((cdr LL)))))
            (filter-lazy-list f ((cdr LL)))))))

;; Returns true if x is not divisible by y
(define not-divisible?
  (lambda (x)
    (lambda (y)
      (if (= (modulo y x) 0)
          #f
          #t))))

;; Takes lazy list and returns new lazy list whose CDR is not divisible by x
(define remainder-list
  (lambda (L x)
        (if ((not-divisible? x) (car ((cdr L))))
            (cons (car ((cdr L))) (lambda () (remainder-list ((cdr L)) x)))
            (remainder-list ((cdr L)) x))))

;; Increments the x for remainder list
(define sieve-helper
  (lambda (L)
    (if (null? L)
        '()
        (cons (car L) (lambda () (sieve-helper (remainder-list L (car L))))))))

;; Returns lazy list representing all prime numbers starting from two
(define sieve
  (lambda ()
    (sieve-helper (lazy-infinite-range 2))))