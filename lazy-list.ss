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

; (first-n (lazy-infinite-range 1) 10)

;; Helper for nth function
(define nth-helper
  (lambda (LL n)
    (cond [(< (length LL) n) #f]
          [(= 1 n) (car LL)]
          [else (nth (cdr LL) (- n 1))])))

;; Compute the nth value in the lazy list LL
(define nth
  (lambda (LL n)
    (nth-helper LL n)))


(define filter-lazy-list
  (lambda (f LL)
    (if (null? LL)
        '()
        (if (f (car LL))
            (cons (car LL) (lambda () (filter-lazy-list f ((cdr LL)))))
            (filter-lazy-list f ((cdr LL)))))))

(define not-divisible?
  (lambda (x)
    (lambda (y)
      (if (= (modulo y x) 0)
          #f
          #t))))

(define remainder-list
  (lambda (LL x)
    (if (null? LL)
        '()
        (if ((not-divisible? (car LL)) x)
            (cons x (lambda () (reminader-list ((cdr LL)) (+ x 1))))
            (lambda () (remainder-list ((cdr LL)) (+ x 1)))))))


(define test-remainder-list
  (lambda (x L)
    (if (null? (cdr L))
        '()
        (if ((not-divisible? x) (cadr L))
            (cons (cadr L) (test-remainder-list x (cdr L)))
            (test-remainder-list x (cdr L))))))

(test-remainder-list 2 '(2 3 4 5 6 7 8 9 10 11 12 13 14))