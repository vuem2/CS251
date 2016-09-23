(define lazy-infinite-range
  (lambda (a)
    (if (not (number? a))
        '()
    (cons a (lambda () (lazy-infinite-range (+ a 1)))))))


(define first-n
  (lambda (LL n)
    (cond
      [(equal? n 0)
       '()]
      [(null? LL)
       '()]
      [(cons (car LL) (first-n ((cdr LL)) (- n 1)))])))
(first-n (lazy-infinite-range 1) 10)