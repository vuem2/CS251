;; Merge two lists of numbers which are already in increasing order
(define merge
  (lambda (f l1 l2)
    (if (null? l1) l2
        (if (null? l2) l1
            (if (f (car l1) (car l2))
                (cons (car l1) (merge f (cdr l1) l2))
                (cons (car l2) (merge f (cdr l2) l1)))))))

(define merge-sort
  (lambda (f l)
    (letrec ((even-indices
              (lambda (l)
                (if (null? l) '()
                    (if (null? (cdr l)) '()
                        (cons (car (cdr l)) (even-indices (cdr (cdr l))))))))
             (odd-indices
              (lambda (l)
                (if (null? l) '()
                    (if (null? (cdr l))
                        (list (car l))
                        (cons (car l) (odd-indices (cdr (cdr l)))))))))
    (if (null? l) l
        (if (null? (cdr l)) l
            (merge f
                         (merge-sort f (odd-indices l))
                         (merge-sort f (even-indices l))))))))

(define sort
  (lambda (f L)
    (merge-sort f L)))




(sort < '(3 4 5 2 3 8 9 70 34 23 12 3 45 34))

(sort (lambda (x y) (< (string-length x) (string-length y)))
      '("friends" "romans" "countrymen" "lend" "me" "your" "ears"))

(sort string-ci<? '("friends" "romans" "countrymen" "lend" "me" "your" "ears"))

(sort > '(3 5 9 1 125 2 34 1 16 1 14 61 61))