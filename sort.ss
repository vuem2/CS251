;;; Michael Vue, Simon Orlovsky, Martin Green
;;; Problem Set 05
;;; Team Camambert / PEPS Squad

;; Merge two lists that are already in the correct order
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