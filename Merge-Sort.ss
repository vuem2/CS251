;;; -----------------------------------------------------------------
;;; Merge two lists of numbers which are already in increasing order

(define merge-lists
  (lambda (f l1 l2)
    (if (null? l1)
        l2
        (if (null? l2)
            l1
            (if (f (car l1) (car l2))
                (cons (car l1) (merge-lists f (cdr l1) l2))
                (cons (car l2) (merge-lists f (cdr l2) l1)))))))

;;; -------------------------------------------------------------------
;;; Given list l, output those tokens of l which are in even positions

(define even-numbers
  (lambda (l)
    (if (null? l)
        '()
        (if (null? (cdr l))
            '()
            (cons (car (cdr l)) (even-numbers (cdr (cdr l))))))))

;;; -------------------------------------------------------------------
;;; Given list l, output those tokens of l which are in odd positions

(define odd-numbers
  (lambda (l)
    (if (null? l)
        '()
        (if (null? (cdr l))
            (list (car l))
            (cons (car l) (odd-numbers (cdr (cdr l))))))))

;;; ---------------------------------------------------------------------
;;; Use the procedures above to create a simple and efficient merge-sort

(define merge-sort
  (lambda (f l)
    (if (null? l)
        l
        (if (null? (cdr l))
            l
            (merge-lists f
                         (merge-sort f (odd-numbers l))
                         (merge-sort f (even-numbers l)))))))

;;; -------------------------------------------------------
;;; examples (the semi-colons are only for commenting)

;(define a (even-numbers '(2 7 6 5 4 5 6 7 4)))
;(7 5 5 7)
;(define b (odd-numbers '(2 7 6 5 4 5 6 7 4)))
;(2 6 4 6 4)
;(define c (merge-sort '(3 4 5 2 3 8 9 70 34 23 12 3 45 34)))
;(2 3 3 3 4 5 8 9 12 23 34 34 45 70)