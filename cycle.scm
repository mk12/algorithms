;;; Determines if a cycle occurs in the given list. If so, returns (mu lambda)
;;; were mu is the index of the beginning of the cycle and lambda is its period.
;;; Returns #f is there is no cycle.
(define (detect-cycle x0)
  (define (race t h)
    (cond ((or (null? t) (null? h) (null? (cdr h))) #f)
          ((eq? t h)
           (list (find-mu t x0)
                 (find-lambda t (cdr h))))
          (else (race (cdr t) (cddr h)))))
  (define (find-mu t h)
    (if (eq? t h)
      0
      (+ 1 (find-mu (cdr t) (cdr h)))))
  (define (find-lambda t h)
    (if (eq? t h)
      1
      (+ 1 (find-lambda t (cdr h)))))
  (if (or (null? x0) (null? (cdr x0)) (null? (cddr x0)))
    #f
    (race (cdr x0) (cddr x0))))
