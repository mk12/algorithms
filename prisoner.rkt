#lang racket

;;;;; Payoff matrix constants

;;; Values of the payoffs.
(define reward 2)
(define temptation 3)
(define suckers-payoff 0)
(define punishment 1)

;;; Matrix indices for the two choices.
(define cooperate 1)
(define defect 2)

;;;;; History

;;; The history is a list of moves from newest to oldest. Each move is an object
;;; containing the moves (cooperate or defect) of both players. For example, if
;;; first payoff was the reward and the second was the temptation, then the
;;; history available in the third move would be ((2 1) (1 1)).

;;; The move objects are represented by pairs.
(define make-move list)
(define me car)
(define them cadr)

;;; Swaps "me" and "them" in the move.
(define (swap move)
  (make-move (them move) (me move)))

;;;;; Iterated game

;;; Returns the payoff received by x when playing against y.
(define (payoff move)
  (case move
    [((1 1)) reward]
    [((2 1)) temptation]
    [((1 2)) suckers-payoff]
    [((2 2)) punishment]))

;;; Returns the history generating after a game of n moves between s1 and s2,
;;; used for debugging strategies.
(define (show-game s1 s2 n)
  (define (go turns hist1 hist2)
    (if (zero? turns)
        hist1
        (let ([move (make-move (s1 hist2) (s2 hist2))])
          (go (sub1 turns)
              (cons move hist1)
              (cons (swap move) hist2)))))
  (go n '() '()))

;;; Calculates the K-value for strategies s1 and s2 using n iterations.
(define (calc-k s1 s2 n)
  (define (total turns hist1 hist2 score)
    (if (zero? turns)
        score
        (let ([move (make-move (s1 hist1) (s2 hist2))])
          (total (sub1 turns)
                 (cons move hist1)
                 (cons (swap move) hist2)
                 (+ score (payoff move))))))
  (/ (total n '() '() 0) n))

;;; Calculates the K-value for all possible pairs of strategies (including both
;;; orderings of each pair) and returns the result in a list of lists.
(define (tournament strategies n)
  (map (lambda (s1)
         (map (lambda (s2)
                (calc-k s1 s2 n))
              strategies))
       strategies))

;;; Returns the average K-value for each strategy when it competes against all
;;; the other strategies (including itself). This isn't necessarily a good way
;;; to evaluate a strategy, as it gives equal weight to the K-values obtained in
;;; competition against all others (some of which may be very simple/bad).
(define (average-k strategies n)
  (map (lambda (xs)
         (/ (apply + xs)
            (length xs)))
       (tournament strategies n)))

;;;;; Strategies

;;; Very simple strategies.
(define (nice hist) cooperate)
(define (mean hist) defect)

;;; Alternates between cooperation and defection.
(define (alternate hist)
  (if (even? (length hist))
      cooperate
      defect))

;;; Cooperates on the first move, then copies the opponents previous move.
(define (tit-for-tat hist)
  (if (null? hist)
      cooperate
      (them (car hist))))

;;; Defects after the other player defects twice in a row, otherwise cooperates.
(define (tit-for-two-tats hist)
  (if (or (< (length hist) 2)
          (not (and (= defect (them (car hist)))
                    (= defect (them (cadr hist))))))
      cooperate
      defect))