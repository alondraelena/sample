#lang racket
(require rackunit)
(provide remove-all)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; remove-all
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; remove-all
;; inputs: a list and an element to remove
;; output: a new list with said element instances removed

(define (remove-all e L)
  (cond [(empty? L) '()]
        [(equal? (first L) e)  (remove-all e (rest L))]
        [else (cons (first L) (remove-all e (rest L)))]

   ))

; provided tests
(check-equal? (remove-all "i" '("a" "l" "i" "i" "i" "e" "n")) 
              '("a" "l" "e" "n"))
(check-equal? (remove-all "i" '( ("a" "l" "i") "i" "i" "e" "n")) 
              '(("a" "l" "i") "e" "n"))
(check-equal? (remove-all 0 '(1 0 1 0 1 0))  
              '(1 1 1))

; additional tests
(check-equal? (remove-all "a" '("a" "l" "o" "n" "d" "r" "a"))
              '("l" "o" "n" "d" "r"))
(check-equal? (remove-all  2 '(1 2 22 3 4))
                           '(1 22 3 4))