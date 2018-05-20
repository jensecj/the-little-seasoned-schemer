;;;;;;;;;;;;;;;;
;; Chapter 11 ;;
;;;;;;;;;;;;;;;;

;; checks whether a is the first element in l
(define (is-first? a l)
  (cond
   [(null? l) #f]
   [else (eq? a (car l))]))

;; are there two equal elements in a row in the list l?
(define (two-in-a-row? l)
  (cond
   [(null? l) #f]
   [else (or (is-first? (car l) (cdr l))
             (two-in-a-row? (cdr l)))]))

(two-in-a-row? '(1 2 3 4))
(two-in-a-row? '(1 2 3 3))

;; the above function keeps searching even if is-first? refurns false
;; because the last part of the list is empty, which is silly, lets
;; fix that. (we'll use mutual recursion)

(define (is-first? a l)
  (cond
   [(null? l) #f]
   [else (or (eq? a (car l))
             (two-in-a-row? (cdr l)))]))
(define (two-in-a-row? l)
  (cond
   [(null? l) #f]
   [else (is-first? (car l) (cdr l))]))

(two-in-a-row? '(1 2 3 4))
(two-in-a-row? '(1 2 3 3))

;; looks like we could probably cook these two into a simpler setup

(define (two-in-a-row?-helper previous l)
  (cond
   [(null? l) #f]
   [else (or (eq? previous (car l))
             (two-in-a-row?-helper (car l) (cdr l)))]))

(define (two-in-a-row? l)
  (cond
   [(null? l) #f]
   [else (two-in-a-row?-helper (car l) (cdr l))]))

(two-in-a-row? '(1 2 3 4))
(two-in-a-row? '(1 2 3 3))

;; given a tup, return a tup with the running sum of the prefixes
(define (sum-of-prefixes-helper acc tup)
  (cond
   [(null? tup) '()]
   ;; we continually cons the running sum on the list, and recur with rest
   [else (cons (+ acc (car tup))
               (sum-of-prefixes-helper (+ acc (car tup))
                                       (cdr tup)))]))

;; we start with 0 tup, because 0 is neutral for +, which makes this
;; our initial case
(define (sum-of-prefixes tup)
  (cond
   [(null? tup) '()]
   [else (sum-of-prefixes-helper 0 tup)]))

(sum-of-prefixes '(1 1 1 1 1))
(sum-of-prefixes '(2 1 9 17 0))

;; create a function that jumps around a bit, given a tuple where each element
;; is not greater than its index, it returns a tuple of the same length, which
;; is constructed by using the elements in the given tuple as 'backwards
;; indices', which jump back from the current position, to the previous elements
;; in the tuple, which becomes the element in the resulting tuple.
;; e.g. (1 1 3 2) -> (1 1 1 3), because one jumps to itself,
;; 3 jumps 3 -> 2 -> 1, and 2 jumps 2 -> 3.

;; we use pick from the little schemer in the next function, this makes
;; it easy to pick an earlier index from our tuple

;; pick the n'th element from lat
(define pick
  (lambda (n lat)
    (cond
     ((null? lat) '())
     ((zero? (sub1 n)) (car lat))
     (else (pick (sub1 n) (cdr lat))))))

;; the main logic of out function
(define (scramble-helper tup rev-pre)
  (cond
   ;; if the tuple is empty, there is nothing left to do
   [(null? tup) '()]
   ;; otherwise we need to construct our new tuple, we do this by
   ;; picking the number at the backwards index from tup (we create
   ;; this 'backwards' counting by creating a reversed prefix list,
   ;; for each prefix we see, we add it to this list, and instead of
   ;; 'picking backwards', we pick forwards in a reversed list)
   [else (cons (pick (car tup)
                     (cons (car tup) rev-pre))
               (scramble-helper (cdr tup)
                                (cons (car tup)
                                      rev-pre)))]))

;; a more legible version, doing the same thing
(define (scramble-helper tup reversed-prefix-list)
  (cond
   [(null? tup) '()]
   [else (let ((current (car tup))
               (remaining (cdr tup))
               (new-reversed-prefix-list (cons (car tup) reversed-prefix-list)))
           (cons (pick current new-reversed-prefix-list)
                 (scramble-helper remaining new-reversed-prefix-list)))]))

;; the initial setup
(define (scramble tup)
  (scramble-helper tup '()))

(scramble '(1 1 3 2))
(scramble '(1 2 3 4 5 6 7 8 9))

