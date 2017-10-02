;; utillity
;; print some text
(define print
  (lambda (txt)
    (write txt)
    (newline)))

;;;;;;;;;;;;;;;
;; Chapter 1 ;;
;;;;;;;;;;;;;;;

;; test built-in functions
(atom? 'atom)
(atom? 'turkey)
(atom? 1492)
(atom? 'u)
(atom? '*abc$)
(atom? '(a b))
(atom? '())

(null? 'a)
(null? '(a b))
(null? '())

(list? '(atom))
(list? '())

(car '(a b c))
(car '((a b c) x y z))
(car 'hotdog)
(car '())
(caar '((hotdog) (kage)))
(cdr '(could er))

(cons 'peanut '(butter and jelly))

;;;;;;;;;;;;;;;
;; Chapter 2 ;;
;;;;;;;;;;;;;;;

;; define some useful functions

;; is l a list of atoms?
(define lat? ;; list of atoms?
  (lambda (l)
    (cond
     ;; the empty list does not contain any lists, so it is a list of (the empty) atom
     ((null? l) #t)
     ;; recurse over the list and check if all members are atoms
     ((atom? (car l)) (lat? (cdr l)))
     ;; if the above case does not end in the first case, there is
     ;; some non-atom member in the list
     (else #f))))

(lat? '())
(lat? '(a b c d))
(lat? '(a (b c) d))

;; is the atom a, a member of the list of atoms lat
(define member?
  (lambda (a lat)
    (cond
     ;; if we have gone through all elements and not found a, it is
     ;; not a member (or if the list is empty)
     ((null? lat) #f)
     ;; look at each element in the list and compare with a
     (else (or (eq? a (car lat))
               (member? a (cdr lat)))))))

(member? 'a '(a b c))
(member? 'a '(x y z))

;;;;;;;;;;;;;;;
;; Chapter 3 ;;
;;;;;;;;;;;;;;;

;; remove a member from a list of atoms
(define rember
  (lambda (a lat)
    (cond
     ;; if the list is empty, a is not a member
     ((null? lat) '())
     ;; if car is the element we want to remove, just return
     ;; the rest of the list
     ((eq? a (car lat)) (cdr lat))
     (else
      ;; otherwise save this element, and move to the next ones
      (cons (car lat) (rember a (cdr lat)))))))

(rember 'a '(a b c))
(rember 'y '(x y z))
(rember 'a '(a b a c))

;; grab the first member of each list
(define firsts
  (lambda (l)
    (cond
     ;; if the list is empty, there is no element to grab
     ((null? l) '())
     ;; grab the first element from this list (it is a list of lists)
     ;; and then recur on the rest of the lists to grab their as well
     (else (cons (caar l) (firsts (cdr l)))))))

(firsts '((a b) (c d) (e f)))

;; insert atom new to the right of atom old, in list of atoms lat
(define insertR
  (lambda (new old lat)
    (cond
     ;; if the list is empty, we cannot insert anything
     ((null? lat) '())
     ;; if this element is where we need to insert, create a list with
     ;; the old element to the left of the one we need to insert, and
     ;; the rest of the list after
     ((eq? old (car lat)) (cons old (cons new (cdr lat))))
     ;; otherwise keep looking for the element we need to insert after
     (else (cons (car lat) (insertR new old (cdr lat)))))))

(insertR 'b 'a '(a c))
(insertR 'b 'a '(x y z))
(insertR 'b 'a '(x y z a))

;; insert atom new to the right of atom old, in list of atoms lat
;; same implementation as above, except we insert before an element,
;; and not after
(define insertL
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ;; can reduce this, because (cons new lat) would be the same
     ;; as (cons old (cdr lat)) when (eq? old (car lat))
     ((eq? old (car lat)) (cons new lat))
     (else (cons (car lat) (insertL new old (cdr lat)))))))

(insertL 'b 'a '(a c))
(insertL 'b 'a '(x y z))
(insertL 'b 'a '(x y z a))

;; substitude the first occurence of old in lat with new
(define subst
  (lambda (old new lat)
    (cond
     ;; if the list is empty there is nothing to substitude
     ((null? lat) '())
     ;; if this element is the one we want to substitude, do it
     ((eq? old (car lat)) (cons new (cdr lat)))
     ;; otherwise keep looking for the element
     (else (cons (car lat) (subst old new (cdr lat)))))))

(subst 'a 'x '(a b c a))
(subst 'a 'o '(x y z))
(subst 'b 'x '(a b c))

;; substitude either the first occurence of o1 or of o2 with new in lat
(define subst2
  (lambda (new o1 o2 lat)
    (cond
     ;; if the list is empty, there is nothing to replace
     ((null? lat) '())
     ;; if this element is either o1 or o2, replace it
     ((or (eq? (car lat) o1)
          (eq? (car lat) o2)) (cons new (cdr lat)))
     ;; otherwise keep looking through the list
     (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))

(subst2 'new 'o1 'o2 '(a b o1 o2 c))
(subst2 'o 'x 'y '(a b c x y x))

;; remove all occurances of a in lat
(define multirember
  (lambda (a lat)
    (cond
     ;; if the list is empty, there is nothing to remove
     ((null? lat) '())
     ((eq? a (car lat)) (multirember a (cdr lat)))
     (else (cons (car lat) (multirember a (cdr lat)))))))

(multirember 'a '(a b a c))
(multirember 'a '(x y z))

;; insert new to the right of all occurences of old in lat
(define multiinsertR
  (lambda (new old lat)
    (cond
     ;; if the list is empty, we do not have to insert anything
     ((null? lat) '())
     ;; if this is the element we are looking for, insert to the right
     ;; of it, and recur through the rest of the list
     ((eq? old (car lat)) (cons old (cons new (multiinsertR new old (cdr lat)))))
     (else (cons (car lat) (multiinsertR new old (cdr lat)))))))

(multiinsertR 'new 'old '(a b c old d e old f))

;; insert new to the left of all occurences of old in lat
(define multiinsertL
  (lambda (new old lat)
    (cond
     ;; if the list is empty, we do not have to insert anything
     ((null? lat) '())
     ;; if this is the element we are looking for, insert to the left
     ;; of it, and recur through the rest of the list
     ((eq? old (car lat)) (cons new (cons old (multiinsertL new old (cdr lat)))))
     (else (cons (car lat) (multiinsertL new old (cdr lat)))))))

(multiinsertL 'new 'old '(a b c old d e old f))

;; substitude all occurences of old with new in lat
(define multisubst
  (lambda (old new lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) old) (cons new (multisubst old new (cdr lat))))
     (else (cons (car lat) (multisubst old new (cdr lat)))))))

(multisubst 'a 'x '(a b c d a f g a))

;;;;;;;;;;;;;;;
;; Chapter 4 ;;
;;;;;;;;;;;;;;;

;; math axioms

(define zero?
  (lambda (x)
    (= x 0)))

(zero? 1)
(zero? 0)

(define add1
  (lambda (x)
    (+ x 1)))

(add1 0)
(add1 15)

(define sub1
  (lambda (x)
    (- x 1)))

(sub1 1)
(sub1 15)

;; some math (we only consider positive whole numbers)

(define add ;; add 1 to n, m times
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (add1 (add n (sub1 m)))))))

(add 2 3)

(define sub ;; sub 1 from n, m times
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (sub1 (sub n (sub1 m)))))))

(sub 4 2)

;; is l a tuple? (list of only numbers)
(define tup?
  (lambda (l)
    (cond
     ((null? l) #t)
     ((number? (car l)) (tup? (cdr l)))
     (else #f))))

(tup? '(1 2 3 5))
(tup? '(1 2 3 a))

;; sum the numbers in a tuple
(define addtup
  (lambda (l)
    (cond
     ((null? l) 0)
     (else (add (car l) (addtup (cdr l)))))))

(addtup '(1 2 3 10))

;; numtiply two numbers
(define mul
  (lambda (n m)
    (cond
     ;; if we multiply with 0, we get 0
     ((zero? m) 0)
     ;; add n to 0, m times
     (else (add n (mul n (sub1 m)))))))

(mul 3 4)
(mul 30 0)

;; add the piecewise elements of two tuples (of the same size)
(define tup+
  (lambda (t1 t2)
    (cond
     ((null? t1) t2)
     ((null? t2) t1)
     (else (cons (add (car t1) (car t2)) (tup+ (cdr t1) (cdr t2)))))))

(tup+ '(3 7) '(4 6))
(tup+ '(3) '(3 10))
(tup+ '(3 9) '(10))

;; is a greater than b?
(define gt?
  (lambda (a b)
    (cond
     ;; we do not need this line because the order of the next two
     ;; lines enforce this property, if a is zero this time, then we
     ;; answered true to b not being zero last time, so a reached 0
     ;; before b, and is thus not bigger. vice versa, if b reaches
     ;; zero this time, we answered true to a not being zero, and thus
     ;; is must be bigger since we sub 1 from each at each iteration
     ;; ((and (zero? a) (zero? b)) #f)
     ((zero? a) #f)
     ((zero? b) #t)
     (else (gt? (sub1 a) (sub1 b))))))

(gt? 1 2)
(gt? 2 2)
(gt? 3 2)

;; is a less than b?
(define lt?
  (lambda (a b)
    (cond
     ;; the order is also important here, since we sub 1 from each at
     ;; each iteration, we check if b reaches zero first, if it does
     ;; then a is not smaller than b. vice versa, if a reaches 0
     ;; before b, it is less than b
     ((zero? b) #f)
     ((zero? a) #t)
     (else (lt? (sub1 a) (sub1 b))))))

(lt? 1 2)
(lt? 2 2)
(lt? 3 2)

;; are n and m equal numbers?
(define ieq?
  (lambda (n m)
    (cond
     ;; if both are zero, they're equal
     ((and (zero? n) (zero? m)) #t)
     ;; if only one is zero, they're not equal
     ((or (zero? n) (zero? m)) #f)
     ;; subtracting one from both and recheck recursively
     (else (ieq? (sub1 n) (sub1 m))))))

(ieq? 1 1)
(ieq? 1 2)
(ieq? 3 2)

;; a rewrite of ieq using lt? and gt?
(define ieq-using-lt-gt?
  (lambda (a b)
    (cond
     ((lt? a b) #f)
     ((gt? a b) #f)
     (else #t))))

(ieq-using-lt-gt? 1 1)
(ieq-using-lt-gt? 1 2)
(ieq-using-lt-gt? 3 1)

;; calculate n to the power of m
(define pow
  (lambda (n m)
    (cond
     ;; any number to the power of 0 is 1
     ((zero? m) 1)
     ;; multiply n with n, m times
     (else (mul n (pow n (sub1 m)))))))

(pow 2 8)
(pow 13 0)
(pow 0 24)

;; divide n with m (integer division)
(define div
  (lambda (n m)
    (cond
     ((or (zero? m) (zero? n)) 0)
     ((lt? n m) 0)
     (else (add1 (div (sub n m) m))))))

(div 16 2)
(div 0 2)
(div 16 0)

;; extra: what is n modulo m?
(define mod
  (lambda (n m)
    (cond
     ((or (zero? n) (zero? m)) 0)
     ((lt? n m) n)
     (else (mod (sub n m) m)))))

(mod 15 4)
(mod 10 2)
(mod 10 11)

;; what is the length of a list lat?
(define length
  (lambda (lat)
    (cond
     ((null? lat) 0)
     (else (add1 (length (cdr lat)))))))

(length '())
(length '(1 2 3))
(length '(a b c d))

;; pick the n'th element from lat
(define pick
  (lambda (n lat)
    (cond
     ((null? lat) '())
     ((zero? (sub1 n)) (car lat))
     (else (pick (sub1 n) (cdr lat))))))

(pick 2 '(a b c d))
(pick 4 '(a b c d))
(pick 0 '(a b c d))

;; remove the n'th element from lat
(define rempick
  (lambda (n lat)
    (cond
     ((null? lat) '())
     ((zero? (sub1 n)) (cdr lat))
     (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(rempick 3 '(1 2 3 4))
(rempick 2 '(1 2 3 4))
(rempick 0 '(1 2 3 4))

;; prune all numbers from a lat
(define no-nums
  (lambda (lat)
    (cond
     ((null? lat) '())
     ((number? (car lat)) (no-nums (cdr lat)))
     (else (cons (car lat) (no-nums (cdr lat)))))))

(no-nums '(a b c d))
(no-nums '(1 a b 2 c d))
(no-nums '(1 2 3 4))

;; extract all the numbers from a lat
(define all-nums
  (lambda (lat)
    (cond
     ((null? lat) '())
     ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
     (else (all-nums (cdr lat))))))

(all-nums '(a b c d))
(all-nums '(1 a b 2 c d))
(all-nums '(1 2 3 4))

;; are a1 and a2 equal, be they numbers or atoms
(define eqan?
  (lambda (a1 a2)
    (cond
     ;; if both are numbers, then use =
     ((and (number? a1) (number? a2)) (= a1 a2))
     ;; if only one of them are numbers, then they are not equal
     ((or (number? a1) (number? a2)) #f)
     ;; if both of them are atom, then use eq?
     (else (eq? a1 a2)))))

(eqan? 1 1)
(eqan? 1 2)
(eqan? 1 'cake)
(eqan? 'cake 'cake)
(eqan? 'bake 'cake)

;; count how many times an atom occurs in a list
(define occur
  (lambda (a lat)
    (cond
     ((null? lat) 0)
     ((eq? a (car lat)) (add1 (occur a (cdr lat))))
     (else (occur a (cdr lat))))))

(occur 1 '(1 2 3))
(occur 1 '(1 2 1 3 1))

;; is a = 1?
(define one?
  (lambda (a)
    (cond
     ((zero? a) #f)
     (else (zero? (sub1 a))))))

(one? 1)
(one? 2)

;; here we could rewrite rempick to use one? instead of using zero?

;;;;;;;;;;;;;;;
;; Chapter 5 ;;
;;;;;;;;;;;;;;;

;; remove all occurences of atom a in list (maybe with sublists)
(define rember*
  (lambda (a l)
    (cond
     ;; if the list is empty, there is nothing to remove
     ((null? l) '())
     ;; otherwise there is an element to look at, is this element an atom?
     ((atom? (car l))
      (cond
       ;; if it is, is it one we should remove? if so, we just recur
       ;; on the rest of the list, and leave this element out
       ((eq? a (car l)) (rember* a (cdr l)))
       ;; otherwise we keep the element, and recur on the rest of the list
       (else (cons (car l) (rember* a (cdr l))))))
     ;; otherwise it is a list, and we should recur on both car and cdr
     (else (cons
            (rember* a (car l))
            (rember* a (cdr l)))))))

(rember* '2 '(a b (1 2 a (3 a)) f))

;; insert new to the right of old in all places in the list
(define insertR*
  (lambda (new old l)
    (cond
     ;; if the list is empty, there is no where to insert
     ((null? l) '())
     ;; is the front element an atom?
     ((atom? (car l))
      (cond
       ;; if the current element is the one we need to insert after, do that
       ((eq? (car l) old) (cons old (cons new (insertR* new old (cdr l)))))
       ;; if not, then we just cons, and recur
       (else (cons (car l) (insertR* new old (cdr l))))))
     ;; otherwise the front element is not an atom, so we need to recurse on both lists
     (else (cons
            (insertR* new old (car l))
            (insertR* new old (cdr l)))))))

(insertR* 'b 'a '(a c d (a f)))
(insertR* '1 '0 '(0 0 0 0 (0 0 0 (0 0))))

;; count how many times an atom occurs in a list
(define occur*
  (lambda (a l)
    (cond
     ;; if the list is empty, there can be no more occurences
     ((null? l) 0)
     ;; is this element an atom?
     ((atom? (car l))
      (cond
       ;; if it is, is it the one we are looking for?
       ;; if so, then we count up once
       ((eq? (car l) a) (add1 (occur* a (cdr l))))
       ;; otherwise we keep looking
       (else (occur* a (cdr l)))))
     ;; if it was not an atom, it must be a list, so we need to count both sublists
     (else (add
            (occur* a (car l))
            (occur* a (cdr l)))))))

(occur* 'a '(a b c (a b d (a a) a)))
(occur* 'a '(b c d e))

;; substitude old with new for all elements in the list
(define subst*
  (lambda (old new l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((eq? (car l) old) (cons new (subst* old new (cdr l))))
       (else (cons (car l) (subst* old new (cdr l))))))
     (else (cons (subst* old new (car l)) (subst* old new (cdr l)))))))

(subst* 'bage 'kage '(bage mand (bage ff)))

;; insert new to the left of old in all places in the list
(define insertL*
  (lambda (new old l)
    (cond
     ;; if the list is empty, there is no where to insert
     ((null? l) '())
     ;; is the front element an atom?
     ((atom? (car l))
      (cond
       ;; if the current element is the one we need to insert after, do that
       ((eq? (car l) old) (cons new (cons old (insertL* new old (cdr l)))))
       ;; if not, then we just cons, and recur
       (else (cons (car l) (insertL* new old (cdr l))))))
     ;; otherwise the front element is not an atom, so we need to recurse on both lists
     (else (cons
            (insertL* new old (car l))
            (insertL* new old (cdr l)))))))

(insertL* 'b 'a '(a c d (a f)))
(insertL* '1 '0 '(0 0 0 0 (0 0 0 (0 0))))

;; is a a member of the list l?
(define member*
  (lambda (a l)
    (cond
     ((null? l) #f)
     ((atom? (car l))
      (cond
       ((eq? (car l) a) #t)
       (else (member* a (cdr l)))))
     (else (or (member* a (car l)) (member* a (cdr l)))))))

(member* 'a '(a b c d))
(member* 'a '(b c d (a b c)))
(member* 'a '(b c d (b c (1 2 3))))

;; get the left most atom in a list
(define leftmost
  (lambda (l)
    (cond
     ((null? l) '())
     ((atom? (car l)) (car l))
     (else (leftmost (car l))))))

(leftmost '(a b c d))
(leftmost '((a b) c d))
(leftmost '(((1) 2) c d))
(leftmost '((() 2) c d))

(define my-and
  (lambda (a b)
    (cond
     (a b)
     (else #f))))

(my-and #t #t)
(my-and #t #f)
(my-and #f #t)
(my-and #f #f)

(define my-or
  (lambda (a b)
    (cond
     (a #t)
     (else b))))

(my-or #t #t)
(my-or #t #f)
(my-or #f #t)
(my-or #f #f)

;; determine if two lists are equal
(define eqlist?
  (lambda (l1 l2)
    (cond
     ;; if both lists are empty, they're equal
     ((and (null? l1) (null? l2)) #t)
     ;; if only one of them is empty, they cant be equal
     ((or (null? l1) (null? l2)) #f)
     ;; here we know that at least one of the lists have an element
     ((and (atom? (car l1)) (null? l2)) #f)
     ;; if both lists have an atom at this element, we need to investigate further
     ((and (atom? (car l1)) (atom? (car l2)))
      (and (eqan? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))
     ;; if the first list has an element here (and the second list does not)
     ((atom? (car l1)) #f)
     ;; if the
     ((null? l2) #f)
     ;;
     ((atom? (car l2)) #f)
     ;;
     (else
      (and
       (eqlist? (car l1) (car l2))
       (eqlist? (cdr l1) (cdr l2)))))))

(eqlist? '() '())
(eqlist? '(a b c) '(a b c))
(eqlist? '(a (b c (d)) e) '(a (b c (d)) e))
(eqlist? '(a (b c (d)) e) '(a (b c (99)) e))

;; determine if two sexps are equal
(define equal?
  (lambda (s1 s2)
    (cond
     ;; if both sexps are atoms, check if they're equal
     ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
     ;; if one is an atom, and one is not, they cant be equal
     ((or (atom? s1) (atom? s2)) #f)
     ;; otherwise both are lists, and we use eqlist?
     (else (eqlist? s1 s2)))))

(equal? 'a 'a)
(equal? '(a b (c)) '(a b (c)))
(equal? '(a b (c)) '())

;; rewrite of eqlist? using equal?
;; notice that here, we're using mutual recursion!
(define eqlist?
  (lambda (l1 l2)
    (cond
     ;; if both lists are empty, they're equal
     ((and (null? l1) (null? l2)) #t)
     ;; if only one of them is empty, they cant be equal
     ((or (null? l1) (null? l2)) #f)
     ;; otherwise both lists have elements, and we check if they're equal
     (else (and (equal? (car l1) (car l2))
                (equal? (cdr l1) (cdr l2)))))))

(eqlist? '() '())
(eqlist? '(a b c) '(a b c))
(eqlist? '(a (b c (d)) e) '(a (b c (d)) e))
(eqlist? '(a (b c (d)) e) '(a (b c (99)) e))

;; rewrite of rember, where s is an sexp, and l is a list of sexps,
;; here the idea is to replace the first occurence of the sexp s, inside of l
(define rember
  (lambda (s l)
    (cond
     ((null? l) '())
     ((equal? (car l) s) (cdr l))
     (else (cons (car l) (rember s (cdr l)))))))

(rember '(1 2) '(a b (1 2) (e f) g))

;;;;;;;;;;;;;;;
;; Chapter 6 ;;
;;;;;;;;;;;;;;;

;; is this arithmetic expression only consisting of numbers?
(define numbered?
  (lambda (aexp)
    (cond
     ((atom? aexp) (number? aexp))
     (else (and (numbered? (car aexp))
                (numbered? (caddr aexp)))))))

(numbered? '1)
(numbered? 'e)
(numbered? '(1 + 3 * 4))
(numbered? '(1 + x * 4))

;; this is correct, because we assume that the given argument is an
;; arithmetic expression, for nonsense input, we get nonsense output
(numbered? '(1 : 3 * 4))

;; figure out the numbered value of a numbered arithmetic expression
;; notice, here we are using in-fix notation for the nexps
(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? '+ (cadr nexp))
      (add (value (car nexp)) (value (caddr nexp))))
     ((eq? '* (cadr nexp))
      (mul (value (car nexp)) (value (caddr nexp))))
     ((eq? '^ (cadr nexp))
      (pow (value (car nexp)) (value (caddr nexp)))))))

(value '3)
(value '(1 + 2))
(value '(((2 * 2) ^ 2) + 4))

;; here we remake value with prefix notation (e.g. (+ 1 2))
(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? '+ (car nexp))
      (add (value (cadr nexp))
           (value (caddr nexp))))
     ((eq? '* (car nexp))
      (mul (value (cadr nexp))
           (value (caddr nexp))))
     ((eq? '^ (car nexp))
      (pow (value (cadr nexp))
           (value (caddr nexp)))))))

(value '3)
(value '(+ 1 2))
(value '(+ 1 (* 2 2)))
(value '(+ 1 (* 3 (^ 2 2))))

(define 1st-sub-exp
  (lambda (e)
    (cadr e)))

(define 2nd-sub-exp
  (lambda (e)
    (caddr e)))

(define operator
  (lambda (e)
    (car e)))

;; rewriting value using out helpers
(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? '+ (operator nexp))
      (add (value (1st-sub-exp nexp))
           (value (2nd-sub-exp nexp))))
     ((eq? '* (operator nexp))
      (mul (value (1st-sub-exp nexp))
           (value (2nd-sub-exp nexp))))
     ((eq? '^ (operator nexp))
      (pow (value (1st-sub-exp nexp))
           (value (2nd-sub-exp nexp)))))))

(value '3)
(value '(+ 1 2))
(value '(+ 1 (* 2 2)))
(value '(+ 1 (* 3 (^ 2 2))))

;; using the helpers, now we can easily change which type of fix wére
;; using (pre,in, or post), simply by changing how theýre defined

;; playing around with number representations

(define sero?
  (lambda (n)
    (null? n)))

(sero? '())
(sero? '(() ()) )

(define edd1
  (lambda (n)
    (cons '() n)))

(edd1 '())
(edd1 '(()))

(define zub1
  (lambda (n)
    (cdr n)))

(zub1 '(()))
(zub1 '(()()))

(sero? (zub1 (edd1 '() )))

(define edd
  (lambda (a b)
    (cond
     ((null? a) b)
     (else (edd1 (edd (zub1 a) b))))))

(edd '(()()) '(()()))

;;;;;;;;;;;;;;;
;; Chapter 7 ;;
;;;;;;;;;;;;;;;

;; is the list s a set?
(define set?
  (lambda (s)
    (cond
     ;; if the list is empty, then no element occurs more than once
     ((null? s) #t)
     ;; otherwise, it has atleast one element, check if that element
     ;; occurs in the rest of the list, if it does, it is not a set
     ((member? (car s) (cdr s)) #f)
     ;; if it does not, the check for the rest of the elements in the list
     (else (set? (cdr s))))))

(set? '())
(set? '(a b 1 c))
(set? '(a b c a))

;;  take a list, and make it a set
;; we do this thus: we look at all elements in the list in order, when
;; we find that one occurs more than once, we skip in in the cons,
;; which results in the the last occurence of the element in the list
;; being the one that stays
(define makeset
  (lambda (l)
    (cond
     ((null? l) '())
     ((member? (car l) (cdr l))
      (makeset (cdr l)))
     (else (cons (car l) (makeset (cdr l)))))))

(makeset '(1 2 3 2 4 5 1 6))

;; we can also make makeset keep the first occurence of the element,
;; we do this using multirember
(define makeset
  (lambda (l)
    (cond
     ((null? l) '())
     (else (cons (car l) (makeset (multirember (car l) (cdr l))))))))

(makeset '(1 2 3 2 4 5 1 6))

;; is s1 a subset of s2?
(define subset?
  (lambda (s1 s2)
    (cond
     ;; if the first set is empty, then it is a subset of s2 (only has
     ;; the empty element)
     ((null? s1) #t)
     ;; if the first element of s1 is in s2, then check if the rest of
     ;; the elements in s1 are in s2
     ((member? (car s1) s2) (subset? (cdr s1) s2))
     ;; otherwise, if the element is not in s2, then this is not a subset
     (else #f))))

(subset? '(1 2) '(1 2 3 4))
(subset? '(1 2 5) '(1 2 3 4))

;; are the two sets equal?
(define eqset?
  (lambda (s1 s2)
    (and (subset? s1 s2) (subset? s2 s1))))

(eqset? '(1 2 3) '(3 2 1))
(eqset? '(1 2 3 4) '(3 2 1))

;; do the two sets s1,s2 intersect? (have atleast one element in
;; common, excluding the empty element)
(define intersect?
  (lambda (s1 s2)
    (cond
     ((null? s1) #f)
     (else (or (member? (car s1) s2)
               (intersect? (cdr s1) s2))))))

(intersect? '(1 2 3) '(3 4 5))
(intersect? '(1 2 3) '(4 5 6))

;; find the elements that intersect between two sets
(define intersect
  (lambda (s1 s2)
    (cond
     ;; if the first set is empty, there are not intersecting elements
     ((null? s1) '())
     ;; if the first element in set1 is in set2, then that element
     ;; intersects the two sets, so add it and recur
     ((member? (car s1) s2)
      (cons (car s1) (intersect (cdr s1) s2)))
     ;; otherwise check the rest of the elements
     (else
      (intersect (cdr s1) s2)))))

(intersect '(1 2 3) '(2 3 4))
(intersect '(1 2 3) '(4 5 6))

;; find the union of two sets
(define union
  (lambda (s1 s2)
    (cond
     ;; if the first set in empty, the union is simply the second set
     ((null? s1) s2)
     ;; if the first element in the first set is in the second set, we
     ;; just skip it for now
     ((member? (car s1) s2)
      (union (cdr s1) s2))
     ;; otherwise we have reached an element in the first set that is
     ;; not in the second, so we cons it and recur
     (else (cons (car s1) (union (cdr s1) s2))))))

(union '(1 2 3) '(3 4 5))
(union '(1 2) '(4 5))

;; find all elements that are in set1, but not in set2
(define set-diff-1
  (lambda (s1 s2)
    (cond
     ((null? s1) '())
     ((member? (car s1) s2)
      (set-diff-1 (cdr s1) s2))
     (else
      (cons (car s1) (set-diff-1 (cdr s1) s2))))))

(set-diff-1 '(1 2 3) '(3 4 5))

;; find all elements the sets do not have in common
(define set-diff
  (lambda (s1 s2)
    (union (set-diff-1 s1 s2)
           (set-diff-1 s2 s1))))

(set-diff '(1 2 3) '(3 4 5))

;; find the elements that intersect all sets in a list of sets
(define intersectall
  (lambda (l)
    (cond
     ;; if there is only one set, it has all its elements in common
     ;; with it self
     ((null? (cdr l)) (car l))
     (else
      ;; otherwise, we create a chain on interactions, from the first
      ;; onwards
      (intersect (car l) (intersectall (cdr l)))))))

(intersectall '((1 2 3) (1 2 3 4 5) (5 4 3 2 1)))

;; is x a pair of two sexps?
(define a-pair?
  (lambda (x)
    (cond
     ;; if x is an atom, it is not a pair
     ((atom? x) #f)
     ;; if x is null, it is not a pair
     ((null? x) #f)
     ;; if the cdr of x is empty, it is not a pair
     ((null? (cdr x)) #f)
     ;; otherwise, if the cdr of cdr of x is empty, it is a pair
     ((null? (cddr x)) #t)
     ;; if it has any other form, it is not a pair
     (else #f))))

(a-pair? '(1))
(a-pair? '(1 2))
(a-pair? '(1 (a)))
(a-pair? '(1 2 3))

;; get the first element of a pair
(define first
  (lambda (p)
    (car p)))

;; get the second element of a pair
(define second
  (lambda (p)
    (cadr p)))

;; get the third element of a list
(define third
  (lambda (p)
    (caddr p)))
;; etc.

;; build a pair from two sexps
(define build
  (lambda (a b)
    (cons a (cons b '()))))

(build 'a 'b)
(first (build 'a 'b))
(second (build 'a 'b))

;; does this set define a function? (a mapping from input to output)
(define fun?
  (lambda (rel)
    ;; we can only have one output per input, output can be duplicates though
    (set? (firsts rel))))

(fun? '((1 a) (2 b) (3 c)))
(fun? '((1 a) (2 b) (2 c)))

;; reverse a relation (mapping from one thing to another)
(define revrel
  (lambda (rel)
    (cond
     ;; if the relation is empty, there is nothing to reverse
     ((null? rel) '())
     ;; otherwise we take the elements of car, and flip them, then recur
     (else (cons (build (second (car rel))
                        (first (car rel)))
                 (revrel (cdr rel)))))))

(revrel '((1 a) (2 b) (2 c)))

;; reverse the two components of a pair
(define revpair
  (lambda (p)
    (build (second p) (first p))))

(revpair '(a b))

;; rewrite of revrel using revpair
(define revrel
  (lambda (rel)
    (cond
     ;; if the relation is empty, there is nothing to reverse
     ((null? rel) '())
     ;; otherwise we take the elements of car, and flip them, then recur
     (else (cons (revpair (car rel))
                 (revrel (cdr rel)))))))

(revrel '((1 a) (2 b) (2 c)))

;; is the given function full?
(define fullfun?
  (lambda (fun)
    (fun? (revrel fun))))

(fullfun? '((1 a) (2 b) (3 c)))
(fullfun? '((1 a) (2 b) (3 b)))

;;;;;;;;;;;;;;;
;; Chapter 8 ;;
;;;;;;;;;;;;;;;

;; rember taking a function to test equality (instead of hardcoding eq/equal)
(define (rember-f test? a l)
  (cond
   ;; if the list is empty, there is nothing to remove
   ((null? l) '())
   ((test? a (car l)) (cdr l))
   (else (cons (car l) (rember-f test? a (cdr l))))))

(rember-f = 5 '(6 2 5 3))
(rember-f eq? 'jelly '(jelly beans are good))
(rember-f equal? '(pop corn) '(lemonade (pop corn) and (cake)))

;; same as above, but using function currying
(define (rember-c test?)
  (lambda (a l)
    (cond
     ((null? l) '())
     ((test? a (car l)) (cdr l))
     (else (cons (car l) ((rember-c test?) a (cdr l)))))))

(define rember-= (rember-c =))
(rember-= 5 '(6 2 5 3))
(define rember-eq? (rember-c eq?))
(rember-eq? 'jelly '(jelly beans are good))
(define rember-equal? (rember-c equal?))
(rember-equal? '(pop corn) '(lemonade (pop corn) and (cake)))

;; rewrite of insertL to use currying
(define (insertL-c test?)
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((test? old (car l)) (cons new (cons old (cdr l))))
     (else (cons (car l) ((insertL-c test?) new old (cdr l)))))))

((insertL-c eq?) '1 '0 '(0 0 0))

;; rewrite of insertR to use currying
(define (insertR-c test?)
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((test? old (car l)) (cons old (cons new (cdr l))) )
     (else (cons (car l) ((insertR-c test?) new old (cdr l)))))))

((insertR-c eq?) '1 '0 '(0 0 0))

;; sequences for consing an atom to the left of right of an element in a list
(define (seqL new old l)
  (cons new (cons old l)))

(define (seqR new old l)
  (cons old (cons new l)))

;; insert an element into a list using seqR/seqL strategy
(define (insert-g seq)
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((eq? old (car l)) (seq new old (cdr l)))
     (else (cons (car l) ((insert-g seq) new old (cdr l)))))))

(define insert-g-left (insert-g seqL))
(insert-g-left '1 '0 '(0 0 0))

(define insert-g-right (insert-g seqR))
(insert-g-right '1 '0 '(0 0 0))

;; magical rewrite of subst using insert-g
;; when substituting we just keep the new element
(define (seqS new old l)
  (cons new l))

(define subst (insert-g seqS))

(subst '1 '0 '(0 0 0))

;; magical rewrite of rember
;; when removing, we dont keep the member we find
(define (seqRem new old l)
  l)

(define rember (insert-g seqRem))

;; the first argument plays no role
(rember #f '2 '(1 2 3))

;; lets use currying to rewrite the value function from earlier
;; we abstract the alike parts out from the function, and make a helper
;; takes an atom (action), and returns the corresponding function (+,*,^)
(define (atom-to-function x)
  (cond
   ((eq? x '+) add)
   ((eq? x '*) mul)
   (else pow)))

;; rewrite of value
(define (value nexp)
  (cond
   ((atom? nexp) nexp)
   (else ((atom-to-function (operator nexp))
          (value (1st-sub-exp nexp))
          (value (2nd-sub-exp nexp))))))

(value '3)
(value '(+ 1 2))
(value '(+ 1 (* 2 2)))
(value '(+ 1 (* 3 (^ 2 2))))

;; rewrite multirember to use currying
(define (multirember-f test?)
  (lambda (a l)
    (cond
     ((null? l) '())
     ((test? a (car l)) ((multirember-f test?) a (cdr l)))
     (else (cons (car l) ((multirember-f test?) a (cdr l)))))))

(define multirember-eq? (multirember-f eq?))

(multirember-eq? 'tuna '(tuna sandwich and tuna casserole))

;; lets combine the test and what wére looking for
(define (multiremberT test? l)
  (cond
   ((null? l) '())
   ((test? (car l)) (multiremberT test? (cdr l)))
   (else (cons (car l) (multiremberT test? (cdr l))))))

(define (eq-tuna? x)
  (eq? x 'tuna))

(multiremberT eq-tuna? '(tuna sandwich and tuna casserole))

;; multirember with collectors, this looks at every element and if
;; they are equal to a, it collects them in the second list, otherwise
;; it collects them in the first list
(define (multirember&co a l co)
  (cond
   ;; if the list is empty, there are no elements to remove, and we collect nothing
   ((null? l) (co '() '()))
   ;; it the front element is the one we are looking for, we collect it in the second
   ;; list, and continue on recurring
   ((eq? a (car l)) (multirember&co a
                                    (cdr l)
                                    (lambda (newl seen)
                                      (co newl (cons (car l) seen)))))
   ;; if the element is not what we were looking for, we collect it in the first list
   ;; and continue recurring
   (else (multirember&co a (cdr l)
                         (lambda (newl seen)
                           (co (cons (car l) newl) seen))))))

;; checks of the second of the two arguments is empty, in this case
;; that would be if no elements have been removed, e.g. no equal
;; elements have been collected
(define (a-friend x y)
  (null? y))

(multirember&co 'tuna '(strawberries tuna and swordfish) a-friend)
(multirember&co 'tuna '() a-friend)
(multirember&co 'tuna '(tuna) a-friend)

;; tells us how many elements that are in the first list, in this case
;; that would be how many elements remain after we have removed equals
(define (last-friend x y)
  (length x))

(multirember&co 'tuna '(strawberries tuna and swordfish) last-friend)
(multirember&co 'tuna '() last-friend)
(multirember&co 'tuna '(tuna) last-friend)

;; multiinsert both on left and right
(define (multiinsertLR new oldL oldR l)
  (cond
   ((null? l) '())
   ((eq? oldL (car l)) (cons new (cons oldL (multiinsertLR new oldL oldR (cdr l)))))
   ((eq? oldR (car l)) (cons oldR (cons new (multiinsertLR new oldL oldR (cdr l)))))
   (else (cons (car l) (multiinsertLR new oldL oldR (cdr l))))))

(multiinsertLR 'n 'L 'R '(L L R R R L))

;; multinsertLR with a collector (which collects number of left / right insertions)
(define (multiinsertLR&co new oldL oldR l co)
  (cond
   ;; if the list is empty, we insert nothing to either left or right
   ((null? l) (co '() 0 0))
   ;; if the front element is the one we need to insert to the left of,
   ;; we insert in the newlat, count the insert, and continue
   ((eq? oldL (car l)) (multiinsertLR&co new oldL oldR (cdr l)
                                         (lambda (newlat li ri)
                                           (co (cons new (cons oldL newlat))
                                               (add1 li)
                                               ri))))
   ;; like wise if it is for the right insert case
   ((eq? oldR (car l)) (multiinsertLR&co new oldL oldR (cdr l)
                                         (lambda (newlat li ri)
                                           (co (cons oldR (cons new newlat))
                                               li
                                               (add1 ri)))))
   ;; otherwise we have not found an element we care about, so we
   ;; count no insertiong, and keep recurring
   (else (multiinsertLR&co new oldL oldR (cdr l)
                           (lambda (newlat li ri)
                             (co (cons (car l) newlat) li ri))))))

;; lets count how many times we have inserted to the left and right
(define (countLR l li ri)
  (build li ri))

(multiinsertLR&co 'n 'L 'R '(L L R L R R R) countLR)

;; is a number even?
(define (even? n)
  (= (mod n 2) 0))

;; remove all odd numbers from a *list
(define (evens-only* l)
  (cond
   ((null? l) '())
   ((atom? (car l))
    (cond
     ((even? (car l)) (cons (car l) (evens-only* (cdr l))))
     (else (evens-only* (cdr l)))))
   (else (cons (evens-only* (car l)) (evens-only* (cdr l))))))

(evens-only* '(1 2 3 4 5 6 7 8 9 10))

;; now with a collector
;; in the book he collects the products of the evens, and sums of the odds
;; directly, here i collect the odd and even numbers in two lists, and can
;; pass some collecter to it to handle them, like figuring out products or sums
(define (evens-only*&co l co)
  (cond
   ((null? l) (co '() '()))
   ((atom? (car l))
    (cond
     ((even? (car l)) (evens-only*&co (cdr l) (lambda (newlat odds)
                                                (co (cons (car l) newlat)
                                                    odds))))
     (else (evens-only*&co (cdr l) (lambda (newlat odds)
                                     (co newlat (cons (car l) odds)))))))
   (else (cons (evens-only*&co (car l) (lambda (newlat odds)
                                         (co newlat odds)))
               (evens-only*&co (cdr l) (lambda (newlat odds)
                                         (co newlat odds)))))))

(define (show-evens-and-odds evens odds)
  (cons evens (cons odds '())))

(evens-only*&co '(1 2 3 4 5 6 7 8 9) show-evens-and-odds)


(define (count-evens-and-odds evens odds)
  (build (length evens) (length odds)))

(evens-only*&co '(1 2 3 4 5 6 7 8 9) count-evens-and-odds)

(define (product-of-evens evens odds)
  (cond
   ((null? evens) 1)
   (else (mul (car evens) (product-of-evens (cdr evens) odds)))))

(evens-only*&co '(1 2 3 4 5 6 7 8 9) product-of-evens)

(define (sum-of-odds evens odds)
  (cond
   ((null? odds) 1)
   (else (add (car odds) (sum-of-odds evens (cdr odds))))))

(evens-only*&co '(1 2 3 4 5 6 7 8 9) sum-of-odds)

;;;;;;;;;;;;;;;
;; Chapter 9 ;;
;;;;;;;;;;;;;;;

;; look for a symbol using a strange table
(define (keep-looking a sorn l)
  (cond
   [(number? sorn) (keep-looking a (pick sorn l) l)]
   (else (eq? sorn a))))

(define (looking a l)
  (keep-looking a (pick 1 l) l))

(looking 'kage '(2 3 kage mand))
(looking 'kage '(2 4 kage mand))

;; a short partial function
(define (eternity x)
  (eternity x))

;; shift from one pair to another
(define (shift p)
  (build (first (first p))
         (build (second (first p))
                (second p))))

(shift '((a b) c))
(shift '((a b) (c d)))

;; align a pair of things
(define (align pora)
  (cond
   [(atom? pora) pora]
   [(a-pair? (first pora)) (align (shift pora))]
   [else (build (first pora) (align (second pora)))]))

(align '(((a ((b c) (d e))) f) g) )

;; count the number of arguments in align
(define (length* pora)
  (cond
   ;; is it an atom? if yes, count it
   [(atom? pora) 1]
   ;; otherwise recur on the pairs parts
   [else (+ (length* (first pora))
            (length* (second pora)))]))

(length* '(((a ((b c) (d e))) f) g))

;; weight the arguments in align
(define (weight* pora)
  (cond
   [(atom? pora) 1]
   [else (+ (* (weight* (first pora)) 2) (weight* (second pora)))]))

(weight* '(((a ((b c) (d e))) f) g))
(weight* '((a b) c))
(weight* '(a (b c)))

;; so we can see that the size of the arguments in align
;; decreases with time, and it turns out that it has an
;; output for all inputs, making it a total function

;; shuffle pairs
(define (shuffle pora)
  (cond
   [(atom? pora) pora]
   [(a-pair? (first pora)) (shuffle (revpair pora))]
   [else (build (first pora) (shuffle (second pora)))]))

(shuffle '(a (b c)))
(shuffle '(a b))
;; shuffle is partial, because the following does not have
;; an output, to know the output it keeps swapping the
                                        ; components of the pair
(shuffle '((a b) (c d)))

;; the C function, by Lothar Collatz
(define (C n)
  (cond
   [(one? n) 1]
   [(even? n) (C (div n 2))]
   [else (C (add1 (mul 3 n)))]))

(C 1) ;; one
(C 2) ;; two
(C 3) ;; three
(C 0) ;; boom

;; the A function, by Wilhelm Ackermann
(define (A n m)
  (cond
   [(zero? n) (add1 m)]
   [(zero? m) (A (sub1 n) 1)]
   [else (A (sub1 n) (A n (sub1 m)))]))

(A 1 0)
(A 1 1)
(A 2 2)
(A 1 2)
(A 4 3)

;; A is still total, it returns a value for
;; (A 4 3), it just takes a really long time

;; an exercise in futility
(define will-stop? #f)

(define (last-try x)
  (and (will-stop? last-try)
       (eternity x)))

;; for more into, see the halting problem

;; in the next section I'm using define for convenience,
;; just replace with (lambda (...)) and apply manually to get the
;; point across, it's just a lot easier to showcase
;; a lot of values once it is defined

;; the length function that can only determine
;; the length of lists of size 0
(define (length0 l)
  (cond
   [(null? l) 0]
   [else (add 1 (eternity (cdr l)))]))

(length0 '())
(length0 '(a))

;; likewise for length 1
(define (length1 l)
  (cond
   [(null? l) 0]
   [else (add 1 ((lambda (l)
                   (cond
                    [(null? l) 0]
                    [else (add 1 (eternity (cdr l)))])) (cdr l)))]))

(length1 '())
(length1 '(a))
(length1 '(a b))

;; now lets abstract the alike parts out,
;; and create a function that creates the functions we need

;; length0
;; takes a function and applies it to eternity, like length0 did
(((lambda (mk-length)
    (mk-length eternity))
  (lambda (length)
    (lambda (l)
      (cond
       [(null? l) 0]
       [else (add1 (length (cdr l)))])))) '())

;; length1
;; takes a function and applies it to itself first, and then on eternity,
;; giving us a single count
(((lambda (mk-length)
    (mk-length
     (mk-length eternity)))
  (lambda (length)
    (lambda (l)
      (cond
       [(null? l) 0]
       [else (add1 (length (cdr l)))])))) '(a))

;; length2
;; etc. etc.
(((lambda (mk-length)
    (mk-length
     (mk-length
      (mk-length eternity))))
  (lambda (length)
    (lambda (l)
      (cond
       [(null? l) 0]
       [else (add1 (length (cdr l)))])))) '(a b))

;; ad infinitum

;; more fun, notice the scoping of the names, and now we simply blow
;; up if the 'tower' is not tall enough
(((lambda (mk-length)
    (mk-length mk-length))
  (lambda (mk-length)
    (lambda (l)
      (cond
       [(null? l) 0]
       [else (add1 (mk-length (cdr l)))])))) '())

;; the length function at last
(((lambda (mk-length)
    ;; this is the initial construction, allowing us to count lists of
    ;; size 0
    (mk-length mk-length))
  (lambda (mk-length)
    (lambda (l)
      (cond
       [(null? l) 0]
       ;; this is the magiç we keep adding a new recursion once we are
       ;; about to 'expire', making the 'tower' one taller.
       [else (add1 ((mk-length mk-length) (cdr l)))]))))
 '(a b c d e f g h i j k l m n o p q r s t u v w x y z))

;; lets play around with it, we pulled the (mk-length mk-length) part
;; out, and simply apply it on the lambda we introduced in its stead
(((lambda (mk-length)
    (mk-length mk-length))
  (lambda (mk-length)
    ((lambda (length)
       (lambda (l)
         (cond
          [(null? l) 0]
          [else (add1 (length (cdr l)))])))
     (mk-length mk-length))))
 '(a))
;; this no longer works, we end up recurring eternally, because we
;; keep applying mk-length to itself again and again, it never converges.


;; the problem was that mk-length used to return a function when we
;; applied it to an argument, it no longer does, it is applied into
;; the body of the lambda and duplicates the insides, instead of
;; making the length part of the tower taller

;; we can simply make it 'return' a function of one argument
(((lambda (mk-length)
    (mk-length mk-length))
  (lambda (mk-length)
    ((lambda (length)
       (lambda (l)
         (cond
          [(null? l) 0]
          [else (add1 (length (cdr l)))])))
     (lambda (x)
       ((mk-length mk-length) x)))))
 '(a b c d))
;; et voila

;; now, if we look at the inner part of the function, it does not
;; depend on the outer shell at all (the length part does not depend
;; on the mk-length part), so we can extract it!

(((lambda (le)
    ((lambda (mk-length)
       (mk-length mk-length))
     (lambda (mk-length)
       (le (lambda (x)
             ((mk-length mk-length) x))))))
  (lambda (length)
    (lambda (l)
      (cond
       [(null? l) 0]
       [else (add1 (length (cdr l)))]))))
 '(a b c d e))

;; now we have two parts, a part that looks like the length function
;; (the latter), and a part that does not look like the length
;; function, but which helps us do the recursion (the former)

;; the first part is also known as the applicative-order Y-combinator.
(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x)
             ((f f) x)))))))

;; or like this
(define Y
  (lambda (h)
    ((lambda (x) (h (lambda (a) ((x x) a))))
     (lambda (x) (h (lambda (a) ((x x) a)))))))
;; the reason these do not look like the Y-combinator by curry, is
;; because of schemes way of evaluating, if we wanted to have the (x
;; x) parts we would end in infinite recursion, this is solved by
;; 'wrapping', we simply create an equivalent function: for a function
;; F, we just (define G (lambda (x) (F x))), which gives us a function
;; G that is exactly like F,


;; so we can simply make the length function thusly, without define:
((Y
  (lambda (length)
    (lambda (l)
      (cond
       [(null? l) 0]
       [else (add1 (length (cdr l)))]))))
 '(a b c d e))

;; now we can de recursion for all kinds of functions
((Y
  (lambda (sum)
    (lambda (l)
      (cond
       [(null? l) 0]
       [else (add (car l) (sum (cdr l)))]))))
 '(1 2 3 4 5 6))

((Y
  (lambda (subst)
    (lambda (old new l)
      (cond
       [(null? l) '()]
       [(eq? old (car l)) (cons new (cdr l))]
       [else (cons (car l) (subst old new (cdr l)))]))))
 'a 'x '(a b c))
;; etc. etc.

;;;;;;;;;;;;;;;;
;; Chapter 10 ;;
;;;;;;;;;;;;;;;;

;; time to build a (simple) scheme interpreter in scheme

;; build a new entry from a set of names and a list of values
(define new-entry build)

(new-entry '(1 2 3 4) '(a b b c))

;; lookup a name in an entry, the function f we here use as a handler
;; for when a name is not found in the entry
(define (lookup-in-entry-helper name names values f)
  (cond
   [(null? names) (f name)]
   [(eq? name (car names)) (car values)]
   [else (lookup-in-entry-helper name (cdr names) (cdr values) f)]))

(define (lookup-in-entry name entry f)
  (lookup-in-entry-helper name (first entry) (second entry) f))

(lookup-in-entry '3
                 (new-entry '(1 2 3 4 5) '(a b c d e))
                 (lambda (x) x))

;; a table (environment) is a list of entries, this function extends
;; an environment
(define extend-table cons)

(extend-table
 (new-entry '(4 5 6 7) '(x y z f))
 (extend-table
  (new-entry '(1 2 3 4) '(a b b c))
  '()))

;; now we should be able to lookup into a table
(define (lookup-in-table name table f)
  (cond
   [(null? table) (f name)]
   [else (lookup-in-entry name (car table)
                          (lambda (x)
                            (lookup-in-table name (cdr table) f)))]))

;; now try to look something up
(lookup-in-table '2
                 (extend-table
                  (new-entry '(4 5 6 7) '(x y z f))
                  (extend-table
                   (new-entry '(1 2 3 4) '(a b b c))
                   '()))
                 (lambda (x) x))

;; notice how the newest entry 'shadows' the earlier entry
(lookup-in-table '4
                 (extend-table
                  (new-entry '(4 5 6 7) '(x y z f))
                  (extend-table
                   (new-entry '(1 2 3 4) '(a b b c))
                   '()))
                 (lambda (x) x))

;; the action for a const expression
(define (*const e env)
  (cond
   ;; numbers are constants, i.e. evaluate to themselves
   [(number? e) e]
   ;; truth values are constants as well
   [(eq? e #t) #t]
   [(eq? e #f) #f]
   ;; otherwise we are playing with a primitive
   [else (build 'primitive e)]))

;; helper to grab the text of a quote expression
(define text-of second)

;; the action for a quote expresison
(define (*quote e env)
  (text-of e))

;; the empty table
(define (initial-table name) '())

;; action for an identifier expression, when we meet an identifier, we
;; want to look it up in the current environment
(define (*identifier e env)
  (lookup-in-table e env initial-table))

;; the action for a lambda expression, here we need to save the things
;; needed for the lambda expression to give meaning, we save the
;; environment, its formals (arguments), and its function body, we
;; will use these to evaluate it later
(define (*lambda e env)
  (build 'non-primitive (cons env (cdr e))))

;; helpers to grab the three parts of the above list we made for
;; lambda expressions
(define table-of first)
(define formals-of second)
(define body-of third)

;; helpers to grab things from cond lines
(define question-of first)
(define answer-of second)
(define (else? e)
  (cond
   [(atom? e) (eq? e 'else)]
   [else #f]))

;; evaluate a cond expression
(define (eval-cond lines table)
  (cond
   ;; if this line is an else line, treat the question as true, and
   ;; evaluate the answer part
   [(else? (question-of (car lines)))
    (meaning (answer-of (car lines)) table)]
   ;; otherwise try to figure out the truth value of the question of
   ;; the line we are at, and evaluate the answer part if true
   [(meaning (question-of (car lines)) table)
    (meaning (answer-of (car lines)) table)]
   ;; if this line was not true, or an else line, move to the next line
   [else (eval-cond (cdr lines) table)]))

;; helper to grab the cond lines from a cond expression
(define cond-lines-of cdr)

;; the action for a cond expression
(define (*cond e env)
  (eval-cond (cond-lines-of e) env))

;; a little test, coffee is true in the environment, so we lookup the
;; value of klatsch, which should be 5
(*cond
 '(cond
   (coffee klatsch)
   (else party))
 '(((coffee) (#t))
   ((klatsch party)
    (5 (6)))))

;; given a list of expressions, return a list of the meanings of those
(define (eval-list l env)
  (cond
   [(null? l) '()]
   [else (cons (meaning (car l) env) (eval-list (cdr l) env))]))

;; helpers to grab things from an application statement
(define function-of car)
(define arguments-of cdr)

;; helpers to figure out which kind of function we are dealing with
(define (primitive? f)
  (eq? (function-of f) 'primitive))
(define (non-primitive? f)
  (eq? (function-of f) 'non-primitive))

;; helper to figure out if something is an atom, we need to add this
;; because we also need to handle our own primitive / nonprimitive
;; functions as atoms
(define (:atom? a)
  (cond
   [(atom? a) #t]
   [(null? a) #f]
   [(eq? (car a) 'primitive) #t]
   [(eq? (car a) 'non-primitive) #t]
   [else #f]))

;; these are the primitive functions, we apply to the values given, in
;; effect this is the 'implementation' of the built-in functions of
;; our interpreter
(define (apply-primitive name vals)
  (cond
   [(eq? name 'cons) (cons (first vals) (second vals))]
   [(eq? name 'car) (car (first vals))]
   [(eq? name 'cdr) (cdr (first vals))]
   [(eq? name 'null?) (null? (first vals))]
   [(eq? name 'eq?) (eq? (first vals) (second vals))]
   [(eq? name 'atom?) (:atom? (first vals))]
   [(eq? name 'zero?) (zero? (first vals))]
   [(eq? name 'add1) (add1 (first vals))]
   [(eq? name 'sub1) (sub1 (first vals))]
   [(eq? name 'number?) (number? (first vals))]))

(define (apply-closure closure vals)
  (meaning (body-of closure)
           (extend-table (new-entry (formals-of closure) vals)
                         (table-of closure))))

;; the actual apply function
(define (apply fun vals)
  (cond
   [(primitive? fun) (apply-primitive (second fun) vals)]
   [(non-primitive? fun) (apply-closure (second fun) vals)]))

;; action for an application (apply the meaning of a function, to the
;; meaning of its arguments)
(define (*application e env)
  (apply (meaning (function-of e) env)
         (meaning (arguments-of e) env)))

;; given an atom expression, what is its action?
(define (atom-to-action a)
  (cond
   [(number? a) *const]
   [(eq? a #f) *const]
   [(eq? a #t) *const]
   [(eq? a 'cons) *const]
   [(eq? a 'car) *const]
   [(eq? a 'cdr) *const]
   [(eq? a 'null?) *const]
   [(eq? a 'eq?) *const]
   [(eq? a 'atom?) *const]
   [(eq? a 'zero?) *const]
   [(eq? a 'add1) *const]
   [(eq? a 'sub1) *const]
   [(eq? a 'number?) *const]
   [else *identifier]))

;; given a list expression, what is its action?
(define (list-to-action l)
  (cond
   [(atom? (car l))
    (cond
     [(eq? (car l) 'quote) *quote]
     [(eq? (car l) 'lambda) *lambda]
     [(eq? (car l) 'cond) *cond]
     [else *application])]
   [else *application]))

;; given an expression, determine its action
(define (expression-to-action e)
  (cond
   [(atom? e) (atom-to-action e)]
   [else (list-to-action e)]))

;; determine the meaning in some environment
(define (meaning e table)
  ((expression-to-action e) e table))

;; figure out the value of an expression, the initial table is empty
(define (value e)
  (meaning e '()))

;; now lets try it out on something a little rough
(value
 (((lambda (le)
     ((lambda (f)
        (f f))
      (lambda (f)
        (le (lambda (x)
              ((f f) x))))))
   (lambda (length)
     (lambda (l)
       (cond
        [(null? l) 0]
        [else (add1 (length (cdr l)))]))))
  '(a b c d e f g))
 )
;; et voila
