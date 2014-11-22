#lang racket

(define num->int-hash 
  #hash((#\I . 1) (#\V . 5) (#\X . 10) (#\L . 50) (#\C . 100) (#\D . 500) (#\M . 1000)))
(define (lookup char) (hash-ref num->int-hash (char-upcase char)))

(define int->num-hash
  #hash((1000 . "M") (900 . "CM") (500 . "D") (400 . "CD") (100 . "C") (90 . "XC")
                     (50 . "L") (40 . "XL") (10 . "X") (9 . "IX") (5 . "V") (4 . "IV") (1 . "I")))


;converts a single char or list of chars to the appropriate numerical value
(define -> (match-lambda
             [(list num) (lookup num)] ;single char in list
             [(list frst scnd) (- (-> scnd) (-> frst))] ;list of 2 chars
             [char (lookup char)])) ;single char

;;groups numerals into appropriate bunches.
; str -> Listof( (char OR Listof(char)))
;e.g.: "LIX" -> '((#\L) (#\I #\X))
(define (group str)
  (define (recur store itms)
    (cond [(empty? itms) store]
          [else 
           (match-define (list frst rst ...) itms)
           #|Normally values descrease as we go left to right.
             However, if the current value is less than the subsequent,
             then it's part of a prefix number i.e.: IV, or XC, so we add them both. |#
           (if (and (cons? rst) ((-> frst) . < . (-> (first rst))))
               (recur (cons (list frst (car rst)) store) (cdr rst))
               (recur (cons (list frst) store) rst))]))
  (reverse (recur '() (string->list str))))

(define (roman-to-int str)
  (foldl + 0 ((compose1 (curry map ->) group) str)))

(define (int-to-roman num)
  ((compose1 (curryr string-join "") second);'second' to drop sum and just get numerals
   (foldl (match-lambda** 
           [((cons int sym) (list sum accum))
            (define times (quotient sum int)) ;does current number divide?
            (if (= 0 times) (list sum accum) ;if not skip
                ;otherwise, subtract out current #'s multiple, and append numerals to the list
                (list (- sum (* times int)) (append accum (build-list times (λ(_) sym)))))])
          (list num (list))
          (sort (hash->list int->num-hash) > #:key car)))) ;fold from biggest to smallest

(module+ test
  (require rackunit)
  
  (define-syntax-rule (end-to-end-test-pair pair)
    (begin
      (check-equal? (roman-to-int (car pair)) (cdr pair))
      (check-equal? (int-to-roman (cdr pair)) (car pair))))
  
  (end-to-end-test-pair '("VIII" . 8))
  (end-to-end-test-pair '("XII" . 12))
  (end-to-end-test-pair '("IX" . 9))
  (end-to-end-test-pair '("II" . 2))
  (end-to-end-test-pair '("MDCCLXXVI" . 1776))
  (end-to-end-test-pair '("XCIV" . 94))
  (end-to-end-test-pair '("IV" . 4))
  (end-to-end-test-pair '("XXXIV" . 34))
  (end-to-end-test-pair '("CCLXVII" . 267))
  (end-to-end-test-pair '("DCCLXIV" . 764))
  (end-to-end-test-pair '("CMLXXXVII" . 987))
  (end-to-end-test-pair '("MCMLXXXIII" . 1983))
  (end-to-end-test-pair '("MMXIV" . 2014))
  (end-to-end-test-pair '("MMMM" . 4000))
  (end-to-end-test-pair '("MMMMCMXCIX" . 4999)))