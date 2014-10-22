#lang racket

(define card-values 
  '(("KING" . 10) ("QUEEN". 10) ("JACK" . 10) ("TEN"  . 10) ("NINE" . 9) ("EIGHT". 8)
                  ("SEVEN". 7) ("SIX"  . 6) ("FIVE" . 5) ("FOUR" . 4) ("THREE". 3) ("TWO"  . 2)))

;str -> (list name #-cards (list-of-card-values (i.e.: FIVE NINE...)))
(define str->hand
  (match-lambda 
    [(regexp #px"([a-zA-z]+): ((?:[a-zA-z]+ of [a-zA-z]+,* *)+)" (list _ name cards))
     (define cards-without-suits
       (map (compose1 string-upcase (match-lambda
                                      [(regexp #rx" *([A-za-z]+) of [A-Za-z]+" (list _ num)) num]))
            (string-split cards ",")))
     (list name (length cards-without-suits) cards-without-suits)]))

(define (check-hand str)
  (let* ([lines (string-split str "\n")]
         [num-players (string->number (first lines))]
         [hands (map str->hand (cdr lines))]
         [hand-values (map get-hand-values hands)]
         ;hand-values has the form (list name #-cards SUM)
         [five-card-tricks (filter (λ(x) (and (>= (second x) 5) (> (third x) -1))) hand-values)]
         [twenty-ones (filter (λ(x) (= (third x) 21)) hand-values)]
         [num-busts (count (λ(x) (= (third x) -1)) hand-values)])
    
    (cond [(cons? five-card-tricks)       (print-winners five-card-tricks "a five-card trick")]
          [(cons? twenty-ones)            (print-winners twenty-ones "21")]
          [(= num-busts num-players)             "Everyone busts."]
          [else
           (define max (third (car (sort hand-values > #:key third))))
           (define wnrs (filter (λ(x) (= (third x) max)) hand-values))
           (print-winners wnrs max)])))

(define (print-winners lst arg)
  (string-append (apply ~a (add-between (map (curry first) lst) " & ")) 
                 (if (> (length lst) 1) " tie with " " wins with ")
                 (if (not (string? arg)) (~a arg) arg) "."))

;swaps out '(FIVE NINE) for '(14)
(define get-hand-values
  (match-lambda [(list name num cards)
                 (list name num (calculate cards))]))

(define (calculate lst) 
  ;recursively builds deck value
  (define (recur lst)
    (cond [(empty? lst) 0]
          [(member "ACE" lst) 
           (define other-vals (recur (remove "ACE" lst)))
           (if (list? other-vals)
               (remove-duplicates ;extra complexity in case of multiple Aces
                (append (map (curry + 1) other-vals) (map (curry + 11) other-vals)))
               (list (+ 1 other-vals) (+ 11 other-vals)))]
          [else ;current card value + value of the rest of the deck through recursion
           (+ (cdr (assoc (car lst) card-values)) (recur (cdr lst)))]))
  
  ;returns highest sum under 21 or -1 if bust
  (define (get-highest nums)
    (if (list? nums) 
        (let ([remove-over-21 (filter (λ(x) (x . <= . 21))  (sort nums >))])
          (if (empty? remove-over-21) -1 (car (take remove-over-21 1))))
        (if (nums . > . 21) -1 nums)))
  
  (get-highest (recur lst)))

; Tests------------------------------------------------------|

(define game1 
  "3
Alice: Ace of Diamonds, Ten of Clubs
Bob: Three of Hearts, Six of Spades, Seven of Spades
Chris: Ten of Hearts, Three of Diamonds, Jack of Clubs
")
(define game2
  "4
Alice: Ace of Diamonds, Ten of Clubs
Bob: Three of Hearts, Six of Spades, Seven of Spades
Chris: Ten of Hearts, Three of Diamonds, Jack of Clubs
David: Two of Hearts, Three of Clubs, Three of Hearts, Five of Hearts, Six of Hearts")

(define game3
  "3
Alice: Ace of Diamonds, Ace of Spades, Ace of Clubs, Ace of Hearts
Bob: Three of Hearts, Six of Spades, Seven of Spades, Ten of Diamonds
Chris: Ten of Hearts, Three of Diamonds")

(define game4
  "3
Alice: Nine of Diamonds, Five of Clubs, Jack of Spades
Bob: King of Diamonds, Six of Spades, Seven of Spades
Chris: Ten of Hearts, Three of Diamonds, Jack of Clubs")

(define game5
  "3
Alice: Two of Clubs, Three of Clubs, Two of Diamonds, Six of Diamonds, Four of Diamonds
Bob: Four of Spades, Three of Hearts, Two of Spades, Six of Spades, Four of Hearts
Chris: King of Spades, Queen of Hearts, Ace of Diamonds")

(define game6 
  "3
Alice: Ten of Hearts, Jack of Spades
Bob: King of Diamonds, Five of Hearts, Five of Clubs
Chris: Queen of Spades, King of Hearts")

(check-hand game1)
(check-hand game2)
(check-hand game3)
(check-hand game4)
(check-hand game5)
(check-hand game6)