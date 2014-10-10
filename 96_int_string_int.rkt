#lang racket
(require sugar)

; Word to Integer-------------------------------------------------------||
(define hsh_word->int
  #hash(
        (zero . 0) (one . 1) (two . 2) (three . 3) (four . 4) (five . 5) (six . 6) (seven . 7) (eight . 8)
                   (nine . 9) (ten . 10) (eleven . 11) (twelve . 12) (thirteen . 13) (fourteen . 14) (fifteen . 15)
                   (sixteen . 16) (seventeen . 17) (eighteen . 18) (nineteen . 19) (twenty . 20) (thirty . 30) 
                   (forty . 40) (fifty . 50) (sixty . 60) (seventy . 70) (eighty . 80) (ninety . 90) (hundred . 100)
                   (thousand . 1000) (million . 1000000) (billion . 1000000000) (trillion . 1000000000000)))

(define multipliers  '(hundred thousand million billion trillion))

(define (words->int lst)
  ;we chunk the inputs until we hit a multiplier
  (let loop ([sum 0] [prev-hundred 0] [chunk 0] [remaining lst])
    (if (empty? remaining)
        (+ sum chunk)
        (let ()
          (match-define (list itm rst ...) remaining)
          ;basically we add numbers to the chunk until we hit a multiplier
          ;special handling for the hundreds, since we might need to recall them later
          ; (e.g: we've already added 700,000 but we needed to add 768,000 in its place)
          (cond [(and (in? itm multipliers) (symbol=? itm 'hundred))
                 (define sm (* chunk (get hsh_word->int itm)))
                 (loop (+ sum sm) sm 0 rst)] ;keep hundreds sum available for next ieration
                [(and (in? itm multipliers) (not (= prev-hundred 0)))
                 (loop (+ (- sum prev-hundred) ;undo previous addition of hundreds
                          ;but then add it again combined with current chunk
                          (* (+ chunk prev-hundred) (get hsh_word->int itm))) 0 0 rst)]
                [(in? itm multipliers)
                 (loop (+ sum (* chunk (get hsh_word->int itm))) 0 0 rst)]
                [else (loop sum prev-hundred (+ chunk (get hsh_word->int itm)) rst)])))))

(define (str->int str-in)
  (words->int 
   (map ->symbol ;turn strings into symbols
        (string-split ;split on spaces
         ;replace "-" & " and " with #\space
         (regexp-replace* #rx"\\-| and " (string-downcase str-in) " ")
         " "))))


;Converts a number (0-9) char into its integer form
(define (num_char->int ch)
  (- (char->integer ch) (char->integer #\0)))



; Integer to Word-------------------------------------------------------||
(define hsh_int->word
  (for/hash ([(k v) (in-hash hsh_word->int)]
             #:when (< v 10))
    (values v k)))

(define hsh_teens_int->word
  (for/hash ([(k v) (in-hash hsh_word->int)]
             #:when (and (> v 9) (< v 100)))
    (values v k)))

(define cluster-suffix
  #hash((1 . thousand)
        (2 . million)
        (3 . billion)
        (4 . trillion)))

;give names to each 3-digit combination
(define (parse-triplet lst1) 
  (define (parse x res)
    ;current power-of-ten, previous value, and accumulator list
    (match-define (list place previous lst) res)
    (if (= x 0) ;skip
        (list (add1 place) x lst)
        (cond [(= place 0) ;add simple num->word translation
               (list (add1 place) x (cons (get hsh_int->word x) lst))]
              [(= place 1)
               (if (= x 1) ;teens are complicated b/c we sometimes modify the last value
                   (list (add1 place) x 
                         (cons (get hsh_teens_int->word 
                                    (+ 10 previous)) (if (empty? lst) lst (drop lst 1))))
                   ;20's/30's/etc. are simpler
                   (list (add1 place) x (cons (get hsh_teens_int->word (* 10 x)) lst)))]
              [(= place 2) ;normal + hundreds symbol
               (list (add1 place) x (cons (get hsh_int->word x) (cons 'hundred lst)))])))
  
  ;we grab the third to only get the final result
  (third (foldr parse
                (list 0 -1 '())
                lst1)))

;add multiplier based on power of ten of the current 3-chunk
(define (add-multiplier lst)
  (define lngth (- (length lst) 1))
  (for/list ([i (length lst)])
    (if (hash-ref cluster-suffix (- lngth i) #f)
        (append (list-ref lst i) (list (hash-ref cluster-suffix (- lngth i))))
        (list-ref lst i))))

;chunk into groups of num, from right-to-left
(define (chunk lst num) 
  (define (recur num lst so-far)
    (if (empty? lst)
        so-far
        (let loop ([cur (list (car lst))][rst (cdr lst)])
          (if (or (empty? rst) (= (length cur) num))
              (recur num rst (cons cur so-far))
              (loop (cons (car rst) cur) (cdr rst))))))
  
  (recur num (reverse lst) '()))

(define (int->words num)
  ((compose1 string-join (curry map symbol->string) (curry apply append) 
             add-multiplier (curry map parse-triplet) (curryr chunk 3)
             (curry map num_char->int) string->list number->string)
   num)) 

; Tests!-------------------------------------------------------||
(module+ test   
  (require rackunit)
  
  (check-eq? (str->int "Four") 4)
  (check-eq? (str->int "One Hundred Million and Two") 100000002)
  (check-eq? (str->int "Eleven") 11)
  (check-eq? (str->int "Sixty three") 63)
  (check-eq? (str->int "One-Thousand and Thirty-Four") 1034)
  (check-eq? (str->int "One Hundred Ninety-Seven") 197)
  (check-eq? (str->int "One Billion and one") 1000000001)
  (check-eq? (str->int "Two Hundred and Twenty One Thousand Three Hundred Sixty Eight") 221368)
  (check-eq? (str->int "One Hundred Thirty-Nine Million Nine Hundred Eighty-Seven thousand Six-Hundred Ninety-Seven") 139987697)
  (check-eq? (str->int "Four trillion Nine billion One Hundred twenty three Million Four Hundred and Fifty-Six thousand Seven-Hundred and Eighty-Nine") 4009123456789)
  (check-equal? (int->words 768) "seven hundred sixty eight")
  (check-equal? (int->words 101) "one hundred one")
  (check-equal? (int->words 1)   "one")
  (check-equal? (int->words 001) "one" )
  (check-equal? (int->words 93) "ninety three")
  (check-equal? (int->words 64) "sixty four")
  (check-equal? (int->words 14) "fourteen" )
  (check-equal? (int->words 999) "nine hundred ninety nine")
  (check-equal? (int->words 900654003) "nine hundred million six hundred fifty four thousand three")
  (check-equal? (int->words 9010003) "nine million ten thousand three")
  (check-equal? (int->words 911234) "nine hundred eleven thousand two hundred thirty four")
  (check-equal? (int->words 26123877) "twenty six million one hundred twenty three thousand eight hundred seventy seven")
  (check-equal? (int->words 973448656) "nine hundred seventy three million four hundred forty eight thousand six hundred fifty six")
  (check-equal? (int->words 987654321987654) "nine hundred eighty seven trillion six hundred fifty four billion three hundred twenty one million nine hundred eighty seven thousand six hundred fifty four"))