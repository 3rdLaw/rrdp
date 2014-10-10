#lang racket
(require rsound)

(define 1-sec 44020) ;sample rate
;Lengths per this guy: http://www.comportco.com/~w5alt/cw/cwindex.php?pg=4
(define dit-length (floor (/ 1-sec 14)))
(define dah-length (floor (* dit-length 3)))
(define dit/dah-spacing dit-length)
(define char-spacing dah-length)
(define word-spacing (floor (* char-spacing 2)))

(define dit (synth-note "vgame" 10 72 dit-length))
(define dah (synth-note "vgame" 10 72 dah-length))
(define dit/dah-space (silence dit/dah-spacing))
(define char-space (silence char-spacing))
(define word-space (silence word-spacing))

(define ltr->morse
  #hash(
        (a . ".-") (b . "-...") (c . "-.-.") (d . "-..") (e . ".") (f . "..-.") (g . "--.")
                   (h . "....") (i . "..") (j . ".---") (k . "-.-") (l . ".-..") (m . "--") 
                   (n . "-.") (o . "---") (p . ".--.") (q . "--.-") (r . ".-.") (s . "...") (t . "-")
                   (u . "..--") (v . "...-") (w . ".--") (x . "-..-") (y . "-.--") (z . "--..")
                   (| | . "/")))

(define morse->sound
  (hash #\. dit
        #\- dah
        #\/ word-space))

(define (lst->string lst) (string-join lst))

;string-> list of morse code for each char
(define (parse str)
  ((compose1 (curry map (λ(x) (hash-ref ltr->morse (string->symbol x)))) ;map each char to morse code
             (curry filter (λ(x) (not (string=? "" x)))) ;remove empty strings
             (curryr string-split "") string-downcase) ;down-case and split on each char
   str))

;replaces '(...char-space word-space char-space...)  with just '(...word-space...)
(define (strip-extra-char-spaces lst)
  (define (recur input output)
    (cond [(empty? input) output]
          ; drop char-space in front
          [(and (> (length input) 1) (equal? (car input) char-space) (equal? (cadr input) word-space))
           (recur (cdr input) output)]
          ;drop char-space in back
          [(and (> (length input) 1) (equal? (car input) word-space) (equal? (cadr input) char-space))
           (recur (cddr input) (cons (car input) output))]
          [else  (recur (cdr input) (cons (car input) output))]))
  
  (reverse (recur lst '())))

;ListOf( Morse-Code-Strings) -> rsound
(define (morse->audio lst)
             ;rs-append* actually makes the sound
  ((compose1 rs-append* strip-extra-char-spaces ;remove un-needed char-spaces if we have a word-space
             flatten (curryr add-between char-space) ;add char-spaces
             ; map each string char to appropriate sound w/spacing
             (curry map (λ(x) (add-between (for/list ([z (in-string x)])
                                             (hash-ref morse->sound z))
                                           dit/dah-space))))
   lst))

(define (run str #:play-it [play-it #t] #:save [save #f] #:path [path "morse.wav"])
  (define lst (parse str))
  (define audio (morse->audio lst))
  (when save (rs-write audio path))
  (when play-it (play audio))
  (lst->string lst))

(module+ test
  (require rackunit)
  (check-equal? (run "i like cats" #:play-it #f) ".. / .-.. .. -.- . / -.-. .- - ...")
  (check-equal? (run "abcdefghijklmnopqrstuvwxyz" #:play-it #f)
                (string-append ".- -... -.-. -.. ."
                               " ..-. --. .... .. .--- -.- .-.. -- -. --- .--. --.- .-. ..."
                               " - ..-- ...- .-- -..- -.-- --..")))

(run "I like cats" #:save #t)