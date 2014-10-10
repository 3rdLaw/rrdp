#lang racket

;;Note, this works fine, but skeeto on rdp did in one pass more efficiently.
; Like this: initialiaze all to zero, generate a unique mine location, place it,
;  then increment just the locations nearby, and then repeat this 
;  until sufficient # of mines added.

(define (list-all-squares height width)
  (for*/list ([h height]
              [w width])
    (list h w)))

(define (gen-board height width num-mines)
  ; (-> listof (posn))
  (define (generate-mines mines)
    (let loop ([lst '()])
      (if (= (length lst) mines)
          lst
          (let ([new (list (random height) (random width))])
            (loop (remove-duplicates (cons new lst)))))))
  
  ; (-> listof (posn))
  (define (get-neighbors list-loc)
    (match-define (list height width) list-loc)
    (for*/list ([w (range -1 2)]
                [h (range -1 2)])
      (list (+ height h) (+ width w))))
  
  ;create hash and add mines
  (define mines (foldl (λ(x grid) (hash-set grid x 'x))
                       (hash)
                       (generate-mines num-mines)))
  
  ;return list dimensions and hash with numbers filled
  (list height width 
        (foldl (λ(x res) 
                 (if (symbol? (hash-ref res x 0))
                     res
                     (hash-set res x 
                               (length (filter symbol? (map (λ(x) (hash-ref res x 0))
                                                            (get-neighbors x)))))))
               mines
               (list-all-squares height width))))
;prints
(define (pp lst)
  (match-define (list height width hsh) lst)
  (map (λ(x) (display (format "~a " (hash-ref hsh x)))
         (when (= (second x) (sub1 width)) (displayln "")))
       (list-all-squares height width))
  (void))

(define g (gen-board 15 10 20))
(pp g)