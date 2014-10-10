#lang racket
(require sugar graph)

;string -> weighted-graph/undirected
(define (parse str) 
  ((compose1 weighted-graph/undirected
             (curry map (λ(x) (match x [(list to from num)
                                        (list (->int num) (string->symbol (string-downcase to))
                                              (string->symbol (string-downcase from)))])))
             (curry map (curryr string-split " ")) (curryr string-split "\n"))
   str))

;; turns '(a b c d) into '((a b) (b c) (c d))
; in our case, this gets all edges used
(define (list->chain-pairs lst1)
  (define (recur sols lst)
    (if (= (length lst) 2) (cons (list (first lst) (second lst)) sols)
        (recur (cons (list (car lst) (cadr lst)) sols) (cdr lst))))
  
  (reverse (recur '() lst1)))

;in-efficient! ~ O(n^2).  We could use hash instead for speed
(define (repeated-edges? lst)
  (define all-edges-used (list->chain-pairs lst))
  (let loop ([verbotten '()][remaining all-edges-used])
    (cond [(empty? remaining) #f]
          [else 
           (match-define (list first-pair res ...) remaining)
           (if (member first-pair verbotten)
               #t
               (loop (append (list first-pair) (list (reverse first-pair)) verbotten)
                     res))])))

(define (gas-used g lst)
  (define (recur total steps)
    (if (= (length steps) 2)
        (+ (edge-weight g (first steps) (second steps)) total)
        (recur (+ (edge-weight g (car steps) (second steps)) total) (cdr steps))))
  
  (recur 0 lst))

(define (count-unique-elements lst) (length (remove-duplicates lst)))

;; BFS - we save possibilities only if they return to start, and weed out when 
; they a) cost too much gas, or b) when they repeat edges
; It works surprisingly fast this way!
(define (solve graph gas start #:debug [debug false])
  (define solutions
    (sort ;sort by decresing unique elements
     (map reverse ;make chains start->finish instead of finish->start
          (let loop ([queue (list (list start))] [solutions '()])
            (cond [(empty? queue) solutions]
                  [else ;take top off queue
                   (match-define (list cur rst ...) queue)
                   (cond [(and (> (length cur) 1) (or (> (gas-used graph cur) gas)
                                                      (repeated-edges? cur)))
                          (loop rst solutions)] ;discard
                         [(and (> (length cur) 1) (equal? (first cur) start))
                          (loop rst (cons cur solutions))] ;save
                         [else ;add new paths for each neighbor to the queue
                          (define stiched-lists (map (curryr cons cur)
                                                     (get-neighbors graph (first cur))))
                          (loop (append rst stiched-lists) solutions)])])))
     (λ(x y) (> (count-unique-elements x) (count-unique-elements y)))))
  
  (when debug (pretty-print solutions))
  (if (not (empty? solutions))
      (get-stats-from-solution graph (first solutions))
      "No solution"))

(define (get-stats-from-solution graph x)
  (let ([cost (->string (gas-used graph x))] [uniqs (->string (count-unique-elements x))]
                                             [lst (format "'~a" x)])
    (format "Cost: ~a, Unique sites: ~a, Path: ~a" cost uniqs lst)))

(define (save-graphiv-file g [name "graph.txt"])
  (call-with-output-file name #:exists 'truncate
    (lambda (out) (display (graphviz g) out))))

(define galaxy "A B 1
A C 1
B C 2
B D 2
C D 1
C E 2
D E 2
D F 2
D G 1
E G 1
E H 1
F I 4 
F G 3
G J 2
G H 3
H K 3
I J 2
I K 2")

(define (run [input galaxy] #:gas [has 5] [start 'a] #:image [image #f]) 
  (define g (parse input))
  (when image (save-graphiv-file g))
  (solve g has start))

(run)