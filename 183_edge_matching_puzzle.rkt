#lang racket
(require sugar "../../library.rkt")

;id is letter, rotation is one of (0 90 180 270), edges is list of 4 edges
(struct tile (id rotation edges) #:transparent)

(define rotations '(0 90 180 270))

;"test" -> '(t e s t)
(define (string->list-of-symbols str)
  (map (λ(y) (string->symbol (string y)))
       (string->list str)))

;takes list of strings, turns into list of tiles
(define (generate-tiles num lst-strs)
  ;Generate letters for each tile
  (define letters 
    (map (λ(x) (string->symbol (string (integer->char (+ (char->integer #\A) x)))))
         (range 0 (* num num))))
  
  ;we make a list by putting together each letter with it's tiles
  ;where its tiles first get translated from a string into a ListofSymbols
  (map (λ(letter symbols)
         (tile letter 0 symbols)) 
       letters
       (map string->list-of-symbols lst-strs)))

;puzzle_string->List(size,ListOf(tiles)
(define (parse str)
  (define strs (string-split str "\n"))
  (define size (string->number (first strs)))
  (define tiles (generate-tiles size (cdr strs)))
  (list size tiles))

(define (get-edge tile edge)
  (list-ref (tile-edges tile)
            (case edge
              [(0 top TOP up UP) 0]
              [(1 right RIGHT) 1]
              [(2 bottom BOTTOM down DOWN) 2]
              [(3 left LEFT) 3]
              [else (error "no match for get-edge case")])))

(define (rotate tile1 dir)
  (define cur-rot (tile-rotation tile1))
  (match-define (list u r d l) (tile-edges tile1))
  (case dir
    [(0) tile1];no rotation, return original
    [(1 90 right) (struct-copy tile tile1
                               (rotation (+ cur-rot 90))
                               (edges (list l u r d)))]
    [(2 180 down) (struct-copy tile tile1
                               (rotation (+ cur-rot 180))
                               (edges (list d l u r)))]
    [(3 270) (struct-copy tile tile1
                          (rotation (+ cur-rot 270))
                          (edges (list r d l u)))]
    [else (error "no match for rotate case")]))

;tile comparator used in remove calls in search below
(define (tile=? x y) (symbol=? (tile-id x) (tile-id y)))

(define (solve lst)
  (match-define (list size tiles) lst)
  
  ;DFS
  (let loop ([placed '()] [remaining tiles])
    (cond [(empty? remaining) (pp size placed) placed]
          [(empty? placed)
           (for/or ([x (in-list remaining)]);try each tile as first w/each rotation
             (for/or ([z rotations])
               (define rotated (rotate x z))
               (loop (list rotated) (remove rotated remaining tile=?))))]
          [else
           (for/or ([x (in-list (get-matches size placed remaining))])
             (loop (append placed (list x)) (remove x remaining tile=?)))])))

;Inverts the case of a symbol (ex: 'D -> 'd)
(define (invert-case sym)
  (define str (symbol->string sym))
  (if (char-lower-case? (string-ref str 0))
      (string->symbol (string-upcase str))
      (string->symbol (string-downcase str))))

;find each rotation where a given color is at given direction
(define (rotate-until tile dir color)
  (filter-map (λ(x) (define rotated (rotate tile x))
                (and (symbol=? (get-edge rotated dir) color) rotated))
              rotations))

;if tile has requested edge, rotate it into the requested position(s)
(define (has-edge? tile color dir)
  (if (member color (tile-edges tile))
      (rotate-until tile dir color)
      #f))

;Same idea as above but handles two dir/color pairs, given in a list
(define (has-2-edges? tile edges-lst)
  (match-define (list dir1 color1 dir2 color2) edges-lst)
  
  (match (tile-edges tile) ;classy, list-no-order matching folows:
    [(list-no-order (== color1) (== color2) (== color1) (== color2));two pairs of wanted colors
     (filter-map (λ(x) (define rotated (rotate tile x))
                   (and  (symbol=? (get-edge rotated dir1) color1)
                         (symbol=? (get-edge rotated dir2) color2) 
                         rotated))
                 rotations)]
    [(list-no-order (== color1) (== color2) _ ...);one pair of wanted colors
     (for/or ([x rotations])
       (define rotated (rotate tile x))
       (and  (symbol=? (get-edge rotated dir1) color1)
             (symbol=? (get-edge rotated dir2) color2) 
             rotated))]
    [_ #f]))

;This will need to get rewritten if we insert into the list other than consequentively
(define (get-matches size placed remaining)
  (define next (length placed))
  (cond [(< next size) ;just match to the left (i.e., we're on the top row)
         (define wanted-color (invert-case (get-edge (get placed (sub1 next)) 'right)))
         (flatten (filter-map (curryr has-edge? wanted-color 'left) remaining))]
        ;we flatten above b/c (has-edge? ...) can return mutiple matches
        [(= (modulo next size) 0) ;just match above (i.e., we're on the left column)
         (define wanted-color (invert-case (get-edge (get placed (- next size)) 'down)))
         (flatten (filter-map (curryr has-edge? wanted-color 'up) remaining))]
        [else ;match left and above (i.e., everywhere else)
         (define wanted-left (invert-case (get-edge (get placed (sub1 next)) 'right)))
         (define wanted-up (invert-case (get-edge (get placed (- next size)) 'down)))
         (flatten 
          (filter-map (curryr has-2-edges? (list 'left wanted-left 'up wanted-up)) remaining))]))

;return readable representation as a multi-line string
(define (pp size lst)
  (define lst-of-size (chunk lst size))
  
  (define (row->string lst)
    (define ltr-in-middle (λ(x) (format "| ~a |" x)))
    (define mid-row (λ(x) (match-define (list left id right) x) (format "~a ~a ~a" left id right)))
    (let* ([tops (map (curryr get-edge 'up) lst)]
           [top (map ltr-in-middle tops)]
           [mids (map (λ(x) (list (get-edge x 'left) (tile-id x) (get-edge x 'right))) lst)]
           [mid (map mid-row mids)]
           [bottoms (map (curryr get-edge 'bottom) lst)]
           [bottom (map ltr-in-middle bottoms)])
      (string-join (map (curryr string-join "") (list top mid bottom)) "\n")))
  
  (displayln
   (string-join 
    (map row->string
         lst-of-size)
    "\n")))


(define (play str)
  (solve (parse str)))

(define sample1 
  "3
CYMk
CmKm
cKyM
cYkY
CMky
ckyM
CYMK
CMKy
CkmY")

(define sample2 "3
ycKC
kKcY
cKMc
mYmk
CCYk
mMyC
MyYk
mKMy
YCMk")

(define challenge1
  "4
mcYC
MmCk
yYcm
yMYC
Ykcy
kkkm
KKcy
KMYK
YMkk
ymKc
MyMK
CmmY
kMMY
yCCM
yccc
kcck")

(define challenge2
  "5
cKCk
yYcc
YcCK
kKCM
CMKc
cKYC
kYcm
KYyY
Mccm
yKcm
mykK
MMCm
ckYC
ycmm
MmKM
kymc
KMMK
KcyM
kYck
YCKM
myYm
kYyY
CMKM
yYCM
YKyk")

(play sample1)
(play sample2)
(play challenge1)
(play challenge2)