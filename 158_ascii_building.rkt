#lang racket

(define (get-str st)
  (define j "++--***...")
  (cond [(string=? st "j") j]
        [(string=? st "i") (substring j 0 (- (string-length j) 1))]
        [(string=? st "h") (substring j 0 (- (string-length j) 2))]
        [(string=? st "g") (substring j 0 (- (string-length j) 3))]
        [(string=? st "f") (substring j 0 (- (string-length j) 4))]
        [(string=? st "e") (substring j 0 (- (string-length j) 5))]
        [(string=? st "d") (substring j 0 (- (string-length j) 6))]
        [(string=? st "c") (substring j 0 (- (string-length j) 7))]
        [(string=? st "b") (substring j 0 (- (string-length j) 8))]
        [(string=? st "a") (substring j 0 (- (string-length j) 9))]
        [else (error (format "invalid input to get-str: ~a" st))]))

(define (format1 str)
  (~a str #:min-width 10 #:right-pad-string " "))

(define (parse input)
  (regexp-match* #rx"[a-j]|[0-9][a-j]" input))

(define (get-line str)
  (cond [(regexp-match #rx"([0-9])([a-j])" str) 
         => (λ (x) (list (format1 (space-string (second x) (get-str (third x))))))]
        [(regexp-match #rx"([a-j])" str) => (λ (x) (list (format1 (get-str (first x)))))]
        [else (error "invalid input to make-line")]))

;turns ListofListofStrings -> ListofStrings
(define (squash llst)
  (map string->list (map car llst)))

(define (rotate llst)
  (if (empty? (first llst))
      '()
      (cons (map last llst) 
            (rotate (map (lambda (x) (take x (- (length x) 1))) llst)))))

(define (space-string num str)
  (foldl (λ (x res) (string-append (string #\space) res))
         str (range (string->number num))))

(define (show llst)
  (map displayln (map list->string llst))
  (void))

(define (do x)
  ((compose1 show rotate squash (curry map get-line) parse) x))

(do "j3f3e3e3d3d3c3cee3c3c3d3d3e3e3f3fjij3f3f3e3e3d3d3c3cee3c3c3d3d3e3e3fj")