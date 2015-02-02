#lang racket
;;https://www.reddit.com/r/dailyprogrammer/comments/1qira9/111213_challenge_135_intermediate_de_morgans_law/

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)

(define-tokens value-tokens (ID))
(define-empty-tokens op-tokens (AND OR NOT LPAR RPAR EOF))

(define (atom? x)
  (not (or (pair? x) (null? x))))

(define my-lexer
  (lexer
   [(:+ (char-range #\a #\z)) (token-ID (string->symbol lexeme))]
   [#\(                       (token-LPAR)]
   [#\)                       (token-RPAR)]
   ["AND"                     (token-AND)]
   ["OR"                      (token-OR)]
   ["NOT"                     (token-NOT)]
   [whitespace                (my-lexer input-port)]
   [(eof)                     (token-EOF)]))

(define my-parser
  (parser
   (start exp)
   (end EOF)
   (tokens value-tokens op-tokens)
   (error (lambda (tok-ok? tok-name tok-value)
            (printf "Parse Error: ~a:~a:~a\n" tok-ok? tok-name tok-value)))
   (precs (right AND OR)
          (nonassoc NOT))
   (grammar
    (exp [(ID)            $1]
         [(NOT exp)       `(NOT ,$2)]
         [(exp AND exp)   `(AND ,$1 ,$3)]
         [(exp OR exp)    `(OR ,$1 ,$3)]
         [(LPAR exp RPAR) $2]))))

(define (apply-lexer lexer input)
  (λ () (lexer input)))

(define (parse str)
  (my-parser (apply-lexer my-lexer (open-input-string str))))

(define apply-De-Morgans
  (match-lambda
    [(list 'OR a b)  `(AND ,(apply-De-Morgans a) ,(apply-De-Morgans b))]
    [(list 'AND a b) `(OR  ,(apply-De-Morgans a) ,(apply-De-Morgans b))]
    [(list 'NOT a)   a]
    [(list a)        `(NOT ,(apply-De-Morgans a))]
    [(? atom? a)     `(NOT ,a)]));ID's

(define prefix->infix
  (match-lambda
    [(list (list a ...) b ...) `(,(prefix->infix a) ,(prefix->infix b))]
    [(list 'OR a b)            `(,(prefix->infix a) OR ,(prefix->infix b))]
    [(list 'AND a b)           `(,(prefix->infix a) AND  ,(prefix->infix b))]
    [(list 'NOT a)             `(NOT ,(prefix->infix a))]
    [(? atom? a)               a]));leave atoms/ID's alone

(define (run str [level 3])
  (case level
    [(3) ((compose1 string-join (curry map symbol->string) flatten
                    prefix->infix apply-De-Morgans parse) str)]
    [(2) ((compose1 prefix->infix apply-De-Morgans parse) str)]
    [(1) ((compose1 apply-De-Morgans parse) str)]
    [(0) (parse str)]))


(module+ test
  (require rackunit)
  (for ([x '(("(a OR b AND c)"
              (OR a (AND b c)))
             ("a AND b AND c"
              (AND a (AND b c)))
             ("NOT (a AND b AND c)"
              (NOT (AND a (AND b c))))
             ("NOT (a OR b AND c) OR NOT(a AND NOT b)"
              (OR (NOT (OR a (AND b c))) (NOT (AND a (NOT b)))))
             ("NOT (a AND b OR c) AND NOT(NOT a OR a OR NOT b)"
              (AND (NOT (AND a (OR b c))) (NOT (OR (NOT a) (OR a (NOT b)))))))])
    (match-define (list input output) x)
    (check-equal? (my-parser (apply-lexer my-lexer (open-input-string input)))
                  output))
  
  (for ([x (in-list '("a"
                      "NOT a"
                      "a AND b"
                      "NOT a AND b"
                      "NOT (a AND b)"
                      "(a OR b AND c)"))]
        [y (in-list '("NOT a"
                      "a"
                      "NOT a OR NOT b"
                      "a OR NOT b"
                      "a AND b"
                      "NOT a AND NOT b OR NOT c"))])
    (check-equal? (run x) y)))
