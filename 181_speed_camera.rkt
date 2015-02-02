#lang racket
(require library/library)

(define (convert-speed lst)
  (match-define (list num unit) lst)
  (if (string=? unit "km/h.")
      (string->number num)
      (* 1.609344 (string->number num))))

(define (parse str)
  (define lst (string-split str "\n"))
  (define limit (convert-speed (cdddr (string-split (first lst)))))
  (define cameras 
    (map (λ(x) (map string->number (cdr x))) 
         (filter-map (curry regexp-match #px"Speed camera number (\\d+) is (\\d+) metres .+") lst)))
  (define logs
    (map cdr 
         (filter-map (curry regexp-match
                            #px"^Vehicle (\\w+ \\w+) passed camera (\\d+) at (\\d\\d:\\d\\d:\\d\\d)")
          lst)))
  (list limit cameras logs))

(define (time->seconds str)
  (define lst (map (λ(x) (string->number (string x)))
                   (string->list (string-replace str ":" ""))))
  (match-define (list h2 h1 m2 m1 s2 s1) lst)
  (+ s1 (* s2 10) (* m1 60) (* m2 600) (* h1 3600) (* h2 36000)))

(define (reduce-log lst)
  (define name (first (first lst)))
  (define times (map time->seconds 
                     (map second lst)))
  
  (define delta-times (delta-reduce times))
  
  (list name delta-times))

(define (solve lst)
  (match-define (list speed-limit cameras logs) lst)
  (define camera-deltas (generate-camera-deltas cameras))
  (define max-times (generate-max-times-in-seconds speed-limit camera-deltas))
  (define grouped-logs
    ((compose1 (curryr chunk 4) (curry map (λ(x) (list (first x) (third x))))
               (curry map (curryr string-split "-")) (curryr sort string<?)
                      (curry map (curryr string-join "-")))
     logs))
  
  (define reduced-logs (map reduce-log grouped-logs))
  ;(pretty-print (list limit cameras camera-deltas max-times reduced-logs))
  (append-map (λ (lst)
                (match-define (list name splits) lst)
                (for/list ([time splits] [limit max-times] [dist camera-deltas]
                                         #:when (< time limit))
                  (let* ([speeding-speed-km-hour (* (/ dist time) 3600)]
                         [speeding-over (- speeding-speed-km-hour speed-limit)]
                         [speeding-in-mph (/ speeding-over 1.609344)])
                    (format "~a speeding by ~a km/h (~a mp/h)." name 
                            (~a speeding-over #:max-width 4) (~a speeding-in-mph #:max-width 4)))))
              reduced-logs))

(define (generate-max-times-in-seconds limit lst)
  (map (λ(x) (* (expt 60 2) (/ x limit)))
       lst))

(define (generate-camera-deltas lst)
  (map (curryr / 1000)
       (delta-reduce (map cadr lst))))

(define (delta-reduce lst #:start [starting-value #f])
  ((compose1 reverse second)
   (foldl (match-lambda** 
           [(x (list so-far points))
            (list x (cons (- x so-far) points))])
          (list (or starting-value (car lst)) '())
          lst)))

(define (run)
  (solve (parse tst)))


(define tst "Speed limit is 60.00 mph.
Speed camera number 1 is 0 metres down the motorway.
Speed camera number 2 is 600 metres down the motorway.
Speed camera number 3 is 855 metres down the motorway.
Speed camera number 4 is 1355 metres down the motorway.
Start of log for camera 1.
Vehicle G122 IVL passed camera 1 at 09:36:12.
Vehicle H151 KEE passed camera 1 at 09:36:15.
Vehicle U109 FIJ passed camera 1 at 09:36:20.
Vehicle LO04 CHZ passed camera 1 at 09:36:23.
Vehicle I105 AEV passed camera 1 at 09:36:28.
Vehicle J828 EBC passed camera 1 at 09:36:29.
Vehicle WF EP7 passed camera 1 at 09:36:32.
Vehicle H108 KYL passed camera 1 at 09:36:33.
Vehicle R815 FII passed camera 1 at 09:36:34.
Vehicle QW04 SQU passed camera 1 at 09:36:34.
Start of log for camera 2.
Vehicle G122 IVL passed camera 2 at 09:36:42.
Vehicle LO04 CHZ passed camera 2 at 09:36:46.
Vehicle H151 KEE passed camera 2 at 09:36:51.
Vehicle QW04 SQU passed camera 2 at 09:36:53.
Vehicle J828 EBC passed camera 2 at 09:36:53.
Vehicle R815 FII passed camera 2 at 09:36:55.
Vehicle U109 FIJ passed camera 2 at 09:36:56.
Vehicle H108 KYL passed camera 2 at 09:36:57.
Vehicle I105 AEV passed camera 2 at 09:37:05.
Vehicle WF EP7 passed camera 2 at 09:37:10.
Start of log for camera 3.
Vehicle LO04 CHZ passed camera 3 at 09:36:55.
Vehicle G122 IVL passed camera 3 at 09:36:56.
Vehicle H151 KEE passed camera 3 at 09:37:03.
Vehicle QW04 SQU passed camera 3 at 09:37:03.
Vehicle J828 EBC passed camera 3 at 09:37:04.
Vehicle R815 FII passed camera 3 at 09:37:09.
Vehicle U109 FIJ passed camera 3 at 09:37:11.
Vehicle H108 KYL passed camera 3 at 09:37:12.
Vehicle I105 AEV passed camera 3 at 09:37:20.
Vehicle WF EP7 passed camera 3 at 09:37:23.
Start of log for camera 4.
Vehicle LO04 CHZ passed camera 4 at 09:37:13.
Vehicle QW04 SQU passed camera 4 at 09:37:24.
Vehicle J828 EBC passed camera 4 at 09:37:26.
Vehicle G122 IVL passed camera 4 at 09:37:28.
Vehicle R815 FII passed camera 4 at 09:37:28.
Vehicle H151 KEE passed camera 4 at 09:37:29.
Vehicle H108 KYL passed camera 4 at 09:37:36.
Vehicle I105 AEV passed camera 4 at 09:37:42.
Vehicle WF EP7 passed camera 4 at 09:37:44.
Vehicle U109 FIJ passed camera 4 at 09:37:45.")


(run)