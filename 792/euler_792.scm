;stream functions can be found under utils/
;tested on drracket (simply scheme profile)
(define (factorial n)
  (fact-iter n 1 1))

(define (fact-iter n start accum)
  (if (> start n)
      accum
      (fact-iter n (+ start 1) (* start accum))))


(define (map-stream proc str)
  (if (null? str)
      the-empty-stream
      (cons-stream (proc (stream-car str))
                   (map-stream proc (stream-cdr str)))))


(define (findlargest-r r)
  "Calculates v2"
  (find-r-helper r 0))
(define (find-r-helper r accum)
  (if (odd? r)
      accum
      (find-r-helper (/ r 2) (+ 1 accum))))
                     
(define (u-n n)
  "get u(n)"
  (findlargest-r (+ (* (s-n n) 3) 4)))

(define (cube n)
  (* n n n))

(define (s-n n)
  (accumulate-stream +
                     0
                     (map-stream
                      (lambda (e)
                        (* (expt -2 e)
                              (/ (B! e (* 2 e))
                                 (factorial e)) )) 
                      (stream-enumerate-interval 1 n))))


; (B! x) => 2x!/x!)

(define (B! start end)
  "returns factorial(end)/factorial(start)"
  (b!-helper (+ start 1 ) end 1))
(define (b!-helper start end accum)
  (if (> start end)
      accum
      (b!-helper (+ start 1) end (* start accum ))))

;(trace factorial)
(define (CapitalU-n n)
   (accumulate-stream + 0 (map-stream (lambda  (i) (u-n (cube i)))

                           (stream-enumerate-interval 1 n))))

;test
;(CapitalU-n 5)
;=> 241
