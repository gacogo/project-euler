;stream functions can be found under utils/
(define (findlargest-r r)
  (find-r-helper r 0))
(define (find-r-helper r accum)
  (if (odd? r)
      accum
      (find-r-helper (/ r 2) (+ 1 accum))))
                     

(define v2 findlargest-r)

(define (u-n n)
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
  (b!-helper (+ start 1 ) end 1))
(define (b!-helper start end accum)
  (if (> start end)
      accum
      (b!-helper (+ start 1) end (* start accum ))))
;(trace accumulate-stream)
;(trace accumulate)
(trace u-n)
(trace findlargest-r)
(trace B!)
(trace s-n)
;(trace factorial)
(define (CapitalU-n n)
   (accumulate-stream + 0 (map-stream (lambda  (i) (u-n (cube i)))

                           (stream-enumerate-interval 1 n))))

;test
;(CapitalU-n 5)
;=> 241