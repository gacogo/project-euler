(define (accumulate combiner init seq)
  (if (null? seq)
      init
      (combiner (car seq)
          (accumulate combiner init (cdr seq)))))

(define (divisible? n divisor)
  (= (remainder n divisor) 0))

(define (div3/5? n)
  (or (divisible? n 3) (divisible? n 5))) 

(define (multiples-3-5 n)
  (accumulate
   (lambda (a  b) (+ a b))
   0
   (filter (lambda (e) (div3/5? e))
           (enumerate-interval 0 n))))

;solution
(multiples-3-5 999)
