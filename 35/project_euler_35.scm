
; For this problem, any number greater than 9
; and the number has an even digit in it  or a '5' are not circular primes by default
; Call these unwanted primes
; stream functions are provided by utils/streams.scm 

(define (unwanted-helper n)
  (or (even? n)
      (= (remainder n 5) 0)))

;unwanted rules are different for numbers < 10
(define (unwanted? n)
  (if (or (= n 2) (= n 5))
      #f
      (unwanted-loop n)))

(define (unwanted-loop m )
  (let ((r (remainder m 10))
        (q (quotient m 10)))
    (cond
      ((< m 10)
       (unwanted-helper m))
      ((unwanted-helper r) #t)
      (else
       (unwanted-loop q)))))
 
 ;given a block(list) test if 
 ; all numbers are prime
(define (prime-block? block)
  (cond ((null? block) #t)
        ((prime? (car block))
         (prime-block? (cdr block)))
        (else #f)))
        
(define (prime? n)
  (define (loop n i) "helper"
    (cond ((< n (* i i)) #t)
          ((zero? (remainder n i)) #f)
          (else
           (loop n (+ i 1)))))
 "primality test"
 (cond ((< n 2) #f)
     (else
      (loop n 2))))

;enumerate prime numbers between two numbers
(define (enumerate-primes start end)
  (filter-stream
   prime?
   (filter-stream
    (lambda (i)
      (not (unwanted? i)))
    (stream-enumerate-interval start end))))

;rotate a number once
(define (int-rotation int)
  "rotating the integer once"
  (define (loop  todo accum len  index)
    (let  ((rem (remainder todo 10))
           (q (quotient todo 10)))
      (cond ((= index len)
             (+ accum todo))
            (else
             (set! accum
                   (+
                    accum
                    (*
                     rem
                     (expt 10 index))))
             (loop q accum len (+ index 1))))))
  (loop int 0 (int-len int) 1))
                    
(define (int-len int)
  "get number of digits in a number"
  (if (< int 10)
      1
      (+ 1 (int-len (quotient int 10)))))

;given a number, return a list of elements of its
;circular permutation.
(define (int-circular-rotation m)
  "Get a full circular rotation of an integer"
    (define (inner-loop int accum len)
      (let ([rotation (int-rotation int)])
        (if (= len 0)
           accum
            (inner-loop rotation (cons  rotation accum) (- len 1)))))
    (inner-loop  m '() (int-len m)))

;stream of primes under n
(define (primes<= n)
  (enumerate-primes 2 n))

; a stream of circular primes under n
(define (circular-primes-stream<= n)
  (filter-stream
   (lambda (e)
     (prime-block? e))
   (stream-map int-circular-rotation
                (primes<= n))))


; euler_35 defines a stream with all the conditoons satisfied

;(define euler_35 (circular-primes-stream<= 1000000))

;uncomment to display the stream
;(display-stream euler_35)

;uncomment to show the solution takes a few seconds
;(accumulate-stream (lambda (a b) (+ 1 b))  0 euler_35) ; 55 <- 
