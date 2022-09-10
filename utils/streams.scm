; using stream data structure. Here are the utils for streams

(define stream-null? null?)
(define the-empty-stream '())
(define true #t)
(define false #f)

(define (memo-proc proc)
  (let ((already-run? false)
        (result false))
    (lambda ()
      (if already-run?
          result
          (begin
            (set! result (proc))
            (set! already-run? true)
          
            result)))))

(define-syntax delay
  (syntax-rules
      ()
    ((delay exp)
     (memo-proc (lambda () exp)))))

(define (force delayed-object)
  (delayed-object))

(define-syntax cons-stream
  (syntax-rules
      ()
    ((cons-stream a b)
     (cons a (delay b)))))

(define (stream-car stream)
  (car stream))
(define (stream-cdr stream)
  (force (cdr stream)))


(define (stream-ref stream n)
  (if (= n 0)
      (stream-car stream)
      (stream-ref (stream-cdr stream) (- n 1))))

(define (stream-map proc . argstream)
  (if (stream-null? (car argstream))
      the-empty-stream
      (cons-stream (apply proc (map stream-car argstream))
                   (apply stream-map
                          (cons proc (map stream-cdr argstream))))))
               
(define (stream-for-each proc stream)
  (if (stream-null? stream)
      'done
      (begin
        (proc (stream-car stream))
        (stream-for-each proc (stream-cdr stream)))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low (stream-enumerate-interval (+ low 1) high))))
(define (display-stream stream)
  (stream-for-each display-line stream))

(define (display-line x)
  (newline)
  (display x))


(define (filter-stream pred str)
  (cond
    ((stream-null? str)
     the-empty-stream)
    ((pred (stream-car str))
     (cons-stream (stream-car str)
                  (filter-stream pred (stream-cdr str))))
    (else   
     (filter-stream pred (stream-cdr str)))))


(define (accumulate-stream combiner init stream)
  (if (stream-null? stream)
      init
      (combiner (stream-car stream)
                (accumulate-stream combiner init (stream-cdr stream)))))
