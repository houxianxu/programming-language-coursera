
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below
;; 1
(define (sequence low high stride)
  (cond [(> low high) null]
        [(= low high) (cons high null)]
        [#t (cons low (sequence (+ low stride) high stride))]))

;; 2
(define (string-append-map xs suffix)
  (map (lambda (string) (string-append string suffix)) xs))
        
;; 3
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (let ([i (remainder n (length xs))])
              (car (list-tail xs i)))]))

;; 4
(define (stream-for-n-steps s n)
        (if (= n 0)
            null
            (cons (car (s))
                  (stream-for-n-steps (cdr (s)) (- n 1)))))

;; 5
(define funny-number-stream
  (letrec 
      ([f (lambda (x) 
            (cons (if (= (remainder x 5) 0) (- x) x)
                  (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))
    
;; test 4 and 5
;; (stream-for-n-steps funny-number-stream 11)

;; 6
(define dan-then-dog
  (letrec
      ([f (lambda (x)
            (cons (if (= (remainder x 2) 0) "dog.jpg" "dan.jpg")
                  (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

;; 7
(define (stream-add-zero s)
  (letrec ([temp-ans (s)]
           [new-car (cons 0 (car temp-ans))]
           [f (lambda () (cons new-car (stream-add-zero (cdr temp-ans))))])
    f))

;; 8
(define (cycle-lists xs ys)
  (letrec ([f (lambda (i j)
                (letrec ([new-car (cons (list-nth-mod xs i) (list-nth-mod ys j))])
                  (cons new-car
                        (lambda () (f (+ i 1) (+ j 1))))))])
    (lambda () (f 0 0))))

;; 9
(define (vector-assoc v vec)
  (letrec ([len-vec (vector-length vec)]
           [f (lambda (pos)
                (cond [(= len-vec pos) #f]
                      [(pair? (vector-ref vec pos))
                       (if (equal? v (car (vector-ref vec pos)))
                           (vector-ref vec pos)
                           (f (+ pos 1)))]
                      [#t (f (+ pos 1))]))])
    (f 0)))

;; 10
(define (cached-assoc xs n)
  (letrec ([cache-vec (make-vector n #f)]
           [index 0]
           [f (lambda (v)
                (letrec ([ans (vector-assoc v cache-vec)])
                  (if ans
                      ans
                      (letrec ([new-ans (assoc v xs)])
                        (begin (set! index (remainder index n))
                               (vector-set! cache-vec index new-ans)
                               new-ans)))))])
    f))
                      
                  
                    
                
                
                
                

                       
                        
                     
                
