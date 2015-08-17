#lang racket
(provide geneaterandom_sudoku)
(require "programwithflaws.scm")
(define (geneaterandom_sudoku)
  (define no (random 100))
  (define rand_sudo (make-2d-vector 9 9 #f))
    (begin (cond [(= (remainder no 3) 0)
        (make-copy rand_sudo (solve-sudoku pp))]
          [(= (remainder no 3) 1)
           (make-copy rand_sudo (solve-sudoku y))]
          [(= (remainder no 3) 2)
           (make-copy rand_sudo (solve-sudoku z))])
           (randomize rand_sudo)
           rand_sudo))
(define  (randomize vec)
  (make-copy vec (create_spaces (change_numbers (flip (transform vec))))))
(define (transform vec)
  (define tranvec (list->2dvec (transpose (2dvec->list vec))))
  (define bigrandno (random 3000000))
  (cond [(= 0 (remainder bigrandno 8))
         vec]
        [(= 1 (remainder bigrandno 8))
         (mirror vec)]
        [(= 2 (remainder bigrandno 8))
         (rotate90 vec)]
        [(= 3 (remainder bigrandno 8))
         tranvec]
        [(= 4 (remainder bigrandno 8))
         (mirror tranvec)]
        [(= 5 (remainder bigrandno 8))
         (rotate90 tranvec)]
        [(= 6 (remainder bigrandno 8))
         (rotate90 (mirror vec))]
        [(= 7 (remainder bigrandno 8))
         (rotate90 (mirror tranvec))]))
(define (flip vec)
  (define bigrand (random 79999))
  (cond [(= 0 (remainder bigrand 3))
            (row-wap (row-wap vec))]
        [(= 1 (remainder bigrand 3))
            (col-wap (row-wap vec))]
        [(= 2 (remainder bigrand 3))
            (col-wap (col-wap vec))]))
        
         
(define (myrandom no)
  (define h (random (- no 1)))
  (+ h 1))
(define (change_numbers vec)
  (define kk (myrandom 9))
  (define kk1 (myrandom 9))
  (begin (if (equal? kk kk1) (if (= kk 9) (set! kk1 (- kk 1))
                          (set! kk1 (+ kk 1)))
      (void))
         (for (define i 0) :(< i 9) :(set! i (+ i 1)) :
           (for (define j 0) : (< j 9) : (set! j (+ j 1)) :
             (if (= (2d-vector-ref vec i j) kk) (2d-vector-set! vec i j kk1)
                 (if (= (2d-vector-ref vec i j) kk1) (2d-vector-set! vec i j kk)
                     (void)))))
         vec))
(define (mirror vec)
  (list->vector (foldr (lambda (x y) (cons (list->vector (reverse x)) y)) '() (2dvec->list vec))))
(define (rotate90 vec)
  (list->vector (foldr (lambda (x y) (cons (list->vector (reverse x)) y)) '() (transpose (2dvec->list vec)))))
(define (rev vec)
  (list->vector (reverse (vector->list vec))))
(define (create_spaces vec)
  (begin (for (define i 0) :(< i 9) :(set! i (+ i 1)) :
           (for (define j 0) : (< j 9) : (set! j (+ j 1)) :
             (if (or 
                  (= 6 (remainder (random 30000) 7))
                  (= 4 (remainder (random 30000) 6))
                     (= 2 (remainder (random 30000) 6))
                     (= 0 (remainder (random 30000) 6)))(2d-vector-set! vec i j #f)
                 (void))))
         vec))
(define (row-wap vec)
  (define pp (random 3))
  (define replace (vector-ref vec (+ (* 3 pp ) pp)))
  (begin (if (= pp 0) (begin 
                 (vector-set! vec 0 (vector-ref vec 1))
                 (vector-set! vec 1 replace))
      (if (= pp 1) (begin 
                 (vector-set! vec 4 (vector-ref vec 5))
                 (vector-set! vec 5 replace))
          (begin 
                 (vector-set! vec 8 (vector-ref vec 7))
                 (vector-set! vec 7 replace))))
         vec))
(define (col-wap vec)
  (define vec1 (list->2dvec (transpose (2dvec->list vec))))
  (define pp (random 3))
  (define replace (vector-ref vec1 (+ (* 3 pp ) pp)))
  (begin (if (= pp 0) (begin 
                 (vector-set! vec1 0 (vector-ref vec1 1))
                 (vector-set! vec1 1 replace))
      (if (= pp 1) (begin 
                 (vector-set! vec1 4 (vector-ref vec1 5))
                 (vector-set! vec1 5 replace))
          (begin 
                 (vector-set! vec1 8 (vector-ref vec1 7))
                 (vector-set! vec1 7 replace))))
         (make-copy vec (list->2dvec (transpose (2dvec->list vec1))))
         vec))