#lang racket/gui
(provide sudoku-vector
         make-2d-vector
         2d-vector-ref
         2d-vector-set!
         solve-sudoku
         make-copy
         pp
         y
         z
         for
         list->2dvec
         transpose
         2dvec->list
         is_sudoku_sol)
(require "sudokusol.scm")
(define (make-copy x y)
  (for (define i 0) : (< i 9) : (set! i (+ i 1)) :
    (for (define j 0) : (< j 9) : (set! j (+ j 1)) : (2d-vector-set! x i j (2d-vector-ref y i j))
      )))

(define (make-2d-vector rows col def-val)
  (build-vector rows (lambda (x) (make-vector col def-val))))
(define (2d-vector-ref vec-name row col)
  (vector-ref (vector-ref vec-name row) col))
(define (2d-vector-set! vec-name row col new-val)
  (vector-set! (vector-ref vec-name row) col new-val))
(define-syntax for 
  (syntax-rules (:)
    [(for init : bexp : change : stmts)  (begin init
                                                (define (loop)
                                                  (cond [bexp (begin stmts
                                                                     change
                                                                     (loop))]))
                                                (loop))]))
(define i 0)
(define j 0)
(define ans-vector
  (make-2d-vector 9 9 '(1 2 3 4 5 6 7 8 9)))
(define sudoku-vector
  (make-2d-vector 9 9 '(1 2 3 4 5 6 7 8 9)))
(define (solve-sudoku taken-vector)
  (define (make-initalization taken-vector)
    (define vec taken-vector)
    (for (begin (set! i 0)) :(< i 9) : (begin  
                                         (set! vec (cdr vec))
                                         (set! i (+ i 1)))
      : (for (begin (set! j 0)) : (< j 9)
          : (set! j (+ j 1)) :
          (cond [(vector-ref (list->vector (car vec)) j)
                 (2d-vector-set! sudoku-vector i j (vector-ref (list->vector(car  vec)) j))]))))
  (define (take-off l1 l2)
    (define kk l2)
    (if (number? l2) l2
        (if (null? l1) kk
            (if (number? l1) (begin (set! kk (remove l1 kk))
                                    kk)
                (begin (set! kk (remove (car l1) kk))
                       (take-off (cdr l1) kk))))))
  (define (no_of_subset ele vec)
    (define x 0)
    (begin (for (define k 0) : (< k (vector-length vec)) :(set! k (+ k 1)) :
             (cond [(number? (vector-ref vec k)) (void)]
                   [(null? (take-off ele (vector-ref vec k))) (set! x (+ x 1))]))
           x))
  (define (twin-row  vec i no_of_elements)
    (define normal-vec ( vector-ref vec i))
    (define twinlist '())
    (for (define j 0) : (< j 9) :(set! j (+ j 1)):
      (cond [(number? (vector-ref
                       normal-vec j)) (void)]
            [(= (length (vector-ref
                         normal-vec j)) no_of_elements)  (begin (if (= (no_of_subset  (vector-ref normal-vec  j) normal-vec)
                                                                       no_of_elements)
                                                                    (set! twinlist (vector-ref normal-vec  j))
                                                                    (set! twinlist '()))
                                                                (for (define kk 0) : (< kk 9) :(set! kk (+ kk 1)) :
                                                                  (if (null? twinlist) (void)
                                                                      (if (null? (take-off twinlist (2d-vector-ref vec i kk))) (void)
                                                                          (if (number? (2d-vector-ref vec i kk)) (void)
                                                                              (2d-vector-set! vec i kk (if (= (length (take-off  twinlist (2d-vector-ref vec i kk))) 1)
                                                                                                           (car(take-off  twinlist (2d-vector-ref vec i kk)))
                                                                                                           (take-off  twinlist (2d-vector-ref vec i kk)))))))))])))
  
  (define (twin-col vec i no_of_elements)
    (define normal-vec (make-vector 9 0)) 
    (for (define pra 0) : (< pra 9) :(set! pra (+ pra 1)) :
      (vector-set! normal-vec pra (2d-vector-ref vec pra i)))
    (define twinlist '())
    (for (define j 0) : (< j 9) :(set! j (+ j 1)):
      (cond [(number? (vector-ref
                       normal-vec j)) (void)]
            [(= (length (vector-ref
                         normal-vec j)) no_of_elements)  (begin (if 
                                                                 (= (no_of_subset  (vector-ref normal-vec  j) normal-vec)
                                                                    no_of_elements)
                                                                 
                                                                 (set! twinlist (vector-ref normal-vec  j))
                                                                 (set! twinlist '()))
                                                                (for (define kk 0) : (< kk 9) :(set! kk (+ kk 1)) :
                                                                  (if (null? twinlist) (void)
                                                                      (if (null? (take-off twinlist (2d-vector-ref vec kk i))) (void)
                                                                          (if (number? (2d-vector-ref vec kk i)) (void)
                                                                              (2d-vector-set! vec kk i (if (= (length (take-off  twinlist (2d-vector-ref vec kk i))) 1)
                                                                                                           (car(take-off  twinlist (2d-vector-ref vec kk i)))
                                                                                                           (take-off  twinlist (2d-vector-ref vec kk i)))))))))])))
  (define (get-grid vec no)
    (define help (make-vector 9 0))
    (define coeff 0)
    (begin (for (define pp (* 3 (quotient (- no 1) 3))) : (< pp (+ (* 3 (quotient (- no 1) 3)) 3)) : (set! pp (+ pp 1)) :
             (for (define k (* 3 (remainder (- no 1) 3))) : (< k (+ (* 3 (remainder (- no 1) 3)) 3)) : (set! k (+ k 1)) :
               (begin (vector-set! help coeff (2d-vector-ref vec pp k))
                      (set! coeff (+ coeff 1)))))
           help))
  (define (twin-box vec d no_of_elements)
    (define normal-vec (get-grid vec d)) 
    (define twinlist '())
    (for (define j 0) : (< j 9) :(set! j (+ j 1)):
      (cond [(number? (vector-ref
                       normal-vec j)) (void)]
            [(= (length (vector-ref
                         normal-vec j)) no_of_elements)  (begin (if 
                                                                 (= (no_of_subset  (vector-ref normal-vec  j) normal-vec)
                                                                    no_of_elements)
                                                                 
                                                                 (set! twinlist (vector-ref normal-vec  j))
                                                                 (set! twinlist '()))
                                                                (for (define pp (* 3 (quotient (- d 1) 3))) : (< pp (+ (* 3 (quotient (- d 1) 3)) 3)) : (set! pp (+ pp 1)) :
                                                                  (for (define k (* 3 (remainder (- d 1) 3))) : (< k (+ (* 3 (remainder (- d 1) 3)) 3)) : (set! k (+ k 1)) :
                                                                    (if (null? twinlist) (void)
                                                                        (if (null? (take-off twinlist (2d-vector-ref vec pp k))) (void)
                                                                            (if (number? (2d-vector-ref vec pp k)) (void)
                                                                                (2d-vector-set! vec pp k (if (= (length (take-off  twinlist (2d-vector-ref vec pp k))) 1)
                                                                                                             (car(take-off  twinlist (2d-vector-ref vec pp k)))
                                                                                                             (take-off  twinlist (2d-vector-ref vec pp k))))))))))])))
  (define (search x l)
    (if (number? l) #f
        (if (null? l )#f
            (if (equal? x (car l))#t
                (search x (cdr l))))))
  (define (lr-box vec i j)
    (define boolean #f)
    (define pp 0)
    (define k 0)
    (define x (2d-vector-ref vec i j))
    (define ans 0)
    (define helpx (void))
    (define (procedure ppinit pplt kkinit kklt)
      (cond [(number? x) (void)]
            [else (begin (for (void) : (and 
                                        (= ans 0)
                                        (not (null? x))) :(begin (set! helpx x)
                                                                 (set! x (cdr x))
                                                                 (set! boolean #f)) :
                           (for (set! pp ppinit) : (< pp pplt) : (set! pp (+ pp 1)) :
                             (for (set! k kkinit) : (< k kklt) : (set! k (+ k 1)) :
                               (begin (cond [(and  (= pp 0)
                                                   (= k 0)) (void)]
                                            [else (set! boolean (or boolean
                                                                    (search (car x) (2d-vector-ref vec (+ i pp)  (+ j k)))))])
                                      (cond [(and (= pp (- pplt  1))
                                                  (= k (- kklt 1))) (if (not boolean) (set! ans 1)
                                                                        (void))])))))
                         (if (null? x) (void)
                             (2d-vector-set! vec i j  (car helpx))))]))
    (cond  [(and (= (remainder i 3) 0) 
                 (= (remainder j 3) 0)) (procedure 0 3  0 3)]
           [(and (= (remainder i 3) 0) 
                 (= (remainder j 3) 1)) (procedure 0 3 -1  2)]
           [(and (= (remainder i 3) 0) 
                 (= (remainder j 3) 2)) (procedure 0 3 -2 1)]
           [(and (= (remainder i 3) 1) 
                 (= (remainder j 3) 0)) (procedure -1 2  0 3)]
           [(and (= (remainder i 3) 1) 
                 (= (remainder j 3) 1)) (procedure -1 2 -1  2)]
           [(and (= (remainder i 3) 1) 
                 (= (remainder j 3) 2)) (procedure -1 2 -2 1)]
           [(and (= (remainder i 3) 2) 
                 (= (remainder j 3) 0)) (procedure -2 1  0 3)]
           [(and (= (remainder i 3) 2) 
                 (= (remainder j 3) 1)) (procedure -2 1 -1  2)]
           [(and (= (remainder i 3) 2) 
                 (= (remainder j 3) 2)) (procedure -2 1 -2 1)]))
  (define (lr-col vec i j)
    (define k 0)
    (define boolean #f)
    (define helpx (void))
    (define ans 0)
    (define x (2d-vector-ref vec i j)) 
    (cond [(number? x) (void)]
          [else  (begin (for (void) : (and 
                                       (= ans 0)
                                       (not (null? x))) :(begin (set! helpx x)
                                                                (set! x (cdr x))
                                                                (set! boolean #f)) : 
                          (for (set! k 0) : (< k 9) : (set! k (+ k 1)) : (begin (cond [  (= k i)
                                                                                         (void)]
                                                                                      [else (set! boolean (or boolean
                                                                                                              (search (car x) (2d-vector-ref vec k  j))))])
                                                                                (cond [(= k 8) (if (not boolean) (set! ans 1)
                                                                                                   (void))])))
                          )
                        (if (null? x) (void)
                            (2d-vector-set! vec i j  (car helpx))))]))
  (define (lr-row vec i j)
    (define k 0)
    (define boolean #f)
    (define helpx (void))
    (define ans 0)
    (define x (2d-vector-ref vec i j)) 
    (cond [(number? x) (void)]
          [else (begin (for (void) : (and 
                                      (= ans 0)
                                      (not (null? x))) :(begin (set! helpx x)
                                                               (set! x (cdr x))
                                                               (set! boolean #f)) : 
                         (for (set! k 0) : (< k 9) : (begin 
                                                       
                                                       (set! k (+ k 1))) : (begin (cond [  (= k j)
                                                                                           (void)]
                                                                                        [else (set! boolean (or boolean
                                                                                                                (search (car x) (2d-vector-ref vec i k))))])
                                                                                  (cond [(= k 8) (if (not boolean) (set! ans 1)
                                                                                                     (void))])))
                         )
                       (if (null? x) (void)
                           (2d-vector-set! vec i j  (car helpx))))]))
  
  (define (check-col vec i j)
    (define k 0)
    (begin (for (set! k 0) : (< k 9) : (set! k (+ k 1)) : (cond [(list? (2d-vector-ref vec i j))
                                                                 (if (= ( length (2d-vector-ref vec i j)) 1) (2d-vector-set! vec i j 
                                                                                                                             (car (2d-vector-ref vec i j)))
                                                                     (if (number? (2d-vector-ref vec k j))
                                                                         (2d-vector-set! vec i j 
                                                                                         (remove  (2d-vector-ref vec k j) 
                                                                                                  (2d-vector-ref vec i j)))
                                                                         (void)))]
                                                                [else (void)]))
           ))
  (define (check-row vec i j)
    (define k 0)
    (begin (for (set! k 0) : (< k 9) : (set! k (+ k 1)) : (cond [(list? (2d-vector-ref vec i j))
                                                                 (if (= ( length (2d-vector-ref vec i j)) 1) (2d-vector-set! vec i j 
                                                                                                                             (car (2d-vector-ref vec i j)))
                                                                     (if (number? (2d-vector-ref vec i k))
                                                                         (2d-vector-set! vec i j 
                                                                                         (remove  (2d-vector-ref vec i k) 
                                                                                                  (2d-vector-ref vec i j)))
                                                                         (void)))]
                                                                [else (void)]))
           ))
  (define (check-box vec i j)
    (define pp 0)
    (define k 0)
    (define (procedure ppinit pplt kkinit kklt)
      (for (set! pp ppinit) : (< pp pplt) : (set! pp (+ pp 1)) :
        (for (set! k kkinit) : (< k kklt) : (set! k (+ k 1)) :
          (cond [(list? (2d-vector-ref vec i j))
                 (if (= ( length (2d-vector-ref vec i j)) 1) (2d-vector-set! vec i j 
                                                                             (car (2d-vector-ref vec i j)))
                     (if (number? (2d-vector-ref vec (+ i pp) (+ j k)))
                         (2d-vector-set! vec i j 
                                         (remove  (2d-vector-ref vec (+ i pp) (+ j k))
                                                  (2d-vector-ref vec i j)))
                         (void)))]
                [else (void)]))))
    
    (cond  [(and (= (remainder i 3) 0) 
                 (= (remainder j 3) 0)) (procedure 0 3  0 3)]
           [(and (= (remainder i 3) 0) 
                 (= (remainder j 3) 1)) (procedure 0 3 -1  2)]
           [(and (= (remainder i 3) 0) 
                 (= (remainder j 3) 2)) (procedure 0 3 -2 1)]
           [(and (= (remainder i 3) 1) 
                 (= (remainder j 3) 0)) (procedure -1 2  0 3)]
           [(and (= (remainder i 3) 1) 
                 (= (remainder j 3) 1)) (procedure -1 2 -1  2)]
           [(and (= (remainder i 3) 1) 
                 (= (remainder j 3) 2)) (procedure -1 2 -2 1)]
           [(and (= (remainder i 3) 2) 
                 (= (remainder j 3) 0)) (procedure -2 1  0 3)]
           [(and (= (remainder i 3) 2) 
                 (= (remainder j 3) 1)) (procedure -2 1 -1  2)]
           [(and (= (remainder i 3) 2) 
                 (= (remainder j 3) 2)) (procedure -2 1 -2 1)]))
  (define (check vec )
    (define (help i j)
      (if (number? (2d-vector-ref vec i j)) (void)
          (begin  (check-col vec i j)
                  (check-row  vec i  j)
                  (check-box vec i j))))
    (for (define i 0) : (< i 9) : (set! i (+ i 1)) :
      (for (define j 0) : (< j 9) : (set! j (+ j 1)) : (help i j))))
  (define (check1 vec)
    (define p (make-2d-vector 9 9 0))
    (make-copy p vec)
    (for (define i 0) : (and 
                         (< i 9)
                         (equal? p vec)): (set! i (+ i 1)) :
      (for (define j 0) : (and (< j 9)
                               (equal? p vec)): (set! j (+ j 1)) : (lr-box vec i j))))
  (define (check2 vec)
    (define p (make-2d-vector 9 9 0))
    (make-copy p vec)
    (for (define i 0) : (and 
                         (< i 9)
                         (equal? p vec)) : (set! i (+ i 1)) :
      (for (define j 0) : (and (< j 9)
                               (equal? p vec)) : (set! j (+ j 1))
        : (lr-row vec i j))))
  (define (check3 vec)
    (define p (make-2d-vector 9 9 0))
    (make-copy p vec)
    (for (define i 0) : (and 
                         (< i 9)
                         (equal? p vec)) : (set! i (+ i 1)) :
      (for (define j 0) : (and (< j 9)
                               (equal? p vec)) : (set! j (+ j 1)) : (lr-col vec i j))))
  (define (check4 vec)
    (for (define i 0) : (< i 9) : (set! i (+ i 1)) :
      (twin-box vec (+ i 1) 2)))
  (define (check5 vec)
    (for (define i 0) : (< i 9) : (set! i (+ i 1)) :
      (twin-row vec i 2)))
  (define (check6 vec)
    (for (define i 0) : (< i 9) : (set! i (+ i 1)) :
      (twin-col vec i 2)))
  (define (check7 vec)
    (for (define i 0) : (< i 9) : (set! i (+ i 1)) :
      (twin-box vec (+ i 1) 3)))
  (define (check8 vec)
    (for (define i 0) : (< i 9) : (set! i (+ i 1)) :
      (twin-row vec i 3)))
  (define (check9 vec)
    (for (define i 0) : (< i 9) : (set! i (+ i 1)) :
      (twin-col vec i 3)))
  (define (bf-elem vec)
    (define (dinu-flaw l i j)
      (if (= (length l) 1) (begin 
                             (2d-vector-set! vec i j (car l))
                             )
          (begin (2d-vector-set! pp i j (car l))
                 (if (is_sudoku_sol (2dvec->list (begin (check_s pp)
                                                        pp))) (make-copy vec pp)
                                                              (begin 
                                                                (make-copy pp vec)
                                                                (dinu-flaw (cdr l) i j))))))
    (define pp (make-2d-vector 9 9 0))
    (make-copy pp vec)
    (for (define i 0) : (< i 9) : (set! i (+ i 1)) :
      (for (define j 0) : (< j 9) : (set! j (+ j 1)) :
        (if (number? (2d-vector-ref vec i j)) (void)
            (dinu-flaw (2d-vector-ref vec i j) i j)))))
  
  (define (make-change vec)
    (define prev (make-2d-vector 9 9 0))
    (make-copy prev vec)
    (begin (check1 vec)
           (if (equal? prev vec) 
               (begin 
                 (check2 vec)
                 (if (equal? prev vec) 
                     (begin (check3 vec)
                            (if (equal? prev vec) 
                                (begin (check4 vec)
                                       (if (equal? prev vec) 
                                           (begin (check5 vec)
                                                  (if (equal? prev vec) 
                                                      (begin (check6 vec)
                                                             (if (equal? prev vec) 
                                                                 (begin (check7 vec)
                                                                        (if (equal? prev vec) 
                                                                            (begin (check8 vec)
                                                                                   (if (equal? prev vec)
                                                                                       (begin (check9 vec)
                                                                                              (if (equal?  prev vec)
                                                                                                  (bf-elem vec)
                                                                                                  (void)))
                                                                                       (void)))
                                                                            (void)))
                                                                 
                                                                 (void)))
                                                      (void)))
                                           (void)))
                                
                                (void)))
                     (void)))
               (void))))
  (define (check_s vec )
    (begin (for (define prev_sudo  (make-2d-vector 9 9 0)) :   (anylist? vec)
             
             : (void) : (if (equal?  prev_sudo vec)
                            (begin (make-change vec)
                                   (make-copy prev_sudo (make-2d-vector 9 9 0))
                                   (check vec))
                            (begin 
                              (make-copy prev_sudo 
                                         vec)
                              (check vec)
                              )))))
  (define kkkk 0)
  (begin  (make-initalization taken-vector)
          (check_s sudoku-vector)
          (make-copy ans-vector sudoku-vector)
          (make-copy sudoku-vector (make-2d-vector 9 9 '(1 2 3 4 5 6 7 8 9)))
          ans-vector))
(define (anylist? vec)
  (define list-of-vecs (vector->list vec))
  (define (help l)
    (if (= (length l) 1) (list? (car l)) 
        (or  (list? (car l)) (help (cdr l)))))
  (define (help2 l)
    (if (= (length l) 1) (help (vector->list (car l)))
        (or (help (vector->list (car l))) (help2 (cdr l)))))
  (help2 list-of-vecs))


(define y 
  (list '(#f 5 #f #f #f 7 4 #f #f)
        '( #f #f #f #f #f 1 #f #f 5)
        '(#f  3 #f #f #f 6  #f #f 8)
        '(#f 2 #f #f 9 #f 1 #f #f)
        '(5 #f 6 #f #f #f 9 #f 3)
        '(#f #f 7 #f 4 #f #f 2 #f)
        '(6 #f 3  2 #f #f #f 5 #f)
        '( 8 #f #f 5 #f #f #f #f #f)      
        '(#f #f 5 3 #f #f #f 7 #f)))
(define pp (list 
            '(5 3  #f #f 7 #f #f #f #f)
            '(6 #f #f 1 9 5 #f #f #f)
            '(#f 9 8  #f #f #f #f 6 #f)
            '(8 #f #f #f 6 #f #f #f 3)
            '(4 #f #f 8 #f 3 #f #f 1)
            '(7 #f #f #f 2 #f #f #f 6)
            '(#f 6 #f #f #f #f 2 8 #f)
            '(#f #f #f 4 1 9 #f #f 5)
            '(#f #f #f #f 8 #f #f 7 9)))
(define z (list
           '(#f 6 #f 3 #f #f 8 #f 4)
           '(5 3 7 #f  9 #f #f #f #f)
           '(#f 4 #f #f #f 6 3 #f 7)
           '(#f 9 #f #f 5 1 2 3 8)
           '(#f #f #f #f #f #f #f #f #f)
           '(7 1 3 6 2 #f #f 4 #f)
           '(3 #f 6 4 #f #f #f 1 #f)
           '(#f #f #f #f 6 #f 5 2 3)
           '(1 #f 2 #f #f 9 #f 8 #f)))