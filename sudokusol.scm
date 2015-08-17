#lang racket
(provide 2dvec->list
         is_sudoku_sol
         verify_input
         list->2dvec
         transpose)
(define-syntax for 
  (syntax-rules (:)
    [(for init : bexp : change : stmts)  (begin init
                                                (define (loop)
                                                  (cond [bexp (begin stmts
                                                                     change
                                                                     (loop))]))
                                                (loop))]))
(define (nth_col  n l)
  (foldr (lambda(x y) (append (list (get_nth_list n x)) y))  '() l))
(define (get_nth_list n l)
  [cond [(= n 1) (car l)]
        [else (get_nth_list (- n 1) (cdr l))]])
(define (search x l)
  (if (null? l )#f
      (if (equal? x (car l))#t
          (search x (cdr l)))))
(define (transpose m )
  (define no_col (length (car m)))
  (foldr (lambda ( x y)(append  y (list (nth_col (+ (length y) 1) m)) )) '() (car m)))
(define (whether_row_sudoku m) 
  (define (help matrix )
    (cond [ (null? matrix) #t]
          [(whether_a_row_sudoku  (car matrix)) (help (cdr matrix))]
          [ else #f]))
  ( help m  ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (whether_a_row_sudoku l)
  (define (help  n)
    (cond [ (> n 9) #t]
          [(search n l) (help  (+ n 1))]
          [ else #f]))
  (help 1)) 

(define (make_3squares_sudoku list1 list2 list3 )
  (define (help l1 l2 l3 l n)
    (if (= n 0) (if (null? l1) (list l)
                    (append (list l)  ( help (cdr l1) (cdr l2) (cdr l3)  (nth_col 1 (list l1 l2 l3)) 2 )))
        ( help (cdr l1) (cdr l2) (cdr l3)  (append l  (nth_col 1  (list l1 l2 l3)) ) (- n 1) )))
  (help list1 list2 list3 '() 3))
(define (make_out_ofcolns m)
  ( append  (append (make_3squares_sudoku (nth_col 1 m) (nth_col 2 m) (nth_col 3 m) )
                    (make_3squares_sudoku (nth_col 4 m) (nth_col 5 m) (nth_col 6 m) ))
            (make_3squares_sudoku (nth_col 7 m) (nth_col 8 m) (nth_col 9 m) )))
(define (repeat_ele ele lst)
  (if (null? lst) 0
      (if (equal? ele (car lst)) (+ 1 (repeat_ele ele (cdr lst)))
          (repeat_ele ele (cdr lst)))))
(define (row_check lst)
  (if (null? lst) #t
      (if (and (number? (car lst))
           (> (repeat_ele (car lst) lst) 1)) #f
          (row_check (cdr lst)))))
(define (check_mat mat)
  (if (null? mat) #t
      (if (row_check (car mat)) (check_mat (cdr mat))
          #f)))
(define (verify_input inputvec)
  (and (check_mat inputvec)
       (check_mat (transpose inputvec))
       (check_mat (make_out_ofcolns inputvec))))
  
(define (is_sudoku_sol m)
  (and (whether_row_sudoku m)
       (whether_row_sudoku (transpose m))
       (whether_row_sudoku (make_out_ofcolns m))))
(define (2dvec->list vec )
  (map (lambda(x) (vector->list x)) (vector->list vec)))
(define x (2dvec->list #(#(5 3 4 6 7 8 9 1 2)
   #(6 7 2 1 9 5 3 4 8)
   #(1 9 8 3 4 2 5 6 7)
   #(8 5 9 7 6 1 4 2 3)
   #(4 2 6 8 5 3 #f 9 1)
   #(7 1 3 9 2 4 8 5 6)
   #(9 6 1 5 3 7 2 8 4)
   #(2 8 7 4 1 9 6 3 5)
   #(3 4 5 2 8 6 1 7 9))))
(define (list->2dvec lst)
  (list->vector (foldr (lambda (x y) (cons (list->vector x) y)) '() lst)))
                                      
  