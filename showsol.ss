
#lang racket/gui

(provide procedure2)
(require htdp/draw)
(require "test.rkt")
(define-syntax for 
  (syntax-rules (:)
    [(for init : bexp : change : stmts)  (begin init
                                                (define (loop)
                                                  (cond [bexp (begin stmts
                                                                     change
                                                                     (loop))]))
                                                (loop))]))
(define sudoku-frame
  (new frame%
       [label "sudoku"]
       [min-width 800]
       [min-height 500]
       [stretchable-width #f]
       [stretchable-height #f]))
(send sudoku-frame show #t)
(define rowpanel (new vertical-panel%
                      [parent sudoku-frame]
                      [alignment '(center top)]
                      [horiz-margin 170]
                      [spacing 00]))
(define row1 (new horizontal-panel% 
                  [parent rowpanel]
                  [alignment '(left top)]))
(define row2 (new horizontal-panel% 
                  [parent rowpanel]
                  [alignment '(left top)]))
(define row3 (new horizontal-panel% 
                  [parent rowpanel]
                  [alignment '(left top)]))
(define kk (new message%
                [parent rowpanel]
                [label "    "]))
(define row4 (new horizontal-panel% 
                  [parent rowpanel]
                  [alignment '(left top)]))
(define row5 (new horizontal-panel% 
                  [parent rowpanel]
                  [alignment '(left top)]))
(define row6 (new horizontal-panel% 
                  [parent rowpanel]
                  [alignment '(left top)]))
(define kk1 (new message%
                 [parent rowpanel]
                 [label "  "]))
(define row7 (new horizontal-panel% 
                  [parent rowpanel]
                  [alignment '(left top)]))
(define row8 (new horizontal-panel% 
                  [parent rowpanel]
                  [alignment '(left top)]))
(define row9 (new horizontal-panel% 
                  [parent rowpanel]
                  [alignment '(left top)]))
(define row '())
(define (change rw)
  (cond [(null? rw) row1]
        [(equal? rw row1) row2]
        [(equal? rw row2) row3]
        [(equal? rw row3) row4]
        [(equal? rw row4) row5]
        [(equal? rw row5) row6]
        [(equal? rw row6) row7]
        [(equal? rw row7) row8]
        [(equal? rw row8) row9]))
(define (list->2dvec lst)
  (list->vector (map (lambda(x) (list->vector x))  lst)))
(define (2dvec->list vec )
  (map (lambda(x) (vector->list x)) (vector->list vec)))

(define line'())
(define vec (2dvec->list (solve-sudoku pp)))
(define change-display '())
(define (procedure2)
  (for  (begin (define value_num 0)
             (define i 0)) : (< i 9) : (begin (set! i (+ i 1))
                                              ):
  (begin (set! row (change row))
         
         (for (define j 0) : (< j 9) : (set! j (+ j 1)) : (begin (set! value_num (+ (* i 9) j))
                                                                 (set! name (number->string (+ (* i 9) j)))
                                                                 (cond [(and (= (remainder j 3) 0)
                                                                             (not (= j 0)))
                                                                        (set! line  (new message%
                                                                                         [parent row]
                                                                                         [label "    "]))])
                                                                 (define name
                                                                   (new button%
                                                                        [label (if (2d-vector-ref (list->2dvec vec ) i j)
                                                                                   (number->string (2d-vector-ref (list->2dvec vec ) i j))
                                                                                   " ")]
                                                                        [parent row]
                                                                        [min-width 50]
                                                                        [min-height 50]
                                                                        [callback (if (2d-vector-ref (list->2dvec vec ) i j)
                                                                                      (lambda (button event) (void))
                                                                                      (lambda (button event)
                                                                                        (procedure name value_num)))])))))))
(procedure2)
(define exit_the_window
  (new button%
       [label "exit"]
       [parent row3]
       [min-width 100]
       [min-height 50]
       [callback 
        (lambda (button event) 
          (send sudoku-frame show #f))]))
