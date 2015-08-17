#lang racket/gui
(provide input-vector
         make-2d-vector
         2d-vector-ref
         2d-vector-set!
         solve-sudoku
         procedure2
         make-copy
         new-timer
         n-timer
         myclass%
         iter)
(define iter 0)
(define myclass%
  (class object%
    (init-field frame)
    (init-field timer)
    (super-new)
    (define (click-event)
      (send frame show #t))
    (define/public (changeiter)
      (set! iter (+ iter 1)))
    (send timer add click-event)))
(define timer%
  (class object%
    (super-new)
    (define (tick-h l)
      (if (null? l) (void)
          (begin 
            ((car l))
            (tick-h (cdr l)))))
    (define prevframe '())
    (define/public (add p-frame)
      (set! prevframe (cons p-frame prevframe)))
    (define/public (show-prevframe)
      (tick-h prevframe))))
(define new-timer (new timer%))
(define n-timer (new timer%))
(require "programwithflaws.scm")
(define input-vector (make-2d-vector 9 9 #f))
(define (procedure2 vec1)
(define sudoku-frame
  (new frame%
       [label "sudoku"]
       [min-width 800]
       [min-height 500]
       [stretchable-width #f]
       [stretchable-height #f]))

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
(define row10 (new horizontal-panel% 
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
(define line'())
(define change-display '())
  (define vec (2dvec->list vec1))
  (begin (send sudoku-frame show #t)
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
                                                                                 [callback (lambda (button event) (void))
                                                                                           ]))))
                  ))
         
         )
(define go_to_mainmenu
  (new button%
       [label "GO to main menu"]
       [parent row10]
       [min-width 250]
       [min-height 50]
       [callback 
        (lambda (button event) 
          (send sudoku-frame show #f)
          (send new-timer show-prevframe)
          (set! iter  0)
                 )]))
(define exit
  (new button%
       [label "EXIT"]
       [parent row10]
       [min-width 250]
       [min-height 50]
       [callback 
        (lambda (button event) 
          (send sudoku-frame show #f)
                 )]))
  (send sudoku-frame show #t))


