#lang racket/gui
(provide procedure100
         vec
         geneaterandom_sudoku
         new-timer
         myclass%
         iter)
(require "gensudo.scm")
(require "showsol.scm")
(require "sudokusol.scm")
(define vec  (make-2d-vector 9 9 #f))
(define (procedure btn no)
  (define calci (new frame% 
                     [label "Numbers for Sudoku"]
                     [min-width 200]
                     [min-height 150]
                     [stretchable-width #f]
                     [stretchable-height #f]))
  (define msg (new message%
                   [parent calci]
                   [label "Instructions:
1) click the number u want in that box 
 and click  ok
2)If  U want to clear number u clicked 
 use c button
3) Note:no  >9 are not taken as input"]))  
  (define mydisplay (new text-field% 
                         [label #f]
                         [parent calci]
                         [vert-margin 10]
                         [init-value ""]
                         [enabled #f]
                         [min-width 200]
                         [stretchable-width #f]
                         [stretchable-height #f]))
  
  (define rowpanel (new vertical-panel%
                        [parent calci]
                        [alignment '(center top)]
                        [horiz-margin 0]
                        [spacing 20]))
  
  (define row1 (new horizontal-panel% 
                    [parent rowpanel]
                    [alignment '(left top)]))
  
  (define num1 (new button%
                    [label "1"]
                    [parent row1]
                    [min-width 50]
                    [min-height 50]
                    [callback (lambda (button event)
                                (if (equal? (send mydisplay get-value) "0") 
                                    (send mydisplay set-value "1")
                                    (send mydisplay set-value (string-append (send mydisplay get-value) "1"))))]))
  
  (define num2 (new button%
                    [label "2"]
                    [parent row1]
                    [min-width 50]
                    [min-height 50]
                    [callback (lambda (button event)
                                (if (equal? (send mydisplay get-value) "0") 
                                    (send mydisplay set-value "2")
                                    (send mydisplay set-value (string-append (send mydisplay get-value) "2"))))]))
  
  (define num3 (new button%
                    [label "3"]
                    [parent row1]
                    [min-width 50]
                    [min-height 50]
                    [callback (lambda (button event)
                                (if (equal? (send mydisplay get-value) "0") 
                                    (send mydisplay set-value "3")
                                    (send mydisplay set-value (string-append (send mydisplay get-value) "3"))))]))
  
  (define row2 (new horizontal-panel% 
                    [parent rowpanel]
                    [alignment '(left top)]))
  
  (define num4 (new button%
                    [label "4"]
                    [parent row2]
                    [min-width 50]
                    [min-height 50]
                    [callback (lambda (button event)
                                (if (equal? (send mydisplay get-value) "0") 
                                    (send mydisplay set-value "4")
                                    (send mydisplay set-value (string-append (send mydisplay get-value) "4"))))]))
  
  (define num5 (new button%
                    [label "5"]
                    [parent row2]
                    [min-width 50]
                    [min-height 50]
                    [callback (lambda (button event)
                                (if (equal? (send mydisplay get-value) "0") 
                                    (send mydisplay set-value "5")
                                    (send mydisplay set-value (string-append (send mydisplay get-value) "5"))))]))
  
  (define num6 (new button%
                    [label "6"]
                    [parent row2]
                    [min-width 50]
                    [min-height 50]
                    [callback (lambda (button event)
                                (if (equal? (send mydisplay get-value) "0") 
                                    (send mydisplay set-value "6")
                                    (send mydisplay set-value (string-append (send mydisplay get-value) "6"))))]))
  
  (define row3 (new horizontal-panel% 
                    [parent rowpanel]
                    [alignment '(left top)]
                    ))
  
  (define num7 (new button%
                    [label "7"]
                    [parent row3]
                    [min-width 50]
                    [min-height 50]
                    [callback (lambda (button event)
                                (if (equal? (send mydisplay get-value) "0") 
                                    (send mydisplay set-value "7")
                                    (send mydisplay set-value (string-append (send mydisplay get-value) "7"))))]))
  
  (define num8 (new button%
                    [label "8"]
                    [parent row3]
                    [min-width 50]
                    [min-height 50]
                    [callback (lambda (button event)
                                (if (equal? (send mydisplay get-value) "0") 
                                    (send mydisplay set-value "8")
                                    (send mydisplay set-value (string-append (send mydisplay get-value) "8"))))]))
  
  (define num9 (new button%
                    [label "9"]
                    [parent row3]
                    [min-width 50]
                    [min-height 50]
                    [callback (lambda (button event)
                                (if (equal? (send mydisplay get-value) "0") 
                                    (send mydisplay set-value "9")
                                    (send mydisplay set-value (string-append (send mydisplay get-value) "9"))))]))
  
  (define row4 (new horizontal-panel%
                    [parent rowpanel]
                    [alignment '(left top)]))
  (define clear (new button%
                [label "C"]
                [parent row4]
                [min-width 50]
                [min-height 50]
                [callback (lambda (button event)
                            (send mydisplay set-value ""))]))
  
  (define ok (new button%
                  [label "OK"]
                  [parent row4]
                  [min-width 100]
                  [min-height 50]
                  [callback (lambda (button event)
                              (if (app-val (send mydisplay get-value))
                                  (begin (send btn set-label (number->string 
                                                              (app-val (send mydisplay get-value))))
                                         (2d-vector-set! input-vector (vector-ref no 0)
                                                         (vector-ref no 1) (app-val (send mydisplay get-value)))
                                         
                                         (send calci show #f)
                                         )
                                  (send mydisplay set-value "")))]))
  (define (app-val str)
    (cond [(equal? str "1") 1]
          [(equal? str "2") 2]
          [(equal? str "3") 3]
          [(equal? str "4") 4]
          [(equal? str "5") 5]
          [(equal? str "6") 6]
          [(equal? str "7") 7]
          [(equal? str "8") 8]
          [(equal? str "9") 9]
          [else #f]))
  (send calci show #t)
  )
(define-syntax for 
  (syntax-rules (:)
    [(for init : bexp : change : stmts)  (begin init
                                                (define (loop)
                                                  (cond [bexp (begin stmts
                                                                     change
                                                                     (loop))]))
                                                (loop))]))
(define (procedure100)
  (define sudoku-frame
    (new frame%
         [label "sudoku"]
         [min-width 800]
         [min-height 500]
         [stretchable-width #f]
         [stretchable-height #f]))
  (define dinu-msg
    (new message%
         (parent sudoku-frame)
         (label "
1) To give input please click on the icon where u want to give input,there will be a window displayed 
from which input is taken.
2) The numbers which are intially filled cannot overridden")))
         
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
  (define row11 (new horizontal-panel% 
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
  (define mybutton%
    (class button%
      (init-field (cordi (vector 0 0)))
      (super-new)))
  
  (define line'())
  (define change-display '())
  
  (define iter 0)
  (define (procedure1 vec1)
    (define name '())
    (cond [(= iter 0)
           (begin 
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
                                                                              (cond [(2d-vector-ref vec1 i j)
                                                                                     (2d-vector-set! input-vector i j (2d-vector-ref vec1 i j))])
                                                                              (define name
                                                                                (new mybutton%
                                                                                     [label (if (2d-vector-ref vec1 i j)
                                                                                                (number->string (2d-vector-ref vec1 i j))
                                                                                                " ")]
                                                                                     [parent row]
                                                                                     [min-width 50]
                                                                                     [min-height 50]
                                                                                     [callback (if (2d-vector-ref vec1 i j)
                                                                                                   (lambda (button event) (void))
                                                                                                   (lambda (button event)
                                                                                                     (procedure name (get-field cordi name) )))]))
                                                                              (set-field! cordi name (vector i j))))))
             (set! row '())
             (set! iter (+ iter 1)))]))
  (procedure1 vec)
  (define errorframe (new frame%
                          [label "Example"]
                          [width 500]
                          [height 300]))
  (define canvasrar (new canvas% [parent errorframe]
                         [paint-callback
                          (lambda (canvas dc)
                            (send dc set-scale 3 3)
                            (send dc set-text-foreground "blue")
                            (send dc draw-text "GIVEN INCORRECT" 0 0)
                            (send dc draw-text  "sudoku" 0 20)
                            (send dc draw-text "TRY AGAIN!!" 0 40))]))
  (send canvasrar set-canvas-background (make-object color% "pink"))
  
  (define solve 
    (new button%
         [label "SOLVE"]
         [parent row3]
         [min-width 100]
         [min-height 50]
         [callback 
          (lambda (button event)
            (send sudoku-frame show #f)
            (if (equal? vec  (make-2d-vector 9 9 #f)) (if (verify_input (2dvec->list input-vector))
                                                          (procedure2 (solve-sudoku (2dvec->list input-vector)))
                                                          (begin 
                                                            (send errorframe show #t)
                                                            (sleep/yield 3)
                                                            (send errorframe show #f)
                                                            (send  sudoku-frame show #t)))
                (procedure2 (solve-sudoku (2dvec->list vec)))))]))
  (define dinu
    (new message%
         [parent row10]
         [label "status corresponding to sudoku"]))
  (define mydisplay (new text-field% 
                         [label #f]
                         [parent row10]
                         [vert-margin 50]
                         [init-value "0"]
                         [enabled #f]
                         [min-width 200]
                         [stretchable-width #f]
                         [stretchable-height #f]))
  (define congoframe (new frame%
                          [label "Example"]
                          [width 500]
                          [height 200]))
  (define canvasr (new canvas% [parent congoframe]
                       [paint-callback
                        (lambda (canvas dc)
                          (send dc set-scale 3 3)
                          (send dc set-text-foreground "blue")
                          (send dc draw-text "CONGOS!!!!!!" 0 0)
                          (send dc draw-text  "SOLVED " 0 20)
                          (send dc draw-text "sudoku!!" 0 40))]))
  (send canvasr set-canvas-background (make-object color% "pink"))
  
  
  (define verify 
    (new button%
         [label "VERIFY"]
         [parent row6]
         [min-width 100]
         [min-height 50]
         [callback 
          (lambda (button event)
            (if (is_sudoku_sol (2dvec->list input-vector))
                (begin 
                  (send mydisplay set-value "congos !!! u have solved sudoku")
                  (send sudoku-frame show #f)
                  (send congoframe show #t)
                  (sleep/yield 2)
                  (send congoframe show #f)
                  (procedure2 input-vector))
                  (begin (send mydisplay set-value "OoPs !!! TrY aGAiN")
                         (sleep/yield 3)
                         (send mydisplay set-value ""))))]))
(define rakesh-idea
  (new message%
       [parent row10]
       [label "|      tym (in seconds)"]))
(define rake-display
  (new text-field% 
       [label #f]
       [parent row10]
       [vert-margin 50]
       [init-value "0"]
       [enabled #f]
       [min-width 200]
       [stretchable-width #f]
       [stretchable-height #f]))
(define (updatetimer)
  (begin (sleep/yield 1)
         (send rake-display set-value (number->string (+ 1 (string->number (send rake-display get-value)))))
         (if (= iter 1)
             (updatetimer)
             (void))))
(send sudoku-frame show #t)
(updatetimer))