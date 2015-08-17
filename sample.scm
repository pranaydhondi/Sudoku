#lang racket/gui


(define (procedure btn)
  ; frame for calculator
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
 and click  ok"]))
  
  (define mydisplay (new text-field% 
                         [label #f]
                         [parent calci]
                         [vert-margin 10]
                         [init-value "0"]
                         [enabled #f]
                         [min-width 200]
                         [stretchable-width #f]
                         [stretchable-height #f]))
  
  (define rowpanel (new vertical-panel%
                        [parent calci]
                        [alignment '(center top)]
                        [horiz-margin 50]
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
  
  (define ok (new button%
                  [label "OK"]
                  [parent row4]
                  [min-width 150]
                  [min-height 50]
                  [callback (lambda (button event)
                              (if (app-val (send mydisplay get-value))
                                  (begin (send btn set-label (number->string 
                                                              (app-val (send mydisplay get-value))))
                                         
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
(define row4 (new horizontal-panel% 
                  [parent rowpanel]
                  [alignment '(left top)]))
(define row5 (new horizontal-panel% 
                  [parent rowpanel]
                  [alignment '(left top)]))
(define row6 (new horizontal-panel% 
                  [parent rowpanel]
                  [alignment '(left top)]))
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


(define change-display '())
(for (define i 0) : (< i 9) : (set! i (+ i 1)) :
  (begin (set! row (change row))
         (for (define j 0) : (< j 9) : (set! j (+ j 1)) : (begin (set! name (number->string (+ (* i 9) j)))
                                                                 (define name
                                                                   (new button%
                                                                        [label ""]
                                                                        [parent row]
                                                                        [min-width 50]
                                                                        [min-height 50]
                                                                        [callback (lambda (button event)
                                                                                        
                                                                                          (procedure name))]))))))
(define canvas1
  (new canvas% [parent sudoku-frame]
       [paint-callback
        (lambda (canvas dc) (paint1 dc))]))
(define (paint1 dc) (send dc draw-bitmap face-bitmap1 0 0))
(define face-bitmap1 (make-object bitmap% 700 500))
(define dc1 (make-object bitmap-dc% face-bitmap1))
(send dc1 set-pen "RED" 10 'solid)
(send dc1 draw-line 10 160 10 200)



