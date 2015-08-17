#lang racket/gui
(provide procedure
         )
; this is a calculator being made in one night :-/

;variables for calculation

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
  
  ;finally my calculator comes alive
  (send calci show #t)
  )




