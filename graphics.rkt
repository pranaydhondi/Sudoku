#lang racket/gui
;----------------------------------------------libraries-------------------------------------------------------------
(require racket/draw)
(provide frame1)
(require "test.rkt")
(require "programwithflaws.scm")
;--------------------------------------------frame and canvas definations---------------------------------------------------------------
(define frame
  (new frame%
       [label "Sudoku"]
       [min-height 480]
       [min-width 640]
       [stretchable-width #f]
       [stretchable-height #f]))
(define bitmap1 "sudoku/ancient-sudoku_3_big.jpg")
(define (bitmap) (read-bitmap bitmap1))
(define (paint x y)
  (send (send canvas get-dc) draw-bitmap (bitmap) x y))
(define bitmap11 "sudoku/secondpage.jpeg")
(define (bitmaps) (read-bitmap bitmap11))
(define (paint1 x y)
  (send (send canvas1 get-dc) draw-bitmap (bitmaps) x y))
(define frame1
  (new frame%
       [label "Sudoku"]
       [min-height 480]
       [min-width 640]
       [stretchable-width #f]
       [stretchable-height #f]))

(define mycanvas%
  (class canvas%
    (define/override (on-event event)
      (cond((send event get-left-down)
            (let*((x (send event get-x))
                  (y (send event get-y)))
              (cond((and(<=  (abs (- x  328)) 162)
                        (<= (abs (- y 68)) 41))(begin (send frame show #f)
                                                      (send frame1 show #t)))
                   [(and(<=  (abs (- x  322)) 134)
                        (<= (abs (- y 355)) 41)) (send frame show #f)])))))
    
    (super-new)))
(define noclicks 0)
(define mycanvas1%
  (class canvas%
    (super-new)
    (define/override (on-event event)
      (cond((send event get-left-down)
            (let*((x (send event get-x))
                  (y (send event get-y)))
              (begin (set! noclicks (+ noclicks 1))
                     (cond [(> noclicks 1) (set! noclicks 0)]  
                           [(and(<=  (abs (- x  101)) 101)
                                (<= (abs (- y 316)) 37)) (if (= iter 0)
                                                             (begin 
                                                               (send frame1 show #f)
                                                               (make-copy vec (make-2d-vector 9 9 #f))
                                                               (procedure100)
                                                               (send sec-f changeiter)
                                                               ) 
                                                             (void))]
                           [(and(<=  (abs (- x  331)) 128)
                                (<= (abs (- y 213)) 25)) (begin 
                                                           (send frame1 show #f)
                                                           (send frame show #t))]
                           [(and(<=  (abs (- x  331)) 128)
                                (<= (abs (- y 275)) 37)) 
                            (if (= iter 0)
                                (begin (send frame1 show #f)
                                       (make-copy vec (geneaterandom_sudoku))
                                       (procedure100)
                                       (send sec-f changeiter))
                                (void))
                            
                            ]
                           [(and(<=  (abs (- x  331)) 128)
                                (<= (abs (- y 348)) 37)) (begin 
                                                           (send help-window show #t)
                                                           (send frame1 show #f)
                                                           )]
                           [(and(<=  (abs (- x  547)) 92)
                                (<= (abs (- y 312)) 33)) (begin 
                                                           (send frame1 show #f)
                                                           (send about-window show #t)
                                                           )]))))))
    
    ))
(define canvas
  (new mycanvas%
       [parent frame]
       [paint-callback (lambda (canvas dc)
                         (set! bitmap1 "sudoku/ancient-sudoku_3_big.jpg")
                         (paint 0 0))]))
(define canvas1
  (new mycanvas1%
       [parent frame1]
       [paint-callback (lambda (canvas dc)
                         (set! bitmap11 "sudoku/secondpage.jpeg")
                         (paint1 0 0))]))

(send frame show #t)
(define sec-f
  (make-object myclass% frame new-timer))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define about-window (new frame% [label "About Us"]
                          [width 800]
                          [height 500]))
(define myfont (make-object font% 20 'roman 'italic 'normal #t 'partly-smoothed #f 'unaligned))

(define mycolor (make-object color% "RED"))
;(send mycolor ok?)

(define imag-canvas (new canvas% [parent about-window]
                         [paint-callback
                          (lambda (canvas dc) (paint-about dc))]))
(send imag-canvas flush)
(send imag-canvas set-canvas-background mycolor)

(define face-bitmap (make-object bitmap% 800 500))

(define (paint-about dc) (send dc draw-bitmap face-bitmap 0 0))
(define bm-dc (make-object bitmap-dc% face-bitmap))

(send bm-dc clear)
(define background (read-bitmap "sudoku/about.jpg"))
(send bm-dc draw-bitmap background 0 0)
(send bm-dc set-font myfont)
(send bm-dc draw-text "SUDOKU" 350 15)
(send bm-dc draw-text "Venkata Dinesh Kota" 50 100)
(send bm-dc draw-text "120050051" 85 170)
(send bm-dc draw-text "1st Year CSE Department" 50 240)
(send bm-dc draw-text "AND" 375 170)
(send bm-dc draw-text "Pranay Dhondi" 520 100)
(send bm-dc draw-text "120050054" 555 170)
(send bm-dc draw-text "1st Year CSE Department" 470 240)
(send bm-dc draw-text "THANK YOU" 320 380)
(new button%
     [label "Back"]
     [parent about-window]
     [font myfont]
     [min-width 80]
     [min-height 25]
     [callback (lambda (button event)
                 (send about-window show #f)
                 (send frame1 show #t)
                 )])
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define help-window (new frame% [label "HELP"]
                         [width 720]
                         [height 480]))
(define myfont1 (make-object font% 15 'roman 'italic 'bold #t 'partly-smoothed #f 'aligned))

(define mycolor1 (make-object color% "PINK"))
;(send mycolor1 ok?)

(define imag-canvas1 (new canvas% [parent help-window]
                          [paint-callback
                           (lambda (canvas dc) (paint-help dc))]))         
(send imag-canvas1 flush)
(send imag-canvas1 set-canvas-background mycolor1)

(define face-bitmap1 (make-object bitmap% 800 500))

(define (paint-help dc) (send dc draw-bitmap face-bitmap1 0 0))
(define bm-dc1 (make-object bitmap-dc% face-bitmap1))

(send bm-dc1 clear)
(define background1 (read-bitmap "sudoku/help.jpg"))
(send bm-dc1 draw-bitmap background1 0 0)
(send bm-dc1 set-font myfont1)
(send bm-dc1 draw-text "INSTRUCTIONS" 115 15)
(send bm-dc1 draw-text "A) The blank spaces given in puzzle must be filled by digits 1 to 9" 15 50)
(send bm-dc1 draw-text "B) Each row,coloum and grid must contains 9 spaces may be partially " 15 80)
(send bm-dc1 draw-text "filled by numbers" 40 110)
(send bm-dc1 draw-text "C) Each row,coloum and grid must contain numbers 1 to 9 only once" 15 140)
(send bm-dc1 draw-text "D) Thus we get our puzzle solved having set of numbers 1 to 9 in" 15 170)
(send bm-dc1 draw-text "every row,every coloum and every minigrid" 40 200)
(send bm-dc1 draw-text "E) The play button generates a random sudoku with some blank spaces" 15 230)
(send bm-dc1 draw-text "which can be given input,thus we can play by solving it" 40 260)
(send bm-dc1 draw-text "F) The solve button on menu gives us a 9*9 gird where we can give" 15 290)
(send bm-dc1 draw-text "input and solve the sudoku of our own choice" 40 320)
(send bm-dc1 draw-text "G) The about button gives the details of the project" 15 350)

(new button%
     [label "Back"]
     [parent help-window]
     [font myfont1]
     [min-width 80]
     [min-height 25]
     [callback (lambda (button event)
                 (send help-window show #f)
                 (send frame1 show #t))])

