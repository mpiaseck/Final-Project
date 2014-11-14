;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname djpower) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
(require 2htdp/universe)
(require 2htdp/image)
(require rsound)
 
(define SR 44100)
(define (s sec) (* SR sec))
(define (both a b) b)
 
(define-struct ws (cs but live W H))
 
(define-struct component (s posn))
 
(define-struct vslider (W H val))
 
(define rh 100)
 
(define VSLIDER-HANDLE-H 40)
(define SLOT-OFFSET (/ VSLIDER-HANDLE-H 2))
 
;; draw the world
;; world -> image
(define (draw-world ws)
  (begin
    (set-box! world-box ws)
    (place-components (ws-cs ws)
            (place-image (text "Deck A" 24 "black") 90 425
            (place-image (text "Tempo" 20 "black") 325 460
            (place-image (text "Crossfader" 20 "black") 510 460
            (place-image (text "Deck B" 24 "black") 790 425
            (place-image (text "Tempo" 20 "black") 695 460
            (place-image (rectangle rh rh "solid" (if (false? (first (ws-but ws))) "red" "green")) 100 550
            (place-image (rectangle rh rh "solid" (if (false? (second (ws-but ws))) "red" "green")) 210 550
            (place-image (rectangle rh rh "solid" (if (false? (third (ws-but ws))) "red" "green")) 320 550
            (place-image (rectangle rh rh "solid" (if (false? (fourth (ws-but ws))) "red" "green")) 680 550
            (place-image (rectangle rh rh "solid" (if (false? (fifth (ws-but ws))) "red" "green")) 790 550
            (place-image (rectangle rh rh "solid" (if (false? (sixth (ws-but ws))) "red" "green")) 900 550
            (place-image (rectangle (* 2 rh) ( * 3 rh) "outline" "black") 150 250                                                                  
            (place-image (rectangle (* 2 rh) ( * 3 rh) "outline" "black") 850 250  
                      (empty-scene (ws-W ws) (ws-H ws))))))))))))))))))
 
 
;; place-components : list-of-components scene -> scene
;; place all of the given components onto the scene
(define (place-components comps scene)
  (cond [(empty? comps) scene]
        [else (place-components
               (rest comps)
               (place-component (first comps)
                                scene))]))
 
;; place-component : component scene -> scene
;; place the image of the component onto the scene
;; (tested indirectly.)
(define (place-component c s)
  (place-image (draw-vslider (component-s c))
               (+ (posn-x (component-posn c)) (/ (component-width c) 2))
               (+ (posn-y (component-posn c)) (/ (component-height c) 2))
               s))
 
 
;; draw a vertical slider
;; slider -> image
(define (draw-vslider vs)
  (local
    [(define w (vslider-W vs))
     (define h (vslider-H vs))
     (define slot-len (- h (* 2 SLOT-OFFSET)))
     (define slider-pixels (- (+ slot-len SLOT-OFFSET)
                              (* slot-len (vslider-val vs))))]
    (place-image
     (overlay
      (text (number->string (* 2 (vslider-val vs)))
            10
            "solid")
      (rectangle w VSLIDER-HANDLE-H "solid" (make-color #xE0 #xD0 #x90)))
     (/ w 2) slider-pixels
     (add-line (rectangle w h "solid" "white")
               (/ w 2) SLOT-OFFSET (/ w 2) (- h SLOT-OFFSET)
               (make-pen "black" 7 "solid" "round" "round")))))
 
;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  HANDLING KEY EVENTS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;
 
(define (onkey ws key)
    (cond [(and (boolean=? (first (ws-but ws)) true) (key=? key "1")) (make-ws (ws-cs ws)
                                                          (list false
                                                                (second (ws-but ws))
                                                                (third (ws-but ws))
                                                                (fourth (ws-but ws))
                                                                (fifth (ws-but ws))
                                                                (sixth (ws-but ws)))
                                                          (ws-live ws)
                                                          (ws-W ws)
                                                          (ws-H ws))]
          [(and (boolean=? (second (ws-but ws)) true) (key=? key "2")) (make-ws (ws-cs ws)
                                                          (list (first (ws-but ws))
                                                                false
                                                                (third (ws-but ws))
                                                                (fourth (ws-but ws))
                                                                (fifth (ws-but ws))
                                                                (sixth (ws-but ws)))
                                                          (ws-live ws)
                                                          (ws-W ws)
                                                          (ws-H ws))]
          [(and (boolean=? (third (ws-but ws)) true) (key=? key "3")) (make-ws (ws-cs ws)
                                                          (list (first (ws-but ws))
                                                                (second (ws-but ws))
                                                                false
                                                                (fourth (ws-but ws))
                                                                (fifth (ws-but ws))
                                                                (sixth (ws-but ws)))
                                                          (ws-live ws)
                                                          (ws-W ws)
                                                          (ws-H ws))]
          [(and (boolean=? (fourth (ws-but ws)) true) (key=? key "4")) (make-ws (ws-cs ws)
                                                          (list (first (ws-but ws))
                                                                (second (ws-but ws))
                                                                (third (ws-but ws))
                                                                false
                                                                (fifth (ws-but ws))
                                                                (sixth (ws-but ws)))
                                                          (ws-live ws)
                                                          (ws-W ws)
                                                          (ws-H ws))]
          [(and (boolean=? (fifth (ws-but ws)) true) (key=? key "5")) (make-ws (ws-cs ws)
                                                          (list (first (ws-but ws))
                                                                (second (ws-but ws))
                                                                (third (ws-but ws))
                                                                (fourth (ws-but ws))
                                                                false
                                                                (sixth (ws-but ws)))
                                                          (ws-live ws)
                                                          (ws-W ws)
                                                          (ws-H ws))]
          [(and (boolean=? (sixth (ws-but ws)) true) (key=? key "6")) (make-ws (ws-cs ws)
                                                          (list (first (ws-but ws))
                                                                (second (ws-but ws))
                                                                (third (ws-but ws))
                                                                (fourth (ws-but ws))
                                                                (fifth (ws-but ws))
                                                                false)
                                                          (ws-live ws)
                                                          (ws-W ws)
                                                          (ws-H ws))]
          [(key=? key "1") (make-ws (ws-cs ws)
                                    (list true
                                         (second (ws-but ws))
                                          (third (ws-but ws))
                                          (fourth (ws-but ws))
                                          (fifth (ws-but ws))
                                          (sixth (ws-but ws)))
                                    (ws-live ws)
                                    (ws-W ws)
                                    (ws-H ws))]
          [(key=? key "2") (make-ws (ws-cs ws)
                                    (list (first (ws-but ws))
                                          true
                                          (third (ws-but ws))
                                          (fourth (ws-but ws))
                                          (fifth (ws-but ws))
                                          (sixth (ws-but ws)))
                                    (ws-live ws)
                                    (ws-W ws)
                                    (ws-H ws))]
          [(key=? key "3") (make-ws (ws-cs ws)
                                    (list (first (ws-but ws))
                                          (second (ws-but ws))
                                          true
                                          (fourth (ws-but ws))
                                          (fifth (ws-but ws))
                                          (sixth (ws-but ws)))
                                    (ws-live ws)
                                    (ws-W ws)
                                    (ws-H ws))]
          [(key=? key "4") (make-ws (ws-cs ws)
                                    (list (first (ws-but ws))
                                          (second (ws-but ws))
                                          (third (ws-but ws))
                                          true
                                          (fifth (ws-but ws))
                                          (sixth (ws-but ws)))
                                    (ws-live ws)
                                    (ws-W ws)
                                    (ws-H ws))]
          [(key=? key "5") (make-ws (ws-cs ws)
                                    (list (first (ws-but ws))
                                          (second (ws-but ws))
                                          (third (ws-but ws))
                                          (fourth (ws-but ws))
                                          true
                                          (sixth (ws-but ws)))
                                    (ws-live ws)
                                    (ws-W ws)
                                    (ws-H ws))]
          [(key=? key "6") (make-ws (ws-cs ws)
                                    (list (first (ws-but ws))
                                          (second (ws-but ws))
                                          (third (ws-but ws))
                                          (fourth (ws-but ws))
                                          (fifth (ws-but ws))
                                          true)
                                    (ws-live ws)
                                    (ws-W ws)
                                    (ws-H ws))]
          [else (make-ws (ws-cs ws)
                         (ws-but ws)
                         (ws-live ws)
                         (ws-W ws)
                         (ws-H ws))]
          )
    )
 
 
                                                               
                                                             
;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  HANDLING MOUSE EVENTS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;
 
;; handle a mouse event;
;; world number number string -> world
(define (meh ws x y event)
  ;; BUTTON-DOWN EVENT:
  (cond [(string=? event "button-down")
         (local
           [(define chosen-component (xy->component (ws-cs ws) x y))]
           ;; missed all components, leave the world state alone:
           (cond [(false? chosen-component) ws]
                 ;; hit one: turn it on:
                 [else
                  (make-ws (components-posn-update
                            (ws-cs ws)
                            chosen-component
                            y)
                           (ws-but ws)
                           chosen-component
                           (ws-W ws)
                           (ws-H ws))]))]
        ;; BUTTON-UP EVENT:
        [(string=? event "button-up")
         ;; turn the live one off:
         (make-ws (ws-cs ws) (ws-but ws) #f (ws-W ws) (ws-H ws))]
        ;; OTHER EVENT:
        [else
         (cond [(false? (ws-live ws)) ws]
               [else
                ;; one of them is already live
                (make-ws (components-posn-update (ws-cs ws) (ws-live ws) y)
                         (ws-but ws)
                         (ws-live ws)
                         (ws-W ws)
                         (ws-H ws))])]))
 
 
;; components-posn-update : list-of-components posn number
;; return a list of components with the chosen component updated
;; to the position indicated by the mouse
(define (components-posn-update comps live y)
  (cond [(empty? comps) (error 'components-posn-update
                               "no matching component")]
        [else (cond
                [(equal? (component-posn (first comps)) live)
                 ;; found it!
                 (cons (component-posn-update (first comps) y)
                       (rest comps))]
                [else ;; keep searching:
                 (cons (first comps)
                       (components-posn-update (rest comps) live y))])]))
 
;; component-posn-update : component number -> component
;; update the given component to reflect the y position of the mouse
;; (tested indirectly.)
(define (component-posn-update c y)
  (make-component
   (slider-posn-update (component-s c) (- y (posn-y (component-posn c))))
   (component-posn c)))
 
;; slider-posn-update: slider number -> slider
;; given a vertical slider and a Y mouse position relative to the
;; slider, update the slider accordingly
(define (slider-posn-update vs ypos)
  (local
    [(define range (- (vslider-H vs)
                      (* 2 SLOT-OFFSET)))
     (define capped (max SLOT-OFFSET
                         (min (- (vslider-H vs) SLOT-OFFSET)
                              ypos)))
     (define new-posn (- 1.0
                         (/ (- capped SLOT-OFFSET) range)))]
    (make-vslider (vslider-W vs) (vslider-H vs) new-posn)))
 
;; component-width : the width of the component
;; [tested indirectly]
(define (component-width c) (vslider-W (component-s c)))
 
;; component-height : the height of the component
;; [tested indirectly]
(define (component-height c) (vslider-H (component-s c)))
 
;; determine which component a mouse position is in
;; xy->component : number number -> maybe-component
(define (xy->component components x y)
  (cond [(empty? components) false]
        [else (local [(define fc (first components))]
                (cond [(in-bounding-box? fc x y)
                       (component-posn fc)]
                      [else (xy->component (rest components) x y)]))]))
 
;; in-bounding-box? : component pixels pixels -> boolean
;; determine whether an x and a y are in the bounding box of a
;; component.
(define (in-bounding-box? component x y)
  (local [(define p (component-posn component))]
  (and (<= (posn-x p) x (sub1 (+ (posn-x p)
                                 (component-width component))))
       (<= (posn-y p) y (sub1 (+ (posn-y p)
                                 (component-height component)))))))
 
(define initial-world
  (make-ws (list
            (make-component (make-vslider 50 400 0.5)
                            (make-posn 300 30))
            (make-component (make-vslider 50 400 0)
                            (make-posn 392 30))
            (make-component (make-vslider 50 400 .5)
                            (make-posn 484 30))
            (make-component (make-vslider 50 400 0)
                            (make-posn 576 30))
            (make-component (make-vslider 50 400 0.5)
                            (make-posn 668 30)))
            (list false
                  false
                  false
                  false
                  false
                  false)
           #f
           1000 1000))
 
(define world-box (box initial-world))
 
(define SONG1 (rs-read "/Users/Matt/Google Drive/Cal Poly/CPE/final-project/arcadia.wav"))
(define SONGLEN1 (rs-frames SONG1))
 
(define SONG2 (rs-read "/Users/Matt/Google Drive/Cal Poly/CPE/final-project/go.wav"))
(define SONGLEN2 (rs-frames SONG2))
 
; song 1 volume
(define fetch-volume1
        (vslider-val
         (component-s
          (third (ws-cs (unbox world-box))))))
 
; song 2 volume
(define fetch-volume2
  (- 1(vslider-val
         (component-s
          (third (ws-cs (unbox world-box)))))))
 
;should song 1 loop?
(define (maybe-wrap1 old incr len)
  (local [(define new (+ old incr))]
    (cond [(<= len new) (- new len)]
          [(< new 0) (+ len new)]
          [else new])))
 
;should song 2 loop?
(define (maybe-wrap2 old incr len)
  (local [(define new (+ old incr))]
    (cond [(<= len new) (- new len)]
          [(< new 0) (+ len new)]
          [else new])))
 
; song 1 speed
(define (fetch-speed1)
  (* 2.0
        (vslider-val
         (component-s
          (first (ws-cs (unbox world-box)))))))
 
; song 2 speed
(define (fetch-speed2)
  (* 2.0
        (vslider-val
         (component-s
          (fifth (ws-cs (unbox world-box)))))))
 
;song 1 delay
(define (fetch-delay1)
  (* 20000.0
     (vslider-val
      (component-s
       (second (ws-cs (unbox world-box)))))))
 
;song 2 delay
(define (fetch-delay2)
  (* 20000.0
     (vslider-val
      (component-s
       (fourth (ws-cs (unbox world-box)))))))
 
;network for song1
(define (flexloop1 len)
  (network (incr)
    [ctr = (maybe-wrap1
            (prev ctr 0.0)
            incr
            len)]))
 
(signal-play
 (network ()
   [ctr <= (flexloop1 SONGLEN1) (fetch-speed1)]
   [delayed-ctr = (maybe-wrap1 ctr (fetch-delay1) SONGLEN1)]
   [out = (*  (vslider-val
         (component-s
          (third (ws-cs (unbox world-box))))) (/ (+ (rs-ith/left SONG1 (floor ctr))
                (rs-ith/left SONG1 (floor delayed-ctr)))
             2.0))]))
 
;network for song2
(define (flexloop2 len)
  (network (incr)
    [ctr = (maybe-wrap2
            (prev ctr 0.0)
            incr
            len)]))
 
(signal-play
 (network ()
   [ctr <= (flexloop2 SONGLEN2) (fetch-speed2)]
   [delayed-ctr = (maybe-wrap2 ctr (fetch-delay2) SONGLEN2)]
   [out = (*  (- 1 (vslider-val
         (component-s
          (third (ws-cs (unbox world-box)))))) (/ (+ (rs-ith/left SONG2 (floor ctr))
                (rs-ith/left SONG2 (floor delayed-ctr)))
             2.0))]))
 
(big-bang initial-world
          [on-mouse meh]
          [to-draw draw-world]
          [on-key onkey])
 
;*******
;
;   TEST CASES
;
;*******
 
 
#;(check-expect
 (draw-vslider
  (make-vslider 50 200 1.0))
 (place-image
  (overlay
   (text "99" 35 "black")
   (rectangle 100 40 "solid" (make-color #xE0 #xD0 #x90)))
  25 20
  (add-line (rectangle 50 200 "solid" "white")
            25 20 25 180
            (make-pen "black" 7 "solid" "round" "round"))))
 
#;(check-expect
 (draw-vslider
  (make-vslider 50 300 0.5))
 (place-image
  (overlay
   (text "50" 35 "black")
  (rectangle 100 40 "solid" (make-color #xE0 #xD0 #x90)))
  25 150
  (add-line (rectangle 50 300 "solid" "white")
            25 20 25 280
            (make-pen "black" 7 "solid" "round" "round"))))
 
#; (check-expect
 (slider-posn-update (make-vslider 50 540 0.2) 145)
 (make-vslider 50 540 3/4))
 
#; (check-expect
 (place-components
  (list (make-component (make-vslider 30 100 1/2) (make-posn 15 10))
        (make-component (make-vslider 40 80 3/4) (make-posn 45 5)))
  (empty-scene 400 500))
 (place-image (draw-vslider (make-vslider 40 80 3/4))
  65 45
  (place-image (draw-vslider (make-vslider 30 100 1/2))
               30 60
               (empty-scene 400 500))))
 
#;(check-expect
 (components-posn-update
  (list (make-component (make-vslider 30 100 1/2) (make-posn 15 10))
        (make-component (make-vslider 40 80 3/4) (make-posn 45 5)))
  (make-posn 45 5)
  0)
 (list (make-component (make-vslider 30 100 1/2) (make-posn 15 10))
        (make-component (make-vslider 40 80 1) (make-posn 45 5))))
 
(define test-component (make-component (make-vslider 30 40 1/2)
                                          (make-posn -50 16)))
;(check-expect (in-bounding-box? test-component -51 16) #f)
;(check-expect (in-bounding-box? test-component -50 15) #f)
;(check-expect (in-bounding-box? test-component -50 16) #t)
;(check-expect (in-bounding-box? test-component -21 16) #t)
;(check-expect (in-bounding-box? test-component -20 16) #f)
;(check-expect (in-bounding-box? test-component -21 55) #t)
;(check-expect (in-bounding-box? test-component -21 56) #f)
 
(define example-components
  (list (make-component (make-vslider 100 150 0.1) (make-posn 30 40))
        (make-component (make-vslider 10 30 0.5) (make-posn 140 20))))
 
;(check-expect (xy->component example-components 0 0) false)
;(check-expect (xy->component example-components 30 40) (make-posn 30 40))
;(check-expect (xy->component example-components 35 45) (make-posn 30 40))
;(check-expect (xy->component example-components 130 45) false)
;(check-expect (xy->component example-components 141 45) (make-posn 140 20))
 
 
 
#;(check-expect
 (draw-world (make-ws (list
                       (make-component (make-vslider 50 150 0.3)
                                       (make-posn 23 49)))
                      #t
                      500 300))
 (place-image (draw-vslider (make-vslider 50 150 0.3))
              (+ 23 25) (+ 49 75)
              (empty-scene 500 300)))
 
 
#;(check-expect
 (meh (make-ws (list (make-component (make-vslider 50 540 0.2)
                                     (make-posn 0 0)))
               (make-posn 0 0)
               50 540)
      100 145 "move")
 (make-ws (list (make-component (make-vslider 50 540 3/4)
                                (make-posn 0 0)))
          (make-posn 0 0)
          50 540))
 
;; ignored move:
#; (check-expect
 (meh (make-ws (list
                (make-component (make-vslider 50 540 1/5)
                                (make-posn 0 0)))
               #f
               50 540)
      100 145 "move")
 (make-ws (list
           (make-component (make-vslider 50 540 1/5) (make-posn 0 0)))
          #f
          50 540))
 
 
 
;; missed click:
#; (check-expect
 (meh (make-ws (list
                (make-component (make-vslider 50 540 1/5)
                                (make-posn 0 0)))
               #f
               50 540)
      100 145 "button-down")
 (make-ws (list
           (make-component (make-vslider 50 540 1/5)
                           (make-posn 0 0)))
          #f
          50 540))
 
;; hit click
#; (check-expect
 (meh (make-ws (list
                (make-component (make-vslider 50 540 1/5)
                                (make-posn 0 0)))
               #f
               50 540)
      40 145 "button-down")
 (make-ws  (list
            (make-component (make-vslider 50 540 3/4)
                            (make-posn 0 0)))
           (make-posn 0 0)
           50 540))
 
 
;; release:
#; (check-expect
 (meh (make-ws (list
                (make-component (make-vslider 50 540 1/5)
                                (make-posn 0 0))) #t
               50 540)
      100 145 "button-up")
 (make-ws (list (make-component (make-vslider 50 540 1/5)
                                (make-posn 0 0))) #f
           50 540))