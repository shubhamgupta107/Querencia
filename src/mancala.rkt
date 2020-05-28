#lang racket/gui
;(require memoize)
(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)
(require "mancala-images.rkt")
(provide mancala-frame
         mancala-welcome-frame
         how-to-play-frame
         mancala-settings-frame
         game-frame
         mancala-game-over-frame
         mancala-frame-update
         mancala_game_frame_update
         online-mancala_game_frame_update
         howtoplay_frame_update
         mancala_settings_frame_update
         mancala-game-over-update
         mancala-online-game-over-update
         mancala-main_frame
         mancala_welcome_frame_update
         ;online-mancala-main_frame
;         mancala-state
;         set-mancala-state
;         TICKS-1
;         set-TICKS-1
;         TICKS-2
;         set-ticks2
         MOVING?
         set-moving?
         MOVING-CHANCE
;         l2
;         set-l2
;         update-mancala-state
;         bestMove
;         difficulty
;         termination-condition)
         set-first-move
         FIRST-MOVE?
         SEND-MOVE?
         set-send-move
         clicked-cup
         set-clicked-cup
         set-playing-online
         MAKE-MANCALA-MOVE
         DISCONNECTING-FROM-GAME?
         set-disconnect
         RESTARTING?
         set-restart
         initialize
         mancala
         set-mancala
         MADE-MOVE?
         set-made-move
         GAME-OVER?
         ONLINE-GAME-OVER?
         set-online-game-over
         set-game-over)

(define initial-state-1 (list '(4 4 4 4 4 4 0 4 4 4 4 4 4 0) 7 #t #t))
(define initial-state-2 (list '(4 4 4 4 4 4 0 4 4 4 4 4 4 0) 14 #f #t))
(define mancala-state #f)
(define (set-mancala-state x)
  (set! mancala-state x))
(define possible-move-position (list 1 2 3 4 5 6))
(define DOWN-STATUS "NO")
(define TICKS-1 0)
(define (set-TICKS-1 x)
  (set! TICKS-1 x))
(define TICKS-2 0)
(define (set-ticks2 x)
  (set! TICKS-2 x))
(define MOVING? #f)
(define (set-moving? x)
  (set! MOVING? x))
(define FIRST-MOVE? #t)
(define (set-first-move x)
  (set! FIRST-MOVE? x))
(define termination-condition #f)
(define evaluation-function #f)
(define update-mancala-state #f)
(define bestMove #f)
(define difficulty 5)
(define mancala "Mancala-2")
(define (set-mancala x)
  (set! mancala x))
(define SEND-MOVE? #f)
(define (set-send-move x)
  (set! SEND-MOVE? x))
(define clicked-cup #f)
(define (set-clicked-cup x)
  (set! clicked-cup x))
(define PLAYING-ONLINE? #f)
(define (set-playing-online x)
  (set! PLAYING-ONLINE? #t))
(define DISCONNECTING-FROM-GAME? #f)
(define (set-disconnect x)
  (set! DISCONNECTING-FROM-GAME? x))
(define RESTARTING? #f)
(define (set-restart x)
  (set! RESTARTING? x))
(define MADE-MOVE? #f)
(define (set-made-move x)
  (set! MADE-MOVE? x))
(define GAME-OVER? #f)
(define (set-game-over x)
  (set! GAME-OVER? x))
(define ONLINE-GAME-OVER? #f)
(define (set-online-game-over x)
  (set! ONLINE-GAME-OVER? x))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;BUTTON_AREAS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (button-area x0 y0)
    (lambda (x-button y-button)
      (λ (x y)
      (and (< (abs (- x x-button)) x0) (< (abs (- y y-button)) y0)))))

(define (welcome-button-area-1 x-button y-button) ((button-area 150 60) x-button y-button))
(define (welcome-button-area-2 x-button y-button) ((button-area 200 60) x-button y-button))
(define (welcome-button-area-3 x-button y-button) ((button-area 135 60) x-button y-button))
(define instruction-back-button-area ((button-area 75 37.5) 1270 650))

(define (game_chance_buttons x y)
  (let([pos (index-of (map (lambda (t) (((button-area 31 66) (+ 144 (posn-x t)) (+ 82 (posn-y t))) x y))
                    center-position-list) #t)])
  (if (equal? pos #f) -1 (+ 1 pos))))

;;;;;;;;;;;;;;;;;;;;;;;COMMON-BUTTON-ABSTRACTION;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (update str inp s1 s2 f f_hover f_press action)
  (define (hover) (f f_hover))
  (define (press) (f f_press))
  (cond [(mouse=? inp "button-down") (begin (set! DOWN-STATUS str)
                                            ;(when sound? (play button_click))
                                            (press)
                                            s1)]
        [(and (not (equal? DOWN-STATUS str)) (mouse=? inp "button-up"))
         (set! DOWN-STATUS "NO") (reset-all) (hover) s1]
        [(mouse=? inp "button-up") (begin (set! DOWN-STATUS "NO")
                                          (hover)
                                          (action)
                                          s2)]
        [(equal? DOWN-STATUS str) (begin (press)
                                         s1)]
        [else (hover) s1]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;RESET-FUNCTION;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (reset-all)
  (1-player-image-set! 1-player-rest)
  (2-player-image-set! 2-player-rest)
  (settings-image-set! settings-rest)
  (how-to-play-image-set! how-to-play-rest)
  (back-image-set! back-rest)
  (instruction-back-image-set! instruction-back-rest)
  (settings-back-image-set! instruction-back-rest)
  (variant1-image-set! variant1-rest)
  (variant2-image-set! variant2-rest)
  (mancala-back-image-set! mancala-back-rest)
  (restart-image-set! restart-rest)
  (quit-image-set! quit-rest)
  (over-restart-set! restart-rest)
  (over-quit-set! quit-rest)
  (online-restart-image-set! restart-rest)
  (online-quit-image-set! quit-rest)
  (online-over-restart-set! restart-rest)
  (online-over-quit-set! quit-rest))

(define (initialize-first-move)
  (cond [(equal? checkbox4 checkbox-down) (set! FIRST-MOVE? #f)]
        [(equal? checkbox5 checkbox-down) (set! FIRST-MOVE? #t)]))

(define (initialize)
  (cond [FIRST-MOVE? (set! mancala-state initial-state-2) (set! l2 (list (list '(4 4 4 4 4 4 0 4 4 4 4 4 4 0) 14 #f)))]
        [else (set! mancala-state initial-state-1) (set! l2 (list (list '(4 4 4 4 4 4 0 4 4 4 4 4 4 0) 7 #t)))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;ABSTRACTED-BUTTONS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (1-player-image-set! image)
  (set! 1-player-image image))
(define (2-player-image-set! image)
  (set! 2-player-image image))
(define (how-to-play-image-set! image)
  (set! how-to-play-image image))
(define (settings-image-set! image)
  (set! settings-image image))
(define (back-image-set! image)
  (set! back-image image))
(define (instruction-back-image-set! image)
  (set! instruction-back-image image))
(define (settings-back-image-set! image)
  (set! settings-back-image image))
(define (variant1-image-set! image)
  (set! variant1 image))
(define (variant2-image-set! image)
  (set! variant2 image))
(define (mancala-back-image-set! image)
  (set! mancala-back-image image))
(define (restart-image-set! image)
  (set! restart-image image))
(define (quit-image-set! image)
  (set! quit-image image))
(define (over-restart-set! image)
  (set! over-restart-image image))
(define (over-quit-set! image)
  (set! over-quit-image image))
(define (online-restart-image-set! image)
  (set! online-restart-image image))
(define (online-quit-image-set! image)
  (set! online-quit-image image))
(define (online-over-restart-set! image)
  (set! online-over-restart-image image))
(define (online-over-quit-set! image)
  (set! online-over-quit-image image))

(define (1-player-update inp)
  (update "1-player" inp "MANCALA-WELCOME" "GAME-1-PLAYER" 1-player-image-set! 1-player-hover 1-player-down (λ () (initialize-first-move)
                                                                                                              (initialize))))
(define (2-player-update inp)
  (update "2-player" inp "MANCALA-WELCOME" "ASK-MANCALA-PARTNER" 2-player-image-set! 2-player-hover 2-player-down initialize))
(define (how-to-play-update inp)
  (update "how-to-play" inp "MANCALA-WELCOME" "HOW-TO-PLAY" how-to-play-image-set! how-to-play-hover how-to-play-down initialize))
(define (settings-update inp)
  (update "settings" inp "MANCALA-WELCOME" "SETTINGS" settings-image-set! settings-hover settings-down initialize))
(define (back-update inp)
  (update "back" inp "MANCALA-WELCOME" "MANCALA" back-image-set! back-hover back-down initialize))
(define (instruction-back-button-update inp)
  (update "back" inp "HOW-TO-PLAY" "MANCALA-WELCOME" instruction-back-image-set! instruction-back-hover instruction-back-down initialize))
(define (settings-back-button-update inp)
  (update "back" inp "SETTINGS" "MANCALA-WELCOME" settings-back-image-set! instruction-back-hover instruction-back-down initialize))
(define (variant1-update inp)
  (update "variant1" inp "MANCALA" "MANCALA-WELCOME" variant1-image-set! variant1-hover variant1-down (λ () (set! mancala "Mancala-1")
                                                                                            (set-functions!))))
(define (variant2-update inp)
  (update "variant2" inp "MANCALA" "MANCALA-WELCOME" variant2-image-set! variant2-hover variant2-down (λ () (set! mancala "Mancala-2")
                                                                                            (set-functions!))))
(define (mancala-back-update inp)
  (update "back" inp "MANCALA" "GO-TO-HOME" mancala-back-image-set! mancala-back-hover mancala-back-down initialize))
(define (quit-update inp)
  (update "quit" inp "GAME-1-PLAYER" "MANCALA-WELCOME" quit-image-set! quit-hover quit-down initialize))
(define (online-quit-update inp)
  (update "quit" inp "GAME-2-PLAYER" "MANCALA-WELCOME" online-quit-image-set! quit-hover quit-down (if PLAYING-ONLINE? (λ () (set! DISCONNECTING-FROM-GAME? #t))
                                                                                                initialize)))
(define (restart-update inp)
  (update "restart" inp "GAME-1-PLAYER" "GAME-1-PLAYER" restart-image-set! restart-hover restart-down initialize))
(define (online-restart-update inp)
  (update "online-restart" inp "GAME-2-PLAYER" "GAME-2-PLAYER" online-restart-image-set! restart-hover restart-down (λ () (set! RESTARTING? #t)
                                                                                                        (initialize))))
(define (over-restart-update inp)
  (update "over-restart" inp "GAME-OVER" "GAME-1-PLAYER" over-restart-set! restart-hover restart-down (λ () (set! RESTARTING? #t)
                                                                                                        (initialize))))
(define (online-over-restart-update inp)
  (update "over-restart" inp "ONLINE-GAME-OVER" "GAME-2-PLAYER" online-over-restart-set! restart-hover restart-down initialize))
(define (over-quit-update inp)
  (update "over-quit" inp "GAME-OVER" "MANCALA" over-quit-set! quit-hover quit-down (if PLAYING-ONLINE? (λ () (set! DISCONNECTING-FROM-GAME? #t))
                                                                                                initialize)))
(define (online-over-quit-update inp)
  (update "online-over-quit" inp "ONLINE-GAME-OVER" "MANCALA" online-over-quit-set! quit-hover quit-down (if PLAYING-ONLINE? (λ () (set! DISCONNECTING-FROM-GAME? #t))
                                                                                                initialize)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;ALL-IMAGES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define 1-player-image 1-player-rest)
(define 2-player-image 2-player-rest)
(define how-to-play-image how-to-play-rest)
(define settings-image settings-rest)
(define back-image back-rest)
(define instruction-back-image instruction-back-rest)
(define settings-back-image instruction-back-rest)
(define checkbox1 checkbox)
(define checkbox2 checkbox-down)
(define checkbox3 checkbox)
(define checkbox4 checkbox)
(define checkbox5 checkbox-down)
(define variant1 variant1-rest)
(define variant2 variant2-rest)
(define mancala-back-image mancala-back-rest)
(define restart-image restart-rest)
(define quit-image quit-rest)
(define over-restart-image restart-rest)
(define over-quit-image quit-rest)
(define online-restart-image restart-rest)
(define online-quit-image quit-rest)
(define online-over-restart-image restart-rest)
(define online-over-quit-image quit-rest)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;FRAMES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (mancala-frame)
  (place-images (list variant1
                      variant2
                      mancala-back-image)
                (list (make-posn 300 250)
                      (make-posn 1000 250)
                      (make-posn 660 600))
                main-frame-background))
(define (mancala-welcome-frame)
  (place-images (list 1-player-image
                      2-player-image
                      settings-image
                      how-to-play-image
                      back-image)
                (list ;(make-posn 495 160)
                      (make-posn 298 240) (make-posn 992 240)
                      (make-posn 298 440) (make-posn 1002 440)
                      (make-posn 655 600))
                main-frame-background))
(define (how-to-play-frame)
  (place-images (list instruction-back-image)
                (list (make-posn 1270 650))
                how-to-play))
(define (mancala-settings-frame)
  (underlay/align "center" "center"
   main-frame-background               
   (place-images (list checkbox1
                      checkbox2
                      checkbox3
                      checkbox4
                      checkbox5
                      settings-back-image)
                (list (make-posn 305 210)
                      (make-posn 500 210)
                      (make-posn 675 210)
                      (make-posn 390 330)
                      (make-posn 610 330)
                      (make-posn 830 430)) (frame settings-frame-background))))


(define (game-frame)
  ;(displayln l2)
  ;(displayln state)
  (place-images (list restart-image
                     quit-image
                     (text (string-append "Your Score: " (~a (list-ref (caar l2) 13))) 32 "White")
                     (text (string-append "Opponent Score: " (~a (list-ref (caar l2) 6))) 32 "White")
                     (text (cond
                             [(third (first l2)) "Opponent's Turn"]
                             [else "Your Turn"]) 32 "White")
                     ) 
               (list (make-posn 150 620)
                     (make-posn 1250 620)
                     (make-posn 1190 170)
                     (make-posn 1190 230)
                     (make-posn 1190 330)
                     )
   (underlay/xy mancala-game-background 144 82 (place-images (number-list->image-list (car (first l2))) center-position-list board))
   ;400 250 (empty-scene 1351 690))
  ))
(define whose-turn #f)
(define (online-game-frame)
  ;(displayln l2)
  ;(displayln state)
  (place-images (list online-restart-image
                     online-quit-image
                     (text (string-append "Your Score: " (~a (list-ref (caar l2) 13))) 32 "White")
                     (text (string-append "Opponent Score: " (~a (list-ref (caar l2) 6))) 32 "White")
                     (text (cond
                             [(third (first l2)) "Opponent's Turn"]
                             [else "Your Turn"]) 32 "White")
                     ) 
               (list (make-posn 150 620)
                     (make-posn 1250 620)
                     (make-posn 1190 170)
                     (make-posn 1190 230)
                     (make-posn 1190 330)
                     )
   (underlay/xy mancala-game-background 144 82 (place-images (number-list->image-list (car (first l2))) center-position-list board))
   ;400 250 (empty-scene 1351 690))
  ))

(define (mancala-game-over-frame)
  (place-images (list over-restart-image
                      over-quit-image
                      (text/font (cond [(< (evaluation-function (caar l2)) 0) "You Won"]
                                       [(> (evaluation-function (caar l2)) 0) "You Lost"]
                                       [else "It's a Draw"]) 48 "White" "Times New Roman" "roman" "normal" "bold" #f))
                (list (make-posn 1241 645)
                      (make-posn 85 645)
                      (make-posn 700 330))
                game-over-background))
(define (mancala-online-game-over-frame)
  (place-images (list online-over-restart-image
                      online-over-quit-image
                      (text/font (cond [(< (evaluation-function (caar l2)) 0) "You Won"]
                                       [(> (evaluation-function (caar l2)) 0) "You Lost"]
                                       [else "It's a Draw"]) 48 "White" "Times New Roman" "roman" "normal" "bold" #f))
                (list (make-posn 1241 645)
                      (make-posn 85 645)
                      (make-posn 700 330))
                game-over-background))

(define how-to-play how-to-play-2)
(define (set-functions!)
  (cond
    [(equal? mancala "Mancala-1") (set! termination-condition termination-condition1)
                                  (set! evaluation-function evaluation-function1)
                              (set! bestMove bestMove1)
                              (set! update-mancala-state update-mancala-state1)
                              (set! how-to-play how-to-play-1)]
    [else (set! termination-condition termination-condition2)
          (set! evaluation-function evaluation-function2)
          (set! bestMove bestMove2)
          (set! update-mancala-state update-mancala-state2)
          (set! how-to-play how-to-play-2)]))
    

(define (MOVING-CHANCE stat)
  ;(displayln l1)
  ;(displayln MOVING?)
  (cond [(equal? stat "GAME-1-PLAYER")
         (cond
           [(and (not (third mancala-state)) (not MOVING?)) stat]
           [(and (not MOVING?) (third mancala-state)) 
                                              (cond
                                                [(< TICKS-2 30) (set! TICKS-2 (add1 TICKS-2)) stat]
                                                [else (set! TICKS-2 0) 
                                                 (set! MOVING? #t) ;(displayln state)
                                                 (cond
                                                   [(termination-condition (first mancala-state) (third mancala-state)) (set! GAME-OVER? #t)
                                                                                                                        "GAME-OVER"]
                                                   [(fourth mancala-state) (set! mancala-state (update-mancala-state (list (first mancala-state)
                                                                                                                           (bestMove mancala-state difficulty) #t #t)))  stat]
                                                   [else  (set! mancala-state (update-mancala-state mancala-state))  stat])])]
           [(null? (cdr l2)) (set! MOVING? #f) stat]
           [MOVING? (cond
                      [(> TICKS-1 10) (set! TICKS-1 0) (set! l2 (cdr l2))]
                      [else (set! TICKS-1 (add1 TICKS-1))]) stat]
           [else stat])]
        [else stat]))

(define (MAKE-MANCALA-MOVE stat)
  (cond [(equal? stat "GAME-2-PLAYER")
         (cond
           [(and (not (third mancala-state)) (not MOVING?)) ;(displayln "Here1")
                                                            stat]
           [(and (third mancala-state) (not MADE-MOVE?) (not MOVING?)) stat]
           [(and (not MOVING?) (third mancala-state) MADE-MOVE?) 
                                              (set! MOVING? #t) ;(displayln state)
                                                 (begin0
                                                   (cond
                                                     [(termination-condition (first mancala-state) (third mancala-state)) (set! ONLINE-GAME-OVER? #t)
                                                                                                                          "ONLINE-GAME-OVER"]
                                                     [(fourth mancala-state) ;(displayln "Here1")
                                                                             (set! mancala-state (update-mancala-state (list (first mancala-state) clicked-cup #t #t)))
                                                                             stat]
                                                     [else (set! mancala-state (update-mancala-state mancala-state)) stat])
                                                   (set! MADE-MOVE? #f))]
           [(null? (cdr l2)) (set! MOVING? #f) stat]
           [MOVING? (cond
                      [(> TICKS-1 10) (set! TICKS-1 0) (set! l2 (cdr l2))]
                      [else (set! TICKS-1 (add1 TICKS-1))]) stat]
           [else stat])]
        [else stat]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (number-list->image-list l)
  (define len (length l))
  (build-list len (lambda (x) (cond
                                 
                                [(or (equal? x (- (/ len 2) 1)) (equal? x (- len 1)))
                                 (if (= x (- (second (first l2)) 1))
                                     (mancala_cups_big (list-ref l x))
                                     (mancala_cups (list-ref l x)))]
                                [else
                                 (if (= x (- (second (first l2)) 1))
                                     (normal_cups_big (list-ref l x))
                                     (normal_cups (list-ref l x)))]))))

(define center-position-list
  (append (build-list 6 (lambda (x) (make-posn (- 564 (* 86 x)) 136)))
          (list (make-posn 50 225))
          (build-list 6 (lambda (x) (make-posn (+ 134 (* 86 x)) 316)))
          (list (make-posn 653 225))))

(define (mancala-frame-update stat x y inp)
  (cond
    [(((button-area 119 41) 300 250) x y) (variant1-update inp)]
    [(((button-area 119 41) 1000 250) x y) (variant2-update inp)]
    [(((button-area 60 35) 660 600) x y) (mancala-back-update inp)]
    [(and (not (equal? DOWN-STATUS "NO"))
          (mouse=? inp "button-up")) (begin (reset-all)
                                            (set! DOWN-STATUS "NO") "MANCALA")]
    [(mouse=? inp "move") (begin (reset-all)
                                 "MANCALA")]
    [else "MANCALA"]))

(define (mancala_game_frame_update stat x y inp)
  ;(displayln MOVING?)
  (cond
    [(((button-area 100 35) 150 620) x y) (restart-update inp)]
    [(((button-area 75 35) 1250 620) x y) (quit-update inp)]
    ;[(equal? (third state) #t) (set! MOVING? #t) (set! state (update-state (list (first state) (bestMove state 5) #t #t))) stat]
    [MOVING? stat]
    [(termination-condition (first mancala-state) (third mancala-state)) "GAME-OVER"]
    [(equal? inp "button-down") (set! DOWN-STATUS #t) stat]
    [(and DOWN-STATUS (equal? inp "button-up")) (set! DOWN-STATUS #f) (set! MOVING? #t)
                                                    (cond 
                                                          [(equal? (second mancala-state) 14) (set! possible-move-position '(8 9 10 11 12 13))]
                                                          [else (set! possible-move-position (list (second mancala-state)))])
                                                    (cond [(member (game_chance_buttons x y) possible-move-position) ;(displayln state)
                                                           (set! mancala-state (update-mancala-state (list (first mancala-state)
                                                                                                           (game_chance_buttons x y)
                                                                                                           (third mancala-state) (fourth mancala-state))))])
                                                    stat]
    [(and (not (equal? DOWN-STATUS "NO"))
          (mouse=? inp "button-up")) (begin (reset-all)
                                            (set! DOWN-STATUS "NO") "GAME-1-PLAYER")]
    [(mouse=? inp "move") (begin (reset-all)
                                 "GAME-1-PLAYER")]
    [else stat]))

(define (online-mancala_game_frame_update stat x y inp)
  ;(displayln "Here2")
  (cond
    [(((button-area 100 35) 150 620) x y) (online-restart-update inp)]
    [(((button-area 75 35) 1250 620) x y) (online-quit-update inp)]
    ;[(equal? (third state) #t) (set! MOVING? #t) (set! state (update-state (list (first state) (bestMove state 5) #t #t))) stat]
    [MOVING? stat]
    [(termination-condition (first mancala-state) (third mancala-state)) "ONLINE-GAME-OVER"]
    [(equal? inp "button-down") (set! DOWN-STATUS #t) stat]
    [(and DOWN-STATUS (equal? inp "button-up") (not (third mancala-state))) (set! DOWN-STATUS #f) (set! MOVING? #t)
                                                    (cond 
                                                          [(equal? (second mancala-state) 14) (set! possible-move-position '(8 9 10 11 12 13))]
                                                          [else (set! possible-move-position (list (second mancala-state)))])
                                                    (cond [(member (game_chance_buttons x y) possible-move-position) ;(displayln state)
                                                           (set! clicked-cup (game_chance_buttons x y))
                                                           (set! SEND-MOVE? #t)
                                                           ;(displayln mancala-state)
                                                           (set! mancala-state (update-mancala-state (list (first mancala-state)
                                                                                                           (game_chance_buttons x y)
                                                                                                           (third mancala-state) (fourth mancala-state))))])
                                                    stat]
    [(and (not (equal? DOWN-STATUS "NO"))
          (mouse=? inp "button-up")) (begin (reset-all)
                                            (set! DOWN-STATUS "NO") "GAME-2-PLAYER")]
    [(mouse=? inp "move") (begin (reset-all)
                                 "GAME-2-PLAYER")]
    [else stat]))

(define (howtoplay_frame_update x y inp)
  (cond
    [(instruction-back-button-area x y) (instruction-back-button-update inp) ]
    [(and (not (equal? DOWN-STATUS "NO"))
          (mouse=? inp "button-up")) (begin (reset-all)
                                            (set! DOWN-STATUS "NO") "HOW-TO-PLAY")]
    [(mouse=? inp "move") (begin (reset-all)
                                 "HOW-TO-PLAY")]
    [else "HOW-TO-PLAY"]))
(define (reset-difficulty-settings)
  (set! checkbox1 checkbox)
  (set! checkbox2 checkbox)
  (set! checkbox3 checkbox))
(define (reset-firstmove-settings)
  (set! checkbox4 checkbox)
  (set! checkbox5 checkbox))
(define (mancala_settings_frame_update x y inp)
  (cond
    [(and (((button-area 20 20) 505 310) x y) (mouse=? inp "button-down"))(reset-difficulty-settings) (set! checkbox1 checkbox-down) (set! difficulty 1) "SETTINGS"]
    [(and (((button-area 20 20) 700 310) x y) (mouse=? inp "button-down"))(reset-difficulty-settings) (set! checkbox2 checkbox-down) (set! difficulty 5) "SETTINGS"]
    [(and (((button-area 20 20) 875 310) x y) (mouse=? inp "button-down"))(reset-difficulty-settings) (set! checkbox3 checkbox-down) (set! difficulty 10) "SETTINGS"]
    [(and (((button-area 20 20) 590 430) x y) (mouse=? inp "button-down")) (reset-firstmove-settings) (set! checkbox4 checkbox-down) (set! FIRST-MOVE? #f) "SETTINGS"]
    [(and (((button-area 20 20) 810 430) x y) (mouse=? inp "button-down")) (reset-firstmove-settings) (set! checkbox5 checkbox-down) (set! FIRST-MOVE? #t) "SETTINGS"]
    [(((button-area 75 37.5) 1030 530) x y) (settings-back-button-update inp)]
    [(and (not (equal? DOWN-STATUS "NO"))
          (mouse=? inp "button-up")) (begin (reset-all)
                                            (set! DOWN-STATUS "NO") "SETTINGS")]
    [(mouse=? inp "move") (begin (reset-all)
                                 "SETTINGS")]
    [else "SETTINGS"]))

(define (mancala-game-over-update x y inp)
  (cond [(((button-area 100 35) 1241 645) x y) (over-restart-update inp)]
        [(((button-area 75 35) 85 645) x y) (over-quit-update inp)]
        [(and (not (equal? DOWN-STATUS "NO"))
              (mouse=? inp "button-up")) (begin (reset-all)
                                                (set! DOWN-STATUS "NO") "GAME-OVER")]
        [(mouse=? inp "move") (begin (reset-all)
                                     "GAME-OVER")]
        [else "GAME-OVER"]))

(define (mancala-online-game-over-update x y inp)
  (cond [(((button-area 100 35) 1241 645) x y) (online-over-restart-update inp)]
        [(((button-area 75 35) 85 645) x y) (online-over-quit-update inp)]
        [(and (not (equal? DOWN-STATUS "NO"))
              (mouse=? inp "button-up")) (begin (reset-all)
                                                (set! DOWN-STATUS "NO") "ONLINE-GAME-OVER")]
        [(mouse=? inp "move") (begin (reset-all)
                                     "ONLINE-GAME-OVER")]
        [else "ONLINE-GAME-OVER"]))

(define (mancala-main_frame stat)
  (begin (cond
           [(equal? stat "MANCALA") (mancala-frame)]
           [(equal? stat "GO-TO-HOME") (mancala-frame)]
           [(equal? stat "MANCALA-WELCOME") (mancala-welcome-frame)]
           [(equal? stat "GAME-1-PLAYER") (game-frame)]
           [(equal? stat "ASK-MANCALA-PARTNER") (mancala-welcome-frame)]
           [(equal? stat "GAME-2-PLAYER") (online-game-frame)]
           [(equal? stat "GAME-OVER") (mancala-game-over-frame)]
           [(equal? stat "ONLINE-GAME-OVER") (mancala-online-game-over-frame)]
           [(equal? stat "SETTINGS") (mancala-settings-frame)]
           [(equal? stat "HOW-TO-PLAY") (how-to-play-frame)]
           [(equal? stat "BACK") (mancala-settings-frame)]
           [else (mancala-settings-frame)])))

(define (mancala_welcome_frame_update x y inp)
  (cond [((welcome-button-area-1 298 240) x y) (1-player-update inp)]
        [((welcome-button-area-1 992 220) x y) (2-player-update inp)]
        [((welcome-button-area-1 298 440) x y) (settings-update inp)]
        [((welcome-button-area-2 1002 440) x y) (how-to-play-update inp)]
        [((welcome-button-area-3 655 600) x y) (back-update inp)]
        [(and (not (equal? DOWN-STATUS "NO"))
                    (mouse=? inp "button-up")) (begin (reset-all)
                                                      (set! DOWN-STATUS "NO") "MANCALA-WELCOME")]
        [(mouse=? inp "move") (begin (reset-all)
                                                 "MANCALA-WELCOME")]
        [else "MANCALA-WELCOME"]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;WORLD;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (mancala-world-make)
  (big-bang "MANCALA"
            (name "MANCALA")
            (to-draw mancala-main_frame)
    (on-tick MOVING-CHANCE)
;            (stop-when (λ (stat) (begin0 (equal? stat "2-lan")
;                                         (cond ((equal? stat "2-lan")
;                                                (begin (num_players-set! '2-lan)
;                                                       (showframe)))))))
    (on-mouse (λ (stat x y inp)
                (cond [(or (mouse=? inp "enter")
                                   (mouse=? inp "leave")) stat]
                      [(equal? stat "MANCALA") (mancala-frame-update stat x y inp)]
                      [(equal? stat "MANCALA-WELCOME") (mancala_welcome_frame_update x y inp)]
                      [(equal? stat "GAME-1-PLAYER") (mancala_game_frame_update stat x y inp)];)))))
                      [(equal? stat "GAME-OVER") (mancala-game-over-update x y inp)]
                      [(equal? stat "SETTINGS") (mancala_settings_frame_update x y inp)]
                      [(equal? stat "HOW-TO-PLAY") (howtoplay_frame_update x y inp)]
                              
                      [else  (error "Wrong stat passed")])))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;ALGORITHM;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax for
  (syntax-rules (:)
    [(for init : condition : step : statements)
     (begin
       init
       (define (iter)
         (cond [condition (begin statements step (iter))]))
       (iter))]))

(define (termination-condition1 l isMax)
      (or (andmap (lambda (x) (equal? x 0)) (take l (- (/ (length l) 2) 1)))
          (andmap (lambda (x) (equal? x 0)) (reverse (cdr (reverse (drop l (/ (length l) 2))))))))

(define (evaluation-function1 l)
  (define len (length l))
  (- (list-ref l (- (/ len 2) 1)) (list-ref l (- len 1))))

(define (termination-condition2 l isMax)
  (cond [isMax (andmap (lambda (x) (equal? x 0)) (take l (- (/ (length l) 2) 1)))]
        [else (andmap (lambda (x) (equal? x 0)) (reverse (cdr (reverse (drop l (/ (length l) 2))))))]))

(define (evaluation-function2 l)
  (define len (length l))
  (define isMax-list (take l (/ len 2)))
  (define isMin-list (drop l (/ len 2)))
  (- (foldr + 0 isMax-list) (foldr + 0 isMin-list)))

(define l2 '((cons (4 4 4 4 4 4 0 4 4 4 4 4 4 0) 7)))
(define (set-l2 x)
  (set! l2 x))

;;(define DEPTH +inf.0)
(define (turn l pos isMax)
  (define l1 '())
  (define curr-pos 7)
  (define len (length l))
  (define no-of-stones (list-ref l (- pos 1)))
  (define picking-all (append (take l (- pos 1))
                              (list 0)
                              (drop l pos)))
  (define opp-home (if isMax (length l) (/ len 2)))
  (define home (if isMax (/ len 2) len))
  (begin
  (for (begin (set! curr-pos (+ 1 (modulo pos len)))
              (set! l1 picking-all)
              (set! l2 (list (list l1 pos (third mancala-state))))
              (define stones-left no-of-stones))
    : (not (equal? stones-left 0))
    : (set! curr-pos (+ 1 (modulo curr-pos len)))
    : (begin (if (not (equal? curr-pos opp-home))
                 (begin (set! stones-left (- stones-left 1))
                        (set! l1 (list-update l1 (- curr-pos 1) add1))
                        (set! l2 (append l2 (list (list l1 curr-pos (third mancala-state))))))
                        (set! l1 l1))
             ;(displayln l1)
             ;(displayln curr-pos)
             ;(displayln stones-left)
             l1))
  (if (equal? curr-pos 1) (set! curr-pos len) (set! curr-pos (- curr-pos 1)))
  (cons l1 curr-pos)))

(define (turn2 l pos isMax)
  (define len (length l))
  (define opp-home (if isMax (length l) (/ len 2)))
  (define home (if isMax (/ len 2) len))
  (let*
      ([res (turn l pos isMax)]
       [curr-pos (cdr res)]
       [l1 (car res)])
  (cond
    [(and (equal? (list-ref l1 (- curr-pos 1)) 1) (> curr-pos (modulo opp-home len)) (< curr-pos home))
     (set! l1 (list-set l1 (- curr-pos 1) 0))(set! l2 (append l2 (list (list l1 curr-pos (third mancala-state)))))
     (set! l1 (list-update l1 (- home 1) add1))(set! l2 (append l2 (list (list l1 curr-pos (third mancala-state)))))
     (set! l2 (append l2 (list (list (list-set l1 (- len curr-pos 1) 0) curr-pos (third mancala-state)))))
     (set! l1 (list-set l1 (- home 1)
                        (+ (list-ref l1 (- len curr-pos 1))
                           (list-ref l1 (- home 1)))))
     (set! l1 (list-set l1 (- len curr-pos 1) 0))(set! l2 (append l2 (list (list l1 curr-pos (third mancala-state)))))
     (set! l2 (append l2 (list (list l1 20 (third mancala-state)))))
     (cons l1 curr-pos)]
    [else (set! l2 (append l2 (list (list l1 20 (third mancala-state)))))
          (cons l1 curr-pos)])))

(define (update-mancala-state1 mancala-state)
  (define state3 (update-state-helper (take mancala-state 3)))
  (define l (first state3))
  (define isMax (third state3))
  (define opp-home (if isMax (length l) (/ (length l) 2)))
  (define home (if isMax (/ (length l) 2) (length l)))
  (define pos (second state3))
  (define len (length (first mancala-state)))
  (cond
    [(equal? (second state3) home) (set! l2 (append l2 (list (list (car (last l2)) 20 (third mancala-state)))))
     (append state3 (list #t))]
    [(equal? (list-ref (first state3) (- pos 1)) 1) (set! l2 (append l2 (list (list (car (last l2)) 20 (not (third mancala-state)))))) (list l opp-home (not isMax) #t)] 
    [else (append state3 (list #f))]))

(define (update-state-helper state3)
  (define l (first state3))
  (define isMax (third state3))
  (define opp-home (if isMax (length l) (/ (length l) 2)))
  (define home (if isMax (/ (length l) 2) (length l)))
  (define pos (second state3))
  (define len (length l))
  (let ([result (turn (first state3) pos isMax)]) (list (car result) (cdr result) isMax)))

(define (update-mancala-state2 mancala-state)
  (define l (first mancala-state))
  (define pos (second mancala-state))
  (define isMax (third mancala-state))
  (define opp-home (if isMax (length l) (/ (length l) 2)))
  (define home (if isMax (/ (length l) 2) (length l)))
  (let ([result (turn2 l pos isMax)])
    (cond
      [(equal? (cdr result) home) (list (car result) (cdr result) isMax #t)]
      [else (set! l2 (append l2 (list (list (car (last l2)) 20 (not (third mancala-state))))))
       (list (car result) opp-home (not isMax) #t)])))

(define (is-a-candidate pos l)
             (not (equal? (list-ref l (- pos 1)) 0)))

;(define ht (make-hash))

(define (minimax state depth alpha beta eval-func termination-func update-func)
  ;(define id (cons state depth))
  (define pos (second state))
  (define l (first state))
  (define isMax (third state))
  (define opp-home (if isMax (length l) (/ (length l) 2)))
  (define home (if isMax (/ (length l) 2) (length l)))
  (define len (length (first state)))
  (define isFirstTurn (fourth state)) 
  (define func (if isMax max min))
  (define initial-state state)
  (define bestPos #t)
  (define initial-list '())
  (begin
    (for (define i (+ (modulo opp-home len) 1)) : (< i home) : (set! i (add1 i)) : (cond [(is-a-candidate i l) (set! initial-list (cons i initial-list))]))
    (cond [(not (= (length initial-list) 0)) (set! bestPos (list-ref initial-list (random (length initial-list))))])
  ;(displayln state)
  ;(cond [(hash-has-key? ht state) ;(displayln "found")
   ;      (hash-ref ht state)]
    ;    [else
  (cond [(or (termination-func l isMax ) (= depth 0)) (cons (eval-func (first state)) bestPos)]
;        [;(begin (displayln depth)
;          (cons 0 bestPos)]
        [isFirstTurn
           (define value 0)
           (define bestVal (if isMax -inf.0 +inf.0))
           (define i 0)
           (for (begin (set! i (+ (modulo opp-home len) 1))
                       (set! state (list (first state) i isMax #t)))
             :(< i home)
             :(begin (set! i (add1 i)) (set! state (list (first state) i isMax #t)))
             :(cond [(is-a-candidate i (first state))
                     (begin
                       ;(displayln state)
                       ;(displayln i)
                       (set! value (car (minimax (update-func state) (- depth 1) alpha beta eval-func termination-func update-func)))
                       (set! bestVal (func bestVal value))
                       (cond [(equal? (- value bestVal) 0.0) (set! bestPos i)]);(displayln value)(displayln bestVal)
                       ;(displayln bestPos)
                       (cond [isMax (set! alpha (func alpha bestVal))]
                             [else (set! beta (func beta bestVal))])
                       (cond [(<= beta alpha) (set! i home)])
                       )]))
               ;(begin (hash-set!
                ;      ht
                 ;     initial-state
           (cons bestVal bestPos)
           ;) (hash-ref ht initial-state))
           ]
             [else ;(displayln "else") (displayln state)
              (minimax (update-func state) depth alpha beta eval-func termination-func update-func)])))

(define (bestMove1 state depth)
  (cdr (minimax state depth -inf.0 +inf.0 evaluation-function1 termination-condition1 update-mancala-state1)))
(define (bestMove2 state depth)
  (cdr (minimax state depth -inf.0 +inf.0 evaluation-function2 termination-condition2 update-mancala-state2)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set-functions!)
;(world-make)
      