#lang racket
(require 2htdp/universe)
(require (except-in 2htdp/image make-pen make-color))
(require racket/gui)
;(require rsound)
(require lang/posn)
(require "images.rkt")
(provide word-main-frame
         word-welcome-frame-update
         take-level-details
         level-1-frame-update
         level-1-soln-frame-update
         congrats-frame-update
         gameover-frame-update
         diff
         timer-timings
         timer-1
         creation
         set-creation
         level-1-word-list
         create-level?
         set-create-level
         work)

(define final-result '())
(define completed? #f)
(define empty-frame (empty-scene 990 690))
(define down-status "NO")
(define origin (cons 300 100))
(define grid_size 10)
(define index-of-button-down 100000)
(define timer-text "0.0")
(define next_image next_normal)
(define retry_image retry_normal)
(define quit_image quit_normal)
(define play-levels_image play-levels_normal)
(define create-level_image create-level_normal)
(define create-level-words-list '())
(define create-level-grid-size '())
(define create-level-time-limit '())
(define create-level? #f)
(define (set-create-level x)
  (set! create-level? x))
(define creation #f)
(define (set-creation x)
  (set! creation x))
(define back_image back_normal)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;RESET FUNCTION;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (reset-all) (begin (set! next_image next_normal)
                           (set! retry_image retry_normal)
                           (set! quit_image quit_normal)
                           (set! play-levels_image play-levels_normal)
                           (set! create-level_image create-level_normal)
                           (set! back_image back_normal)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;ALGORITHM OF GRID MAKING;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (char->picture a)
  (cond [(equal? a #\A) a_normal]
        [(equal? a #\B) b_normal]
        [(equal? a #\C) c_normal]
        [(equal? a #\D) d_normal]
        [(equal? a #\E) e_normal]
        [(equal? a #\F) f_normal]
        [(equal? a #\G) g_normal]
        [(equal? a #\H) h_normal]
        [(equal? a #\I) i_normal]
        [(equal? a #\J) j_normal]
        [(equal? a #\K) k_normal]
        [(equal? a #\L) l_normal]
        [(equal? a #\M) m_normal]
        [(equal? a #\N) n_normal]
        [(equal? a #\O) o_normal]
        [(equal? a #\P) p_normal]
        [(equal? a #\Q) q_normal]
        [(equal? a #\R) r_normal]
        [(equal? a #\S) s_normal]
        [(equal? a #\T) t_normal]
        [(equal? a #\U) u_normal]
        [(equal? a #\V) v_normal]
        [(equal? a #\W) w_normal]
        [(equal? a #\X) x_normal]
        [(equal? a #\Y) y_normal]
        [(equal? a #\Z) z_normal]))
(define (picture->char a)
  (cond
          [(equal? a a_normal) #\A]
          [(equal? a b_normal) #\B]
          [(equal? a c_normal) #\C]
          [(equal? a d_normal) #\D]
          [(equal? a e_normal) #\E]
          [(equal? a f_normal) #\F]
          [(equal? a g_normal) #\G]
          [(equal? a h_normal) #\H]
          [(equal? a i_normal) #\I]
          [(equal? a j_normal) #\J]
          [(equal? a k_normal) #\K]
          [(equal? a l_normal) #\L]
          [(equal? a m_normal) #\M]
          [(equal? a n_normal) #\N]
          [(equal? a o_normal) #\O]
          [(equal? a p_normal) #\P]
          [(equal? a q_normal) #\Q]
          [(equal? a r_normal) #\R]
          [(equal? a s_normal) #\S]
          [(equal? a t_normal) #\T]
          [(equal? a u_normal) #\U]
          [(equal? a v_normal) #\V]
          [(equal? a w_normal) #\W]
          [(equal? a x_normal) #\X]
          [(equal? a y_normal) #\Y]
          [(equal? a z_normal) #\Z]))

(define (normal->hover a)
  (cond [(equal? a a_normal) a_hover]
        [(equal? a b_normal) b_hover]
        [(equal? a c_normal) c_hover]
        [(equal? a d_normal) d_hover]
        [(equal? a e_normal) e_hover]
        [(equal? a f_normal) f_hover]
        [(equal? a g_normal) g_hover]
        [(equal? a h_normal) h_hover]
        [(equal? a i_normal) i_hover]
        [(equal? a j_normal) j_hover]
        [(equal? a k_normal) k_hover]
        [(equal? a l_normal) l_hover]
        [(equal? a m_normal) m_hover]
        [(equal? a n_normal) n_hover]
        [(equal? a o_normal) o_hover]
        [(equal? a p_normal) p_hover]
        [(equal? a q_normal) q_hover]
        [(equal? a r_normal) r_hover]
        [(equal? a s_normal) s_hover]
        [(equal? a t_normal) t_hover]
        [(equal? a u_normal) u_hover]
        [(equal? a v_normal) v_hover]
        [(equal? a w_normal) w_hover]
        [(equal? a x_normal) x_hover]
        [(equal? a y_normal) y_hover]
        [(equal? a z_normal) z_hover]
        [else a]))
(define (normal->press a)
  (cond [(equal? a a_normal) a_press]
        [(equal? a b_normal) b_press]
        [(equal? a c_normal) c_press]
        [(equal? a d_normal) d_press]
        [(equal? a e_normal) e_press]
        [(equal? a f_normal) f_press]
        [(equal? a g_normal) g_press]
        [(equal? a h_normal) h_press]
        [(equal? a i_normal) i_press]
        [(equal? a j_normal) j_press]
        [(equal? a k_normal) k_press]
        [(equal? a l_normal) l_press]
        [(equal? a m_normal) m_press]
        [(equal? a n_normal) n_press]
        [(equal? a o_normal) o_press]
        [(equal? a p_normal) p_press]
        [(equal? a q_normal) q_press]
        [(equal? a r_normal) r_press]
        [(equal? a s_normal) s_press]
        [(equal? a t_normal) t_press]
        [(equal? a u_normal) u_press]
        [(equal? a v_normal) v_press]
        [(equal? a w_normal) w_press]
        [(equal? a x_normal) x_press]
        [(equal? a y_normal) y_press]
        [(equal? a z_normal) z_press]
        [else a ]))

(define (press->normal a)
  (cond [(equal? a a_press) a_normal]
        [(equal? a b_press) b_normal]
        [(equal? a c_press) c_normal]
        [(equal? a d_press) d_normal]
        [(equal? a e_press) e_normal]
        [(equal? a f_press) f_normal]
        [(equal? a g_press) g_normal]
        [(equal? a h_press) h_normal]
        [(equal? a i_press) i_normal]
        [(equal? a j_press) j_normal]
        [(equal? a k_press) k_normal]
        [(equal? a l_press) l_normal]
        [(equal? a m_press) m_normal]
        [(equal? a n_press) n_normal]
        [(equal? a o_press) o_normal]
        [(equal? a p_press) p_normal]
        [(equal? a q_press) q_normal]
        [(equal? a r_press) r_normal]
        [(equal? a s_press) s_normal]
        [(equal? a t_press) t_normal]
        [(equal? a u_press) u_normal]
        [(equal? a v_press) v_normal]
        [(equal? a w_press) w_normal]
        [(equal? a x_press) x_normal]
        [(equal? a y_press) y_normal]
        [(equal? a z_press) z_normal]
        [else a ]))

(define normal_list (list a_normal b_normal c_normal d_normal e_normal f_normal g_normal h_normal i_normal j_normal k_normal l_normal m_normal n_normal o_normal p_normal q_normal r_normal s_normal
                         t_normal u_normal v_normal w_normal x_normal y_normal z_normal))
(define (fill il r)
  (if (= r (length il)) il
  (if (equal? vacant (list-ref il r)) (fill (list-set il r (list-ref normal_list (random 26))) (+ r 1))
      (fill il (+ r 1)))))

;(define (grid-unsolved word-list size)
;  (begin ;(define sorted-word-list (lsort (map string->list word-list)))
;         (define images-list (make-list (sqr size) vacant))
;         (define positions (append* (build-list size (lambda (y) (build-list size (lambda (x) (make-posn (+ 300 (* x 50)) (+ 100 (* y 50)))))))))
;         (place-images (fill (update images-list (map string->list word-list) size (map string->list word-list)) 0) positions empty-frame)))



(define images-list (make-list (sqr grid_size) vacant))
(define positions (append* (build-list grid_size (lambda (y) (build-list grid_size (lambda (x) (make-posn (+ (car origin) (* x 50)) (+ (cdr origin) (* y 50)))))))))



(define (grid-maker word-list size)
  (begin ;(define sorted-word-list (lsort (map string->list word-list)))
    (define images-list (make-list (sqr size) vacant))
    (define positions (append* (build-list size (lambda (y) (build-list size (lambda (x) (make-posn (+ (car origin) (* x 50)) (+ (cdr origin) (* y 50)))))))))
    (place-images ;(fill (update images-list (map string->list word-list) size (map string->list word-list)) 0)
     (update images-list (map string->list word-list) size (map string->list word-list))
     positions empty-frame)))
(define (checker wl size)
  (if (> (length (argmax length wl)) size) #f
      (if (equal? (length (argmax length wl)) (length (argmax length (remove (argmax length wl) wl)))) #f
          #t)))
(define (update il wl size wl-r)
  (if (not (list? il)) (update (make-list (sqr size) vacant) wl-r size wl-r)
  (if (null? wl) il
  (let ([rand-2 (random 2)]
        [rand-4 (random 4)]
        [check-word (check (car wl) il)])
  (cond [(equal? il (make-list (sqr size) vacant)) (update (place-random il (argmax length wl) rand-4) (remove (argmax length wl) wl) size wl-r)]
        [check-word (if (= rand-2 0) (update check-word (cdr wl) size wl-r)
                                 (update (place-random il (car wl) rand-2) (cdr wl) size wl-r))]
        [(place-random il (car wl) rand-2) (update (place-random il (car wl) rand-4) (cdr wl) size wl-r)]
        [else (update (make-list (sqr size) vacant) wl-r size wl-r)])))))

(define (check word l)
  (let ([ w-w (which-letter word l 0 '())])
  (cond [w-w (placing-word word l w-w)]
        [else #f])))

(define (placing-word word l comb-list)
  (if (null? comb-list) #f
      (let*([random-check (random-checker (list (cons h-adjacent "horizontal")
                                            (cons v-adjacent "vertical")
                                            (cons d-up-adjacent "d-up")
                                            (cons d-down-adjacent "d-down")) l (cdar comb-list) word (caar comb-list))])
  
      (if random-check random-check
          (placing-word word l (cdr comb-list))))))
;  (if (h-adjacent l (cdar comb-list) word (caar comb-list)) (place l (cdar comb-list) word (caar comb-list) "horizontal")
;                 (if (v-adjacent l (cdar comb-list) word (caar comb-list)) (place l (cdar comb-list) word (caar comb-list) "vertical")
;                    (placing-word word l (cdr comb-list))))))

(define (random-checker l l1 p q r)
  (if (null? l) #f
  (let ([rand-4 (random (length l))])
    (if ((car (list-ref l rand-4)) l1 p q r) (place l1 p q r (cdr (list-ref l rand-4)))
        (random-checker (remove (list-ref l rand-4) l) l1 p q r)))))
 ;;the output is list of all matching letters with their position such
 ;; that if one combination fails we shall go to next
(define (which-letter word l r l1)                                                                     
  (if (= r (length word)) (if (null? l1) #f
                              l1)
      (let([index (index-of l (char->picture (list-ref word r)))])
        (if index (which-letter word l (+ r 1) (cons (cons r index) l1))
            (which-letter word l (+ r 1) l1)))))

(define (h-adjacent l posn word letter-posn)
  (if (and (find-vacant l posn letter-posn "behind-horizontal") (find-vacant l posn (- (length word) 1 letter-posn) "forward-horizontal")) #t
      #f))

(define (v-adjacent l posn word letter-posn)
  (if (and (find-vacant l posn letter-posn "behind-vertical") (find-vacant l posn (- (length word) 1 letter-posn) "forward-vertical")) #t
      #f))

(define (d-up-adjacent l posn word letter-posn)
  (if (and (find-vacant l posn letter-posn "behind-d-up") (find-vacant l posn (- (length word) 1 letter-posn) "forward-d-up")) #t
      #f))

(define (d-down-adjacent l posn word letter-posn)
  (if (and (find-vacant l posn letter-posn "behind-d-down") (find-vacant l posn (- (length word) 1 letter-posn) "forward-d-down")) #t
      #f))

(define (find-vacant l posn len string)
  (let([row-size (expt (length l) (/ 1 2))])
  (cond [(= 0 len) #t] 
        [(equal? string "behind-horizontal") (if (< (remainder posn row-size) len) #f
                                                 (if (equal? vacant (list-ref l (- posn 1))) (find-vacant l (- posn 1) (- len 1) string)
                                                 #f))]
        [(equal? string "forward-horizontal") (if (< (- row-size 1 (remainder posn row-size)) len) #f
                                                  (if (equal? vacant (list-ref l (+ 1 posn))) (find-vacant l (+ posn 1) (- len 1) string)
                                                  #f))]
        [(equal? string "behind-vertical") (if (< (quotient posn row-size) len) #f
                                               (if (equal? vacant (list-ref l (- posn row-size))) (find-vacant l (- posn row-size) (- len 1) string)
                                               #f))]
        [(equal? string "forward-vertical") (if (< (- row-size 1 (quotient posn row-size)) len) #f
                                                (if (equal? vacant (list-ref l (+ posn row-size))) (find-vacant l (+ posn row-size) (- len 1) string)
                                                    #f))]
        [(equal? string "behind-d-up") (if (or (< (remainder posn row-size) len) (< (- row-size 1 (quotient posn row-size)) len)) #f
                                           (if (equal? vacant (list-ref l (- (+ posn row-size) 1))) (find-vacant l (- (+ posn row-size) 1) (- len 1) string)
                                               #f))]
        [(equal? string "forward-d-up") (if (or (< (- row-size 1 (remainder posn row-size)) len) (< (quotient posn row-size) len)) #f
                                            (if (equal? vacant (list-ref l (+ (- posn row-size) 1))) (find-vacant l (+ (- posn row-size) 1) (- len 1) string)
                                                #f))]
        [(equal? string "behind-d-down") (if (or (< (remainder posn row-size) len) (< (quotient posn row-size) len)) #f
                                             (if (equal? vacant (list-ref l (- posn row-size 1))) (find-vacant l (- posn row-size 1) (- len 1) string)
                                                 #f))]
        [(equal? string "forward-d-down") (if (or (< (- row-size 1 (remainder posn row-size)) len) (< (- row-size 1 (quotient posn row-size)) len)) #f
                                              (if (equal? vacant (list-ref l (+ posn row-size 1))) (find-vacant l (+ posn row-size 1) (- len 1) string)
                                                  #f))])))

(define (place l posn word letter-posn string)
  (let([row-size (expt (length l) (/ 1 2))])
  (cond [(equal? string "horizontal") (place-word l (- posn letter-posn) word "horizontal")]
        [(equal? string "vertical") (place-word l (- posn (* letter-posn row-size)) word "vertical")]
        [(equal? string "d-up") (place-word l (- (+ posn (* letter-posn row-size)) letter-posn) word "d-up")]
        [(equal? string "d-down") (place-word l (- posn (* letter-posn row-size) letter-posn) word "d=down")])))

(define (place-word l posn word string)
  (let([row-size (expt (length l) (/ 1 2))])
    (cond [(null? word) l]
          [(equal? string "horizontal") (place-word (list-set l posn (char->picture (car word))) (+ posn 1) (cdr word) string)]
          [(equal? string "vertical") (place-word (list-set l posn (char->picture (car word))) (+ posn row-size) (cdr word) string)]
          [(equal? string "d-up") (place-word (list-set l posn (char->picture (car word))) (+ (- posn row-size) 1) (cdr word) string)]
          [(equal? string "d-down") (place-word (list-set l posn (char->picture (car word))) (+ posn row-size 1) (cdr word) string)])))

(define (place-random l word num)
  (let ([irp-h (is-randomly-placeable l word (build-list (length l) (lambda(x) x)) "horizontal")]
        [irp-v (is-randomly-placeable l word (build-list (length l) (lambda(x) x)) "vertical")]
        [irp-d-up (is-randomly-placeable l word (build-list (length l) (lambda(x) x)) "d-up")]
        [irp-d-down (is-randomly-placeable l word (build-list (length l) (lambda(x) x)) "d-down")])
  (cond [(= num 0) (if irp-h (place-word l irp-h word "horizontal")
                       #f)]
        [(= num 1) (if irp-v (place-word l irp-v word "vertical")
                       #f)]
        [(= num 2) (if irp-d-up (place-word l irp-d-up word "d-up")
                       #f)]
        [(= num 3) (if irp-d-down (place-word l irp-d-down word "d-down")
                       #f)])))

(define (is-randomly-placeable l word num-list string)
  (let ([rand-num (random (length l))]
        [row-size (expt (length l) (/ 1 2))])
  (cond [(null? num-list) #f]
        [(equal? string "horizontal") (if (find-vacant l (- rand-num 1) (length word) "forward-horizontal") rand-num
                                          (is-randomly-placeable l word (remove rand-num num-list) string))]
        [(equal? string "vertical") (if (find-vacant l (- rand-num row-size) (length word) "forward-vertical")  rand-num
                                          (is-randomly-placeable l word (remove rand-num num-list) string))]
        [(equal? string "d-up") (if (find-vacant l (- (+ rand-num row-size) 1) (length word) "forward-d-up") rand-num
                                    (is-randomly-placeable l word (remove rand-num num-list) string))]
        [(equal? string "d-down") (if (find-vacant l (- rand-num row-size 1) (length word) "forward-d-down") rand-num
                                      (is-randomly-placeable l word (remove rand-num num-list) string))])))

;(define update-frame-solved (grid-maker (list  "LIFE" "MATHS" "ROUGH" "FEAR" "DEATH" "LAUGHTER" "CHALLENGE") 9))
(define update-frame-unsolved  (if(> (length (argmax length (map string->list (list  "RESPECT" "VILENDERA" "SUKHMAN" "SHUBHAM" "NOSE" "DHANANJAY" "MISHKA")))) 9) "PLEASE INCREASE THE GRID SIZE"
                                  (grid-maker (list  "I" "LOVE" "YOU" "PAPA") 9)))


(define (picture-words word-list ) (map (lambda (x) (text/font x 27 "orange" #f "modern" "normal" "bold" #f)) word-list))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (game-frame) update-frame-unsolved)
;(define (solution-frame stat) ;update-frame-solved)



;(define (solution-world) (big-bang "IT HAS BEGUN"
;                       [name "WORD SEARCH"]
;                       [to-draw solution-frame]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;BUTTONS PLACEMENT;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (button-area x0 y0)
    (lambda (x-button y-button)
      (λ (x y)
      (and (< (abs (- x x-button)) x0) (< (abs (- y y-button)) y0)))))

(define (grid-button-area x y) ((button-area 25 25) x y))
(define (image-button-area x y) ((button-area 138 29) x y))
(define play-button-area (image-button-area 564 322))
(define custom-play-button-area (image-button-area 564 402))
(define (game-area x y grid_size) (and (> x (- (car origin) 25)) (< x (+ (- (car origin) 25) (* grid_size 50))) (> y (- (cdr origin) 25)) (< y (+ (- (cdr origin) 25) (* grid_size 50)))))







;;need to take care of "drag";;
(define (frame-update str inp s1 s2 f f_hover f_press action)
  (define (hover) (f f_hover))
  (define (press) (f f_press))
  (cond [(mouse=? inp "button-down") (begin (set! down-status str)
                                            ;(when sound? (play button_click))
                                            (press)
                                            s1)]
        [(and (not (equal? down-status str)) (mouse=? inp "button-up"))
         (set! down-status "NO") (reset-all) (hover) s1]
        [(mouse=? inp "button-up") (begin (set! down-status "NO")
                                          (hover)
                                          (action)
                                          s2)]
        [(equal? down-status str) (begin (press)
                                         s1)]
        [else (hover) s1]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;MAKING LEVELS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define levels-words-list (list
                           (list "CAT" "ADVICE" "CREEP" "TEAR")
                           (list "FAULT" "BUZZER" "SHUBHAM" "SECRET" "EASTER")
                           (list "LIFE" "RULES" "RHYTHM" "MEXICO" "AROUND" "COURSE" "DAWN")
                           (list "CREATE" "LUSTRE" "UPDATE" "PERFECT" "ANALYSIS" "PROLIFIC")
                           (list  "NOSTALGIA" "FAMOUS" "SUKHMAN" "RESPECT" "HEAVEN" "VENTURE")
                           (list "HAPPY" "EXQUISITE" "MANIFEST" "CIRCUMVENT" "OBLIVION")
                           (list "WORLD" "FLAIR" "SLUMBER" "MENIAL" "BRUTAL")
                           (list "GAME" "CHAT" "BOMBAY" "DHANANJAY" "INTERFACE" "SUCCESS")))

(define special-levels-words-list (list
                                  (list "KHALEESI" "DRAGON" "KNIGHT" "JONSNOW" "TYRION" "ARYA" "WINTER")
                                  (list "JARVIS" "GAUNTLET" "MJOLNIR" "KNOWHERE" "NEBULA" "THANOS" "IRONMAN")))


(define levels-timer (list 45 90 75 85 80 75 75 60))

(define special-levels-timer (list 45 45))

(define levels-grid-size (list 6 8 8 9 10 10 10 12))

(define special-levels-grid-size (list 12 12))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;ALL FRAMES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define general-levels-frame (place-images (list word-box
                                                 timer
                                                 quit_image)
                                                   ;solution)
                                           (list (make-posn 100 200)
                                           (make-posn 890 30)
                                           (make-posn 850 670)
                                           )
                                           ;(make-posn 850 670))
                                           (underlay/xy empty-frame 0 0 background)))

(define level-1-grid_size 12)
(define level-1-word-list (list "HOPE" "OKAY" "YOU"))
(define timer-timings 60)
(define state-new 0)
(define level-1-formed-words-list '())

(define level-1-image-word-list '())

(define level-1-images-list '())

(define filled-level-1-images-list '())
(define filled-level-1-pressed-images-list '())
(define new-image-list-1 '())

(define level-1-positions '())
(define level-1-package '())
(define timer-duration '())

(define (updates) (cond [(> state-new 7) (begin (set! level-1-word-list (list-ref  special-levels-words-list (- state-new 8)))
                                                        (set! level-1-grid_size (list-ref  special-levels-grid-size (- state-new 8)))
                                                        (set! timer-timings (list-ref  special-levels-timer (- state-new 8)))
                                                        (set! level-1-formed-words-list '())
                                                        (set! timer-duration (number->string timer-timings))

                                                        (set! level-1-image-word-list (map (lambda (x) (map (lambda (y) (char->picture y)) (string->list x))) level-1-word-list))

                                                        (set! level-1-images-list (update (make-list (sqr level-1-grid_size) vacant)
                                                                                            (map string->list level-1-word-list) level-1-grid_size (map string->list level-1-word-list)))

                                                        (set! filled-level-1-images-list (fill level-1-images-list 0))
                                                        (set! filled-level-1-pressed-images-list filled-level-1-images-list)
                                                        (set! new-image-list-1 filled-level-1-pressed-images-list)

                                                        (set! level-1-positions (append* (build-list level-1-grid_size (lambda (y) (build-list level-1-grid_size (lambda (x) (make-posn (+ (car origin) (* x 50)) (+ (cdr origin) (* y 50)))))))))
                                                        (set! level-1-package (list level-1-word-list new-image-list-1 level-1-positions level-1-grid_size))
)]
                                [(< state-new 8) (begin (set! level-1-word-list (list-ref levels-words-list  state-new))
                                                        (set! level-1-grid_size (list-ref levels-grid-size state-new))
                                                        (set! timer-timings (list-ref levels-timer state-new))
                                                        (set! level-1-formed-words-list '())
                                                        (set! timer-duration (number->string timer-timings))
                                                        (set! level-1-image-word-list (map (lambda (x) (map (lambda (y) (char->picture y)) (string->list x))) level-1-word-list))

                                                        (set! level-1-images-list (update (make-list (sqr level-1-grid_size) vacant)
                                                                                            (map string->list level-1-word-list) level-1-grid_size (map string->list level-1-word-list)))

                                                        (set! filled-level-1-images-list (fill level-1-images-list 0))
                                                        (set! filled-level-1-pressed-images-list filled-level-1-images-list)
                                                        (set! new-image-list-1 filled-level-1-pressed-images-list)

                                                        (set! level-1-positions (append* (build-list level-1-grid_size (lambda (y) (build-list level-1-grid_size (lambda (x) (make-posn (+ (car origin) (* x 50)) (+ (cdr origin) (* y 50)))))))))
                                                        (set! level-1-package (list level-1-word-list new-image-list-1 level-1-positions level-1-grid_size))
                                                        )]))

(define (create-level-update) (begin (set! level-1-word-list create-level-words-list)
                                                        (set! level-1-grid_size create-level-grid-size)
                                                        (set! timer-timings create-level-time-limit)
                                                        (set! level-1-formed-words-list '())
                                                        (set! timer-duration (number->string timer-timings))
                                                        (set! level-1-image-word-list (map (lambda (x) (map (lambda (y) (char->picture y)) (string->list x))) level-1-word-list))

                                                        (set! level-1-images-list (update (make-list (sqr level-1-grid_size) vacant)
                                                                                            (map string->list level-1-word-list) level-1-grid_size (map string->list level-1-word-list)))

                                                        (set! filled-level-1-images-list (fill level-1-images-list 0))
                                                        (set! filled-level-1-pressed-images-list filled-level-1-images-list)
                                                        (set! new-image-list-1 filled-level-1-pressed-images-list)

                                                        (set! level-1-positions (append* (build-list level-1-grid_size (lambda (y) (build-list level-1-grid_size (lambda (x) (make-posn (+ (car origin) (* x 50)) (+ (cdr origin) (* y 50)))))))))
                                                        (set! level-1-package (list level-1-word-list new-image-list-1 level-1-positions level-1-grid_size))
                                                        ))

(define (level-1_frame) (level-1_frame-const) )



(define (level-1_frame-const)
        (begin 
               (place-images (append
                        new-image-list-1
                        (picture-words (list (string-append "LEVEL - " (number->string (+ state-new 1)))))
                        (picture-words level-1-word-list)
                        (picture-words (list timer-text "DURATION" timer-duration)))
                             (append level-1-positions
                                     (list (make-posn 470 30))
                                     (build-list (length level-1-word-list) (lambda (y) (make-posn 100 (+ 10 30 (+ 27 (quotient 150 (length level-1-word-list))) (* y (+ 27 (quotient 150 (length level-1-word-list))))))))
                                     (list (make-posn 890 70)
                                           (make-posn 890 100)
                                           (make-posn 890 130)))
                             general-levels-frame)))
(define level-1_frame-const-soln
        (begin 
               (place-images (append
                        level-1-images-list
                        (picture-words (list (string-append "LEVEL - " (number->string (+ state-new 1)))))
                        (picture-words level-1-word-list)
                        (picture-words (list "0")))
                             (append level-1-positions
                                     (list (make-posn 470 30))
                                     (build-list (length level-1-word-list) (lambda (y) (make-posn 100 (+ 10 30 (+ 27 (quotient 150 (length level-1-word-list))) (* y (+ 27 (quotient 150 (length level-1-word-list))))))))
                                     (list (make-posn 890 70)))
                             general-levels-frame)))

;(define (level-1_frame-soln stat ) level-1_frame-const-soln)


(define (level-1-frame-update x y inp)
   (cond ;[(> diff 5) "LEVEL_1-SOLN"]
         [(and (mouse=? "button-down" inp)
              (((button-area 97.5 24.5) 850 670) x y) ) "WORD-WELCOME"]
         [(game-area x y level-1-grid_size) (cond [(mouse=? inp "button-down") (begin (set! down-status "YES") (set! index-of-button-down (index-of-image x y level-1-positions 0)) (grid-update x y inp level-1-package "LEVEL-1") "LEVEL-1")]
                                                  [(mouse=? inp "button-up")(begin (set! down-status "NO") (set! index-of-button-down +inf.0) (grid-update x y inp level-1-package "LEVEL-1") "LEVEL-1")]
                                                  [else (begin (grid-update x y inp level-1-package "LEVEL-1") "LEVEL-1" )])]
         [(not (game-area x y level-1-grid_size)) (if (mouse=? inp "button-up") (begin (set! down-status "NO") (set! index-of-button-down +inf.0) (set! new-image-list-1 filled-level-1-images-list) "LEVEL-1")
                                                      (begin (set! new-image-list-1 filled-level-1-images-list) "LEVEL-1")) ]
         [(mouse=? inp "button-down") (begin (set! down-status "YES")  "LEVEL-1")]
         [else (begin (set! down-status "NO") (set! index-of-button-down +inf.0) "LEVEL-1")]))
(define (level-1-soln-frame-update x y inp)
   (cond [(and (mouse=? "button-down" inp)
              (((button-area 125 40) 850 670) x y)) "LEVEL-1"]
        [else "LEVEL_1-SOLN"]))

(define (congrats-frame-update x y inp)
  (cond [(((button-area 97.5 24.5) 503 496) x y) (frame-update "next" inp "CONGRATULATIONS" "LEVEL-1" (lambda (x) (set! next_image x)) next_hover next_press (lambda () (begin
                                                                                                                                                         (set! state-new (+ 1 state-new))
                                                                                                                                                         (if (> state-new 9) "WORD-WELCOME"
                                                                                                                                                         (begin (updates)
                                                                                                                                                         (send timer-1 stop)
                                                                                                                                                         (reset-timer)
                                                                                                                                                         (work 10)
                                                                                                                                                         )))))]
        [(((button-area 97.5 24.5) 225 627) x y) (frame-update "retry" inp "CONGRATULATIONS" "LEVEL-1" (lambda (x) (set! retry_image x)) retry_hover retry_press (lambda () (begin (updates)
                                                                                                                                                                 (send timer-1 stop)
                                                                                                                                                                 (reset-timer)
                                                                                                                                                                 (work 10)
                                                                                                                                                                 )))]
        [(((button-area 97.5 24.5) 781 627) x y) (frame-update "quit" inp "CONGRATULATIONS" "WORD-WELCOME" (lambda (x) (set! quit_image x)) quit_hover quit_press (lambda () (begin
                                                                                                                                                         (send timer-1 stop)
                                                                                                                                                         (reset-timer)
                                                                                                                                                         (set! state-new (+ 1 state-new))
                                                                                                                                                         )))]
        [(and (not (equal? down-status "NO"))
              (mouse=? inp "button-up")) (begin (reset-all)
                                                (set! down-status "NO") "CONGRATULATIONS")]
        [(mouse=? inp "move") (begin (reset-all)
                                     "CONGRATULATIONS")]
        [else "CONGRATULATIONS"]))
(define (gameover-frame-update x y inp)
  ;(displayln (cons diff timer-timings))
  (cond [(((button-area 97.5 24.5) 480 554) x y) (frame-update "retry" inp "GAMEOVER" "LEVEL-1" (lambda (x) (set! retry_image x)) retry_hover retry_press (lambda () (begin (updates)
                                                                                                                                                          (send timer-1 stop)
                                                                                                                                                          (reset-timer)
                                                                                                                                                          (work 10)
                                                                                                                                                          ;(displayln (cons diff 'done))
                                                                                                                                                          )))]
        [(((button-area 97.5 24.5) 480 644) x y) (frame-update "quit" inp "GAMEOVER" "WORD-WELCOME" (lambda (x) (set! quit_image x)) quit_hover quit_press (lambda () (begin
                                                                                                                                                   (send timer-1 stop)
                                                                                                                                                         (reset-timer)
                                                                                                                                                         )))]
        [(and (not (equal? down-status "NO"))
              (mouse=? inp "button-up")) (begin (reset-all)
                                                (set! down-status "NO") "GAMEOVER")]
        [(mouse=? inp "move") (begin (reset-all)
                                     "GAMEOVER")]
        [else "GAMEOVER"]))
(define (word-welcome-frame-update x y inp)
  (cond [(((button-area 157 40) 220 600) x y) (frame-update "play" inp "WORD-WELCOME" "LEVEL-1" (lambda (x) (set! play-levels_image x)) play-levels_hover play-levels_press (lambda () (begin (send timer-1 stop)
                                                                                                                                                                        (reset-timer)
                                                                                                                                                                        (updates)
                                                                                                                                                                        (work 10)
                                                                                                                                                                        )))]
        [(((button-area 157 40) 720 600) x y) (frame-update "create" inp "WORD-WELCOME" "CREATE-LEVEL" (lambda (x) (set! create-level_image x)) create-level_hover create-level_press void)]
        [(((button-area 67 32) 880 50) x y) (frame-update "back" inp "WORD-WELCOME" "GO-TO-HOME" (lambda (x) (set! back_image x)) back_hover back_press void)]
        [(and (not (equal? down-status "NO"))
              (mouse=? inp "button-up")) (begin (reset-all)
                                                (set! down-status "NO") "WORD-WELCOME")]
        [(mouse=? inp "move") (begin (reset-all)
                                     "WORD-WELCOME")]
        [else "WORD-WELCOME"]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;THE BEST PART;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (index-of-image x y positons r)
  ;(begin (displayln positons) (displayln x) (displayln y)
         (if (and (<= (abs (- x (posn-x (car positons)))) 25) (<= (abs (- y (posn-y (car positons)))) 25)) r
             (index-of-image x y (cdr positons) (+ r 1))))



(define (grid-update x y inp package stat)
  ;(begin (displayln package)
  (cond [(and (equal? down-status "NO") (equal? stat "LEVEL-1")) (begin 
                                                                        (if (member (map (lambda (x) (press->normal (car x))) (remove-duplicates level-1-formed-words-list (lambda (x y) (equal? (cdr y) (cdr x)))))
                                                                                    level-1-image-word-list)
                                                                            (begin ;(displayln level-1-word-list)
                                                                                   (set! level-1-word-list (remove (list->string (map (lambda (x) (picture->char x)) (map (lambda (x) (press->normal (car x))) (remove-duplicates level-1-formed-words-list (lambda (x y) (equal? (cdr y) (cdr x))))))) level-1-word-list))
                                                                                   (set! level-1-formed-words-list null)
                                                                                   (set! filled-level-1-images-list filled-level-1-pressed-images-list)
                                                                                   (set! new-image-list-1 (list-update filled-level-1-pressed-images-list
                                                                                                                       (index-of-image x y (list-ref package 2) 0)
                                                                                                                       (lambda (x) (normal->hover x)))))
                                                                            (if (member (reverse (map (lambda (x) (press->normal (car x))) (remove-duplicates level-1-formed-words-list (lambda (x y) (equal? (cdr y) (cdr x))))))
                                                                                    level-1-image-word-list)
                                                                            (begin ;(displayln level-1-word-list)
                                                                                   (set! level-1-word-list (remove (list->string (reverse (map (lambda (x) (picture->char x)) (map (lambda (x) (press->normal (car x))) (remove-duplicates level-1-formed-words-list (lambda (x y) (equal? (cdr y) (cdr x)))))))) level-1-word-list))
                                                                                   (set! level-1-formed-words-list null)
                                                                                   (set! filled-level-1-images-list filled-level-1-pressed-images-list)
                                                                                   (set! new-image-list-1 (list-update filled-level-1-pressed-images-list
                                                                                                                       (index-of-image x y (list-ref package 2) 0)
                                                                                                                       (lambda (x) (normal->hover x)))))
                                                                            (begin (set! level-1-formed-words-list null)
                                                                                   (set! filled-level-1-pressed-images-list filled-level-1-images-list)
                                                                                   (set! new-image-list-1 (list-update filled-level-1-pressed-images-list
                                                                                                                       (index-of-image x y (list-ref package 2) 0)
                                                                                                                       (lambda (x) (normal->hover x))))))))]
        [(and (equal? down-status "YES") (equal? stat "LEVEL-1")) (begin ;(displayln level-1-formed-words-list)
                                                                         ;(displayln (selecting_func index-of-button-down (index-of-image x y (list-ref package 2) 0) level-1-grid_size))
                                                                         (set! level-1-formed-words-list (map (lambda (x) (cons (list-ref filled-level-1-pressed-images-list x) x)) (selecting_func index-of-button-down (index-of-image x y (list-ref package 2) 0) level-1-grid_size)))
                                                                         ;(displayln filled-level-1-pressed-images-list)
                                                                         (set! filled-level-1-pressed-images-list filled-level-1-images-list)
                                                                                                                    (set! filled-level-1-pressed-images-list (last (map (lambda (y) (begin (set! filled-level-1-pressed-images-list (list-update filled-level-1-pressed-images-list
                                                                                                                                                                         y
                                                                                                                                                                         (lambda (x) (normal->press x))))
                                                                                                                                                                                          filled-level-1-pressed-images-list))
                                                                                                                                                (selecting_func index-of-button-down (index-of-image x y level-1-positions 0) level-1-grid_size))))
                                                                                                                    ;(displayln filled-level-1-pressed-images-list)
                                                                                                                    (set! new-image-list-1 (last (map (lambda (y) (begin (set! filled-level-1-pressed-images-list (list-update filled-level-1-pressed-images-list
                                                                                                                                                                         y
                                                                                                                                                                         (lambda (x) (normal->press x))))
                                                                                                                                                                                          filled-level-1-pressed-images-list))
                                                                                                                                                (selecting_func index-of-button-down (index-of-image x y level-1-positions 0) level-1-grid_size)))))]))
(define (selecting_func base-ind arrow-ind grid_sze)
  (let ([diff (abs (- arrow-ind base-ind))])
  (cond [(= diff 0) (list base-ind)]
        [(and (>= diff grid_sze) (= 0 (modulo diff grid_sze))) (if (< base-ind arrow-ind) (build-list (+ 1 (quotient diff grid_sze)) (lambda (x) (+ base-ind (* grid_sze x))))
                                                              (build-list (+ 1 (quotient diff grid_sze)) (lambda (x) (+ arrow-ind (* grid_sze x)))))]
        [(and (not (> diff grid_sze)) (= (quotient base-ind grid_sze) (quotient arrow-ind grid_sze))) (if (< base-ind arrow-ind) (build-list (+ 1 diff) (lambda (x) (+ base-ind x)))
                                 (build-list (+ 1 diff) (lambda (x) (+ arrow-ind x))))]
        [(and (< base-ind arrow-ind) (= (modulo diff (+ grid_sze 1)) 0) (not (= (modulo diff (- grid_sze 1)) 0))) (cons base-ind (takef (build-list (+ 0 (quotient diff (+ grid_sze 1))) (lambda (x) (+ base-ind (* (+ 1 grid_sze) (+ 1 x))))) (lambda (x) (not 
                                                                                                                                                                          (equal? (modulo x grid_sze) 0)))))]
        [(and (> base-ind arrow-ind) (= (modulo diff (+ grid_sze 1)) 0) (not (= (modulo diff (- grid_sze 1)) 0))) (cons base-ind (takef (build-list (+ 0 (quotient diff (+ grid_sze 1))) (lambda (x) (- base-ind (* (+ 1 grid_sze) (+ 1 x))))) (lambda (x) (not (equal? (modulo x grid_sze) (- grid_sze 1))
                                                                                                                                                                          ))))]
        [(and (= (modulo diff (- grid_sze 1)) 0) (< base-ind arrow-ind) (not (= (modulo diff (+ grid_sze 1)) 0))) (cons base-ind (takef (build-list (+ 0 (quotient diff (- grid_sze 1))) (lambda (x) (+ base-ind (* (- grid_sze 1) (+ 1 x))))) (lambda (x) (not (equal? (modulo x grid_sze) (- grid_sze 1))
                                                                                                                                                                          ))))]
        [(and (= (modulo diff (- grid_sze 1)) 0) (> base-ind arrow-ind) (not (= (modulo diff (+ grid_sze 1)) 0))) (cons base-ind (takef (build-list (+ 0 (quotient diff (- grid_sze 1))) (lambda (x) (- base-ind (* (- grid_sze 1) (+ 1 x))))) (lambda (x) (not
                                                                                                                                                                          (equal? (modulo x grid_sze) 0)))))]
        [(and (= (modulo diff (- grid_sze 1)) 0) (= (modulo diff (+ grid_sze 1)) 0)) (let ([upar-left (takef (build-list (+ 0 (quotient diff (+ grid_sze 1))) (lambda (x) (+ base-ind (* (+ 1 grid_sze) (+ 1 x))))) (lambda (x) (not 
                                                                                                                                                                          (equal? (modulo x grid_sze) 0))))]
                                                                                           [upar-right (takef (build-list (+ 0 (quotient diff (- grid_sze 1))) (lambda (x) (+ base-ind (* (- grid_sze 1) (+ 1 x))))) (lambda (x) (not (equal? (modulo x grid_sze) (- grid_sze 1))
                                                                                                                                                                          )))]
                                                                                           [neeche-left (takef (build-list (+ 0 (quotient diff (- grid_sze 1))) (lambda (x) (- base-ind (* (- grid_sze 1) (+ 1 x))))) (lambda (x) (not
                                                                                                                                                                          (equal? (modulo x grid_sze) 0))))]
                                                                                           [neeche-right (takef (build-list (+ 0 (quotient diff (+ grid_sze 1))) (lambda (x) (- base-ind (* (+ 1 grid_sze) (+ 1 x))))) (lambda (x) (not (equal? (modulo x grid_sze) (- grid_sze 1))
                                                                                                                                                                          )))])
                                                                                       (if (< base-ind arrow-ind) (if (= (quotient diff (+ grid_sze 1)) (length upar-left)) (cons base-ind upar-left)
                                                                                           (cons base-ind upar-right))
                                                                                           (if (= (quotient diff (+ grid_sze 1)) (length neeche-right)) (cons base-ind neeche-right)
                                                                                               (cons base-ind neeche-left))))]
        [else (list base-ind)])))
        
  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;FRAME WORKING;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (word-main-frame stat)
  (begin  ;(displayln stat)
          (cond [(equal? stat "LEVEL-1") (level-1_frame)]
               [(equal? stat "LEVEL_1-SOLN") level-1_frame-const-soln]
               [(equal? stat "SOLUTION") (game-frame)]
               [(equal? stat "GAMEOVER") (gameover/retry-frame)]
               [(equal? stat "CONGRATULATIONS") (congrats-frame)]
               [(equal? stat "WORD-WELCOME") (welcome-frame)]
               [(equal? stat "CREATE-LEVEL") (welcome-frame)]
               [(equal? stat "GO-TO-HOME") (welcome-frame)]
               [else (error "WRONG STAT PASSED")])))
;(define (create-level-frame) empty-frame)
(define (take-level-details)
  (define fr (new frame%
                  (label "Enter level details")
                  (width 350)
                  (height 200)))
  (define msg
    (new message%
         [parent fr]
         [label "ENTER THE WORDS SEPERATED BY SPACES"]
         [vert-margin 5]))
  (define words-field
    (new text-field%
         [parent fr]
         [label "Words: "]
         [vert-margin 5]))
  (define grid-field
   (new text-field%
         [parent fr]
         [label "Grid Size: "]
         [vert-margin 5]))
  (define time-field
   (new text-field%
         [parent fr]
         [label "Time Limit: "]
         [vert-margin 5]))
  (define error-message
    (new message%
         [parent fr]
         [label "                                                                \n                                          "]
         [vert-margin 5]))
  (define submit-button
    (new button%
         [parent fr]
         [label "Submit"]
         (callback (lambda (b e) (let ([words (send words-field get-value)]
                                       [grid-size (send grid-field get-value)]
                                       [time-limit (send time-field get-value)])
                                   (cond [(or (equal? words "") (equal? grid-size "") (equal? time-limit ""))
                                          (send error-message set-label "Please enter the details correctly")]
                                         [(or (not (number? (string->number grid-size))) (not (number? (string->number time-limit))))
                                          (send error-message set-label "Please enter the details correctly")]
                                         [(> (string-length (argmax string-length (string-split words))) (string->number grid-size))
                                          (send error-message set-label "Please make sure that the length of\nevery word is less than the grid-size")]
                                         [(> (string->number grid-size) 12)
                                          (send error-message set-label "Please make sure that the grid-size\nis less than 13")]
                                         [else (set! create-level-words-list (string-split (string-upcase words)))
                                               (set! create-level-grid-size (string->number grid-size))
                                               (set! create-level-time-limit (string->number time-limit))
                                               (create-level-update)
                                               (set! create-level? #t)
                                               (set! creation #t)
                                               (send fr show #f)]))))))
  (send fr show #t))
                                   
         
(define (welcome-frame) (place-images (list wordsearch
                                            play-levels_image
                                            create-level_image
                                            back_image)
                                      (list (make-posn 468 300)
                                            (make-posn 220 600)
                                            (make-posn 720 600)
                                            (make-posn 880 50))
                                      (underlay/xy empty-frame 0 0 welcome-background)))

(define (gameover/retry-frame) (place-images (list retry_image
                                                   quit_image)
                                             (list (make-posn 480 554 )
                                                   (make-posn 480 644 ))
                                             (underlay/xy empty-frame 0 0 gameover)))
(define (congrats-frame) (place-images (list next_image
                                             retry_image
                                             quit_image)
                                       (list (make-posn 503 496 )
                                             (make-posn 225 627 )
                                             (make-posn 781 627 ))
  (underlay/xy empty-frame 0 0 congratulations)))
(define (word-game-world) ;(work 10)
  (big-bang "WORD-WELCOME"
                       [name "WORD SEARCH"]
                       [to-draw word-main-frame]
                       [on-mouse (λ (stat x y inp)
                        (cond [(or (mouse=? inp "enter")
                                   (mouse=? inp "leave")) stat]
                              ;[(and (equal? stat "LEVEL-1") (= diff 5.0)) "LEVEL_1-SOLN"]
                              [(equal? stat "WORD-WELCOME") (word-welcome-frame-update x y inp)]
                              [(equal? stat "CREATE-LEVEL") (take-level-details) (word-welcome-frame-update x y inp)]
                              [(equal? stat "LEVEL-1") (level-1-frame-update x y inp)]
                              [(equal? stat "LEVEL_1-SOLN") (level-1-soln-frame-update x y inp)]
                              [(equal? stat "CONGRATULATIONS") (congrats-frame-update x y inp)]
                              [(equal? stat "GAMEOVER") (gameover-frame-update x y inp)]
                              [else (error "WRONG STAT PASSED IN ON-MOUSE")]))]
                       [on-tick (λ (stat) (cond [(> diff timer-timings) (begin (send timer-1 stop) "GAMEOVER")]
                                                [(and (null? level-1-word-list) (equal? stat "LEVEL-1") (equal? creation #t))
                                                 (send timer-1 stop) (set! creation #f) "WORD-WELCOME"]
                                                [(and (null? level-1-word-list) (equal? stat "LEVEL-1")) (begin (send timer-1 stop) "CONGRATULATIONS")]
                                                [create-level? (begin (set! create-level? #f) (work 10) "LEVEL-1")]
                                                [else stat]))]))
                              

(define begin-1 0.0)
(define diff -2.0)
;(define timer-text "0.0")
(define timer-1 (new timer%
                   [notify-callback (lambda ()
                                       (set! diff (- (current-seconds) begin-1))
                                       (set! timer-text (number->string diff)))]
                   
                    [interval #f]))
(define (reset-timer) (begin (set! timer-text "0.0")
                             (set! diff -2.0)
                               ))

(define (work num) (begin (set! begin-1 (current-seconds)) (send timer-1 start num)))

;(word-game-world)