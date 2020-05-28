#lang racket/gui
(provide (all-defined-out))
(require 2htdp/universe)
(require 2htdp/image)
(require racket/date)
(require lang/posn)
(require images/icons/style)
(require "image.rkt")

;;;;;;;;;;;DICTIONARY CREATION;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (read-hist-word-list file-path #:pick? [choice 'word])
  (call-with-input-file file-path
    (lambda (fin)
      (for/list ([word-count (in-lines fin)])
        (let ([wc-split (string-split word-count #:trim? #t)])
          (match choice
            ['word   (car wc-split)]
            ['counts (string->number (cadr wc-split))]
            ['both   (cons (car wc-split)
                           (string->number (cadr wc-split)))]))))))

(define dictionary
  (read-hist-word-list "data/google-books-common-words.txt"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define PROFILE-PIC default-profile-pic)
(define (set-profile-pic x) (set! PROFILE-PIC x))
(define ORIGINAL-PROFILE-PIC default-profile-pic)
(define SETTINGS-PROFILE-PIC (resize-appropriately ORIGINAL-PROFILE-PIC 330))
(define (set-original-profile-pic x) (begin0 (set! ORIGINAL-PROFILE-PIC x)
                                             (set! SETTINGS-PROFILE-PIC
                                                   (resize-appropriately ORIGINAL-PROFILE-PIC 330))))
(define USER-DETAILS-IMAGES (make-list 3 empty-image))
(define user-status "")
(define USER-STATUS-IMAGE empty-image)
(define (set-user-status x)
  (set! USER-STATUS-IMAGE (text/font (set-form x 40) 25 "ForestGreen" "Microsoft Sans Serif" "decorative" "normal" "bold" #f))
  (set! user-status x))
(define user-details '())
(define (set-user-details x)
  (set! user-details x)
  (set! USER-DETAILS-IMAGES (map (λ (x) (resize-appropriately x 640))
                                 (list (text/font (car user-details) 36 "DodgerBlue" "Microsoft Sans Serif" "decorative" "normal" "bold" #f)
                                       (text/font (second user-details) 24 "DodgerBlue" "Microsoft Sans Serif" "decorative" "normal" "normal" #f)
                                       (text/font (third user-details) 24 "DodgerBlue" "Microsoft Sans Serif" "decorative" "normal" "normal" #f)))))

(define requested-details (list "" "" "" ""))
(define requested-profile-pic empty-image)
(define requested-details-images (make-list 4 empty-image))

(define (set-form x d)
  (define (helper l)
    (if (<= (length l) d) l
        (append (take l d) (list #\newline) (helper (drop l d)))))
  (list->string (helper (string->list x))))
  
(define (set-requested-profile-pic x)
  (set! requested-profile-pic (resize-appropriately x 400)))
(define (set-requested-details x)
  (set! requested-details x)
  (set! requested-details-images
        (list (text/font (car requested-details) 36 "DodgerBlue" "Microsoft Sans Serif" "decorative" "normal" "bold" #f)
              (text/font (second requested-details) 24 "DodgerBlue" "Microsoft Sans Serif" "decorative" "normal" "normal" #f)
              (text/font (third requested-details) 24 "DodgerBlue" "Microsoft Sans Serif" "decorative" "normal" "normal" #f)
              (text/font (set-form (fourth requested-details) 45) 24 "ForestGreen" "Microsoft Sans Serif" "decorative" "normal" "normal" #f))))

(define starting-button1-image starting-button1-rest)
(define starting-button2-image starting-button2-rest)
(define starting-button3-image starting-button3-rest)
(define about-users-image about-users-rest)
(define about-back-image about-back-rest)
(define be-a-user-image be-a-user-rest)
(define be-a-host-image be-a-host-rest)
(define login-image login-rest)
(define signup-image signup-rest)
(define welcome-back-image back-rest)
(define start-chat-image start-chat-rest)
(define play-mancala-image play-mancala-rest)
(define play-word-search-image play-word-search-rest)
(define change-profile-pic-image change-profile-pic-rest)
(define go-offline-image go-offline-rest)
(define online-settings-image online-settings-rest)
(define help-online-image help-online-rest)
(define change-password-image change-password-rest)
(define change-status-image change-status-rest)
(define change-details-image change-details-rest)
(define settings-back-image settings-back-rest)
(define user-acc-back-image user-acc-back-rest)
(define online-help-back-image settings-back-rest)

(define down_status "NO")

(define delta (make-object style-delta%))
(define change-delta (begin (send delta set-delta 'change-smoothing 'smoothed)
                            (send delta set-delta 'change-size 15)
                            (send delta set-delta-face "Arial")))
(define online-delta (make-object style-delta%))
(define change-online-delta (begin (send online-delta set-delta 'change-smoothing 'smoothed)
                            (send online-delta set-delta 'change-size 15)
                            (send online-delta set-delta 'change-bold)
                            (send online-delta set-delta-face "Constantia")))
;(define PLAYING-MANCALA? #f)
(define our-font-1
  (make-object font%
    15
    "Courier"
    'decorative))

(define our-font-2
  (make-object font%
    15
    "Arial"
    'decorative))

(define our-font-3
  (make-object font%
    15
    "Constantia"
    'decorative
    'slant
    'bold))

(define text-field-font
  (make-object font%
    10
    "Arial"
    'default))

(define empty_frame-starting (underlay/xy (empty-scene 1351 690) 0 0 background))
(define empty_frame (empty-scene 1351 690))


(define (create-image-frame image)
  (let* ([iw (image-width image)]
         [ih (image-height image)]
         [fw (if (> iw 600) iw 600)]
         [fh (if (> ih 600) ih 600)])
    (underlay/xy (empty-scene fw
                              fh) (/ (- fw iw) 2) (/ (- fh ih) 2) image)))

(define (starting-frame)
  (place-images (list starting-button1-image
                      starting-button2-image
                      starting-button3-image
                      about-users-image)
                (list (make-posn 300 480)
                      (make-posn 683 480)
                      (make-posn 1066 480)
                      (make-posn 1306 45))
                empty_frame-starting))

(define (about-frame)
  (place-images (list about-back-image)
                (list (make-posn 1301 647))
                about-background))

(define (decision-frame)
  (place-images (list be-a-user-image
                      be-a-host-image)
                (list (make-posn 490.5 380)
                      (make-posn 860.5 380))
                main-background))

(define (welcome_frame)
  (place-images (list login-image
                      signup-image
                      welcome-back-image)
                (list (make-posn 335.5 615)
                      (make-posn 675.5 615) 
                      (make-posn 1015.5 615))
                welcome-background))

(define (wall-frame)
  (place-images (list PROFILE-PIC
                      USER-STATUS-IMAGE
                      (car USER-DETAILS-IMAGES)
                      (second USER-DETAILS-IMAGES)
                      (third USER-DETAILS-IMAGES)
                      start-chat-image
                      play-mancala-image
                      play-word-search-image
                      go-offline-image
                      online-settings-image
                      help-online-image)
                (list (make-posn (+ 10 (/ (image-width PROFILE-PIC) 2))
                                 (+ 80 (/ (image-height PROFILE-PIC ) 2)))
                      (make-posn (+ 10 (/ (image-width USER-STATUS-IMAGE) 2))
                                 (+ 100 (image-height PROFILE-PIC) (/ (image-height USER-STATUS-IMAGE) 2)))
                      (make-posn (+ 10 (/ (image-width (car USER-DETAILS-IMAGES)) 2))
                                 (+ 520 (/ (image-height (car USER-DETAILS-IMAGES)) 2)))
                      (make-posn (+ 10 (/ (image-width (second USER-DETAILS-IMAGES)) 2))
                                 (+ 530 (image-height (car USER-DETAILS-IMAGES)) (/ (image-height (second USER-DETAILS-IMAGES)) 2)))
                      (make-posn (+ 10 (/ (image-width (third USER-DETAILS-IMAGES)) 2))
                                 (+ 540
                                    (image-height (car USER-DETAILS-IMAGES))
                                    (image-height (second USER-DETAILS-IMAGES)) (/ (image-height (third USER-DETAILS-IMAGES)) 2)))
                      (make-posn 827 322)
                      (make-posn 912 446)
                      (make-posn 892 570)
                      (make-posn 1297 53)
                      (make-posn 1226 53)
                      (make-posn 1324 97))
                wall-background))

(define (settings-frame)
  (place-images (list change-profile-pic-image
                      SETTINGS-PROFILE-PIC
                      change-password-image
                      change-status-image
                      change-details-image
                      settings-back-image)
                (list (make-posn (+ 355 (/ (image-width SETTINGS-PROFILE-PIC) 2))
                                 (+ 388 (/ (image-height SETTINGS-PROFILE-PIC) 2)))
                      (make-posn 375 408)
                      (make-posn 769 267)
                      (make-posn 739.5 357)
                      (make-posn 781 447)
                      (make-posn 1106 591))
                (underlay/align "middle" "middle" (wall-frame) (frame online-settings-background))))
(define status-text (text/font "Status" 36 "ForestGreen" "Microsoft Sans Serif" "decorative" "normal" "bold" #t))

(define (user-account-frame)
  (let ([w (image-width requested-profile-pic)]
        [h (image-height requested-profile-pic)])
  (place-images (append (list requested-profile-pic
                              status-text
                              user-acc-back-image)
                        requested-details-images)
                (list (make-posn (- 1141 (/ w 2))
                                 (+ 40 (/ h 2)))
                      (make-posn (+ 200 (/ (- 941 w) 2))
                                 (+ 40 (/ (image-height status-text) 2)))
                      (make-posn 231 (+ 195 h)) 
                      (make-posn (- 1141 (/ (image-width (car requested-details-images)) 2))
                                 (+ 50 h (/ (image-height (car requested-details-images)) 2)))
                      (make-posn (- 1141 (/ (image-width (cadr requested-details-images)) 2))
                                 (+ 55
                                    h
                                    (image-height (car requested-details-images))
                                    (/ (image-height (cadr requested-details-images)) 2)))
                      (make-posn (- 1141 (/ (image-width (caddr requested-details-images)) 2))
                                 (+ 60
                                    h
                                    (image-height (car requested-details-images))
                                    (image-height (cadr requested-details-images))
                                    (/ (image-height (caddr requested-details-images)) 2)))
                      (make-posn (+ 210 (/ (image-width (last requested-details-images)) 2))
                                 (+ 60 (image-height status-text) (/ (image-height (last requested-details-images)) 2))))
                (underlay/xy (wall-frame) 200 30 (frame (crop 0 0 951 (+ 200 h) user-account-frame-back))))))

(define (online-help-frame)
  (place-images (list online-help-back-image)
                (list (make-posn 1306 639))
                online-help-background))
                

(define (button-area x0 y0)
    (lambda (x-button y-button)
      (λ (x y)
      (and (< (abs (- x x-button)) x0) (< (abs (- y y-button)) y0)))))
(define (image-button-area x y) ((button-area 147 35.5) x y))
(define (welcome-button-area x y) ((button-area 135 60) x y))
(define (back-area x y) ((button-area 100 35.5) x y))
(define login-area (welcome-button-area 335.5 615))
(define signup-area (welcome-button-area 675.5 615))
(define welcome-back-area (welcome-button-area 1015.5 615))
(define (starting-buttons-area x y) ((button-area 135 160) x y))
(define starting-button1-area (starting-buttons-area 300 480))
(define starting-button2-area (starting-buttons-area 683 480))
(define starting-button3-area (starting-buttons-area 1066 480))
(define about-users-button-area ((button-area 30 30) 1306 45))
(define about-back-button-area ((button-area 40 33) 1301 647))
(define be-a-user-button-area (starting-buttons-area 490.5 380))
(define be-a-host-button-area (starting-buttons-area 860.5 380))
(define start-chat-area ((button-area 177 42) 827 322))
(define play-mancala-area ((button-area 262 42) 912 446))
(define play-word-search-area ((button-area 242 42) 892 570))
(define change-profile-pic-area ((button-area 20 20) (+ 355 (/ (image-width SETTINGS-PROFILE-PIC) 2))
                                                       (+ 388 (/ (image-height SETTINGS-PROFILE-PIC) 2))))
(define go-offline-area ((button-area 54 17) 1297 53))
(define online-settings-area ((button-area 17 17) 1226 53))
(define help-online-area ((button-area 17 17) 1324 97))
(define change-password-area ((button-area 169 25) 769 267))
(define change-status-area ((button-area 139.5 25) 739.5 357))
(define change-details-area ((button-area 181 25) 781 447))
(define settings-back-area ((button-area 35 41) 1106 591))
(define online-help-back-area ((button-area 35 41) 1306 639))

(define (reset-all)
  (set! login-image login-rest)
  (set! signup-image signup-rest)
  (set! welcome-back-image back-rest)
  (set! start-chat-image start-chat-rest)
  (set! starting-button1-image starting-button1-rest)
  (set! starting-button2-image starting-button2-rest)
  (set! starting-button3-image starting-button3-rest)
  (set! about-users-image about-users-rest)
  (set! about-back-image about-back-rest)
  (set! be-a-user-image be-a-user-rest)
  (set! be-a-host-image be-a-host-rest)
  (set! start-chat-image start-chat-rest)
  (set! play-mancala-image play-mancala-rest)
  (set! play-word-search-image play-word-search-rest)
  (set! change-profile-pic-image change-profile-pic-rest)
  (set! go-offline-image go-offline-rest)
  (set! online-settings-image online-settings-rest)
  (set! help-online-image help-online-rest)
  (set! change-password-image change-password-rest)
  (set! change-status-image change-status-rest)
  (set! change-details-image change-details-rest)
  (set! settings-back-image settings-back-rest)
  (set! user-acc-back-image user-acc-back-rest)
  (set! online-help-back-image settings-back-rest))

(define (user-update inp stat f up-img1 up-img2 str)
  (cond [(mouse=? inp "button-down") (f up-img1)
                                     (set! down_status "YES")
                                     stat]
        [(and (mouse=? inp "button-up")
              (not (equal? down_status "YES"))) (set! down_status "NO")
                                                (reset-all)
                                                (f up-img2)
                                                stat]
        [(mouse=? inp "button-up") (set! down_status "NO")
                                   (f up-img2)
                                   str]
        [(equal? down_status "YES") (f up-img2)
                                    stat]
        [else (f up-img2) stat]))

(define (login-update inp)
  (user-update inp "WELCOME" (lambda (x) (set! login-image x)) login-down login-hover "LOGIN"))

(define (signup-update inp)
  (user-update inp "WELCOME" (lambda (x) (set! signup-image x)) signup-down signup-hover "NEW-USER"))

(define (welcome-back-update inp)
  (user-update inp "WELCOME" (lambda (x) (set! welcome-back-image x)) back-down back-hover "STARTING-FRAME"))

(define (starting-button1-update inp)
  (user-update inp "STARTING-FRAME" (lambda (x) (set! starting-button1-image x)) starting-button1-down starting-button1-hover "WELCOME"))

(define (starting-button2-update inp)
  (user-update inp "STARTING-FRAME" (lambda (x) (set! starting-button2-image x)) starting-button2-down starting-button2-hover "RUN-WORD-SEARCH"))

(define (starting-button3-update inp)
  (user-update inp "STARTING-FRAME" (lambda (x) (set! starting-button3-image x)) starting-button3-down starting-button3-hover "RUN-MANCALA"))

(define (about-users-button-update inp)
  (user-update inp "STARTING-FRAME" (lambda (x) (set! about-users-image x)) about-users-down about-users-hover "ABOUT-USERS"))

(define (about-back-update inp)
  (user-update inp "ABOUT-USERS" (lambda (x) (set! about-back-image x)) about-back-down about-back-hover "STARTING-FRAME"))

(define (be-a-user-update inp)
  (user-update inp "DECISION-FRAME" (lambda (x) (set! be-a-user-image x)) be-a-user-down be-a-user-hover "BE-A-USER"))

(define (be-a-host-update inp)
  (user-update inp "DECISION-FRAME" (lambda (x) (set! be-a-host-image x)) be-a-host-down be-a-host-hover "RUN-UNIVERSE"))

(define (start_chat_update inp)
  (user-update inp "GO-TO-WALL" (lambda (x) (set! start-chat-image x)) start-chat-down start-chat-hover "START-THIS-CHAT"))

(define (play-mancala-update inp)
  (user-update inp "GO-TO-WALL" (lambda (x) (set! play-mancala-image x)) play-mancala-down play-mancala-hover "RUN-MANCALA-ONLINE"))

(define (play-word-search-update inp)
  (user-update inp "GO-TO-WALL" (lambda (x) (set! play-word-search-image x)) play-word-search-down play-word-search-hover "RUN-WORD-SEARCH-ONLINE"))

(define (go-offline-update inp)
  (user-update inp "GO-TO-WALL" (lambda (x) (set! go-offline-image x)) go-offline-down go-offline-hover "POSSIBLY-GOING-OFFLINE"))

(define (online-settings-update inp)
  (user-update inp "GO-TO-WALL" (lambda (x) (set! online-settings-image x)) online-settings-down online-settings-hover "ONLINE-SETTINGS"))

(define (help-online-update inp)
  (user-update inp "GO-TO-WALL" (lambda (x) (set! help-online-image x)) help-online-down help-online-hover "ONLINE-HELP"))

(define (change-password-update inp)
  (user-update inp "ONLINE-SETTINGS" (lambda (x) (set! change-password-image x)) change-password-down change-password-hover "CHANGE-PASSWORD"))

(define (change-status-update inp)
  (user-update inp "ONLINE-SETTINGS" (lambda (x) (set! change-status-image x)) change-status-down change-status-hover "CHANGE-STATUS"))

(define (change-details-update inp)
  (user-update inp "ONLINE-SETTINGS" (lambda (x) (set! change-details-image x)) change-details-down change-details-hover "CHANGE-PERSONAL-DETAILS"))

(define (change-profile-pic-update inp)
  (user-update inp "ONLINE-SETTINGS" (lambda (x) (set! change-profile-pic-image x)) change-profile-pic-down change-profile-pic-hover "CHANGE-PROFILE-PIC"))

(define (settings-back-update inp)
  (user-update inp "ONLINE-SETTINGS" (lambda (x) (set! settings-back-image x)) settings-back-down settings-back-hover "GO-TO-WALL"))

(define (user-acc-back-update inp)
  (user-update inp "USER-ACCOUNT-FRAME" (lambda (x) (set! user-acc-back-image x)) user-acc-back-down user-acc-back-hover "GO-TO-WALL"))

(define (online-help-back-update inp)
  (user-update inp "ONLINE-HELP" (lambda (x) (set! online-help-back-image x)) settings-back-down settings-back-hover "GO-TO-WALL"))

(define (starting-frame-update x y inp)
  (cond [(starting-button1-area x y) (starting-button1-update inp)]
        [(starting-button2-area x y) (starting-button2-update inp)]
        [(starting-button3-area x y) (starting-button3-update inp)]
        [(about-users-button-area x y) (about-users-button-update inp)]
        [(and (not (equal? down_status "NO"))
              (mouse=? inp "button-up")) (reset-all)
                                         (set! down_status "NO")
                                         "STARTING-FRAME"]
        [(mouse=? inp "move") (reset-all) "STARTING-FRAME"]
        [else "STARTING-FRAME"]))

(define (about-users-update x y inp)
  (cond [(about-back-button-area x y) (about-back-update inp)]
        [(and (not (equal? down_status "NO"))
              (mouse=? inp "button-up")) (reset-all)
                                         (set! down_status "NO")
                                         "ABOUT-USERS"]
        [(mouse=? inp "move") (reset-all) "ABOUT-USERS"]
        [else "ABOUT-USERS"]))

(define (decision-frame-update x y inp)
  (cond [(be-a-user-button-area x y) (be-a-user-update inp)]
        [(be-a-host-button-area x y) (be-a-host-update inp)]
        [(and (not (equal? down_status "NO"))
              (mouse=? inp "button-up")) (reset-all)
                                         (set! down_status "NO")
                                         "DECISION-FRAME"]
        [(mouse=? inp "move") (reset-all) "DECISION-FRAME"]
        [else "DECISION-FRAME"]))


(define (welcome_frame_update x y inp)
  (cond [(login-area x y) (login-update inp)]
        [(signup-area x y) (signup-update inp)]
        [(welcome-back-area x y) (welcome-back-update inp)]
        [(and (not (equal? down_status "NO"))
              (mouse=? inp "button-up")) (reset-all)
                                         (set! down_status "NO")
                                         "WELCOME"]
        [(mouse=? inp "move") (reset-all) "WELCOME"]
        [else "WELCOME"]))



(define (wall_frame_update x y inp)
  (cond [(start-chat-area x y) (start_chat_update inp)]
        [(play-mancala-area x y) (play-mancala-update inp)]
        [(play-word-search-area x y) (play-word-search-update inp)]
        [(go-offline-area x y) (go-offline-update inp)]
        [(online-settings-area x y) (online-settings-update inp)]
        [(help-online-area x y) (help-online-update inp)]
        [(and (not (equal? down_status "NO"))
              (mouse=? inp "button-up")) (reset-all)
                                         (set! down_status "NO")
                                         "GO-TO-WALL"]
        [(mouse=? inp "move") (reset-all) "GO-TO-WALL"]
        [else "GO-TO-WALL"]))

(define (online_settings_frame_update x y inp)
  (cond [(change-password-area x y) (change-password-update inp)]
        [(change-status-area x y) (change-status-update inp)]
        [(change-details-area x y) (change-details-update inp)]
        [(((button-area 20 20) (+ 355 (/ (image-width SETTINGS-PROFILE-PIC) 2))
                               (+ 388 (/ (image-height SETTINGS-PROFILE-PIC) 2))) x y) (change-profile-pic-update inp)]
        [(settings-back-area x y) (settings-back-update inp)]
        [(and (not (equal? down_status "NO"))
              (mouse=? inp "button-up")) (reset-all)
                                         (set! down_status "NO")
                                         "ONLINE-SETTINGS"]
        [(mouse=? inp "move") (reset-all) "ONLINE-SETTINGS"]
        [else "ONLINE-SETTINGS"]))

(define (user-acc-frame-update x y inp)
  (cond [(((button-area 21 25) 231 (+ 195 (image-height requested-profile-pic))) x y) (user-acc-back-update inp)]
        [(and (not (equal? down_status "NO"))
              (mouse=? inp "button-up")) (reset-all)
                                         (set! down_status "NO")
                                         "USER-ACCOUNT-FRAME"]
        [(mouse=? inp "move") (reset-all) "USER-ACCOUNT-FRAME"]
        [else "USER-ACCOUNT-FRAME"]))

(define (online-help-update x y inp)
  (cond [(online-help-back-area x y) (online-help-back-update inp)]
        [(and (not (equal? down_status "NO"))
              (mouse=? inp "button-up")) (reset-all)
                                         (set! down_status "NO")
                                         "ONLINE-HELP"]
        [(mouse=? inp "move") (reset-all) "ONLINE-HELP"]
        [else "ONLINE-HELP"]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (find-possible-subs n partial-word history)
  (let* ([word-len (string-length partial-word)]
         [first-list (extract-words n partial-word word-len (if (null? history)
                                                                history
                                                                (map (λ (x) (car x)) history))
                                    '())]
         [first-list-len (length first-list)])
    (if (>= first-list-len n) (take first-list n)
        (let* ([second-list (extract-words (- n first-list-len)
                                           (string-upcase partial-word)
                                           word-len
                                           dictionary '() first-list)]
               [second-list-len (length second-list)])
          (if (>= second-list-len n) (append first-list second-list)
              (append first-list
                      second-list
                      (make-list (- n first-list-len second-list-len) "")))))))

(define (extract-words n word len word-list ans [existing-list '()])
  (cond [(or (null? word-list) (= n 0)) (reverse ans)]
        [(or (<= (string-length (car word-list)) len)
             ;(null? existing-list)
             (list? (member (string-downcase (car word-list)) existing-list)))
         (extract-words n word len (cdr word-list) ans)]
        [(equal? word (substring (car word-list) 0 len)) (extract-words (- n 1)
                                                                   word len
                                                                   (cdr word-list)
                                                                   (cons (string-downcase (car word-list)) ans))]
        [else (extract-words n word len (cdr word-list) ans)]))