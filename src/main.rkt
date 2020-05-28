#lang racket/gui
(provide (all-defined-out))
(require 2htdp/image)
(require 2htdp/batch-io)
(require 2htdp/universe)
(require racket/date)
(require file/sha1)
(require "frame-updates.rkt")
(require "image.rkt")
(require "universe-for-host.rkt")
(require "encryption.rkt")
(require "mancala.rkt")
(require "Word-Search/word-search.rkt")

(define ip-address "10.16.28.165")
(define user-remembered? #f)
(define ID-PASSWORD-PATH "data/id-password.txt")
(define id-password (cond [(file-exists? ID-PASSWORD-PATH)
                           (set! user-remembered? #t)
                           (string-split (read-file ID-PASSWORD-PATH))]
                          [else '()]))
(define user-id (if (null? id-password) ""
                    (car id-password)))
(define user-password (if (null? id-password) ""
                          (cadr id-password)))
(define login-username "")
(define login-password "")
(define USER-DETAILS (if (file-exists? "data/personal-information.txt")
                         (read-lines "data/personal-information.txt")
                         '("" "" "")))
(define CHANGE-USER-DETAILS? #f)
(define NEW-USER-DETAILS '())
(define USER-STATUS (if (file-exists? "data/status.txt")
                        (read-file "data/status.txt")
                        ""))
(define NEW-USER-STATUS "")
(define CHANGE-STATUS? #f)
(define original-profile-pic (if (file-exists? "data/user-profile-pic.png")
                             (bitmap/file "data/user-profile-pic.png")
                             default-profile-pic))
(define USER-PROFILE-PIC (resize-appropriately original-profile-pic 270))
(set-profile-pic USER-PROFILE-PIC)
(set-original-profile-pic original-profile-pic)
(set-user-details USER-DETAILS)
(set-user-status USER-STATUS)
(define CREATE-ONLINE-WORLD? #f)
(define REGISTER? #f)
(define REMEMBER-USER? #f)
(define LOGIN? #f)
(define SEND-MESSAGE? #f)
(define START-CHAT? #f)
(define START-MANCALA? #f)
(define TO-CHAT-WITH "")
(define TO-PLAY-WITH "")
(define VIEW-ACCOUNT? #f)
(define REQUESTED-USER-ACCOUNT (list "" "" "" ""))
(define REQUESTED-USER-PROFILE-PIC empty-image)
(define CHATTING? #f)
(define CHATTING-WITH "")
(define PLAYING-WITH "")
(define CHATTING-WITH-OFFLINE? #f)
(define TYPING? #f)
(define CURR-DATE "")
(define HOST-ONLINE-STATUS #f)
(define START-TICKS? #f)
(define TICKS 0)
(define REQUEST-LIST? #f)
(define Active-users '())
(define interacting-people-history '())
;;;;;;;;;;;;;;;SENDING-AND-RECIVEING-IMAGES;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define IMAGE-FRAME empty_frame)
(define SEND-PACKET? #f)
(define FILE-TO-SEND-PATH "")
(define SENT-FILE-SEXP #"")
(define SENT-FILENAME "")
(define ENCRYPTED-FILENAME "")
(define LIST-FILE-SEXP '())
(define RECEIVED-PACKETS '())
(define RECEIVED-FILE-SEXP #"")
(define RECEIVED-FILE-NAME "")
(define SAVE-PATH "data/chat-histories/")
(define CURR-SAVE-PATH SAVE-PATH)
(define PROCESSING? #f)
;;;;;;;;;;;;;;;;;;;;;;;ENCRYPTION;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define HASHED-PASSWORD (if (equal? user-password "") ""
                            (bytes->hex-string (sha256-bytes (open-input-string user-password)))))
(define OLD-PASSWORD "")
(define NEW-PASSWORD "")
(define CHANGE-PASSWORD? #f)
(define DEFAULT-ENCRYPTION-KEY '(#\k #\J #\H #\t #\r #\Q #\a #\m #\V #\g #\c #\O #\G #\o #\j #\Y #\i #\x #\y #\B #\p #\b #\I #\w #\f #\d #\M #\X #\E #\n #\C #\l #\N #\P #\F #\Z #\q #\D #\e #\A #\s #\T #\S #\u #\h #\L #\U #\R #\v #\z #\W #\K))
(define ENCRYPTION-KEY '())
;;;;;;;;;;;;;;;;;;;Vocabulary Learning;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (modify word-list ans)
  (cond [(null? word-list) ans]
        [(equal? (car word-list) "") (modify (cdr word-list) ans)] 
        [else (let ([occurences (length (filter (λ (x) (equal? x (car word-list))) word-list))])
                (modify (filter (λ (x) (not (equal? x (car word-list)))) (cdr word-list))
                        (cons (cons (string-downcase (car word-list)) occurences) ans)))]))

(define chat-history "")
(define WORD-HISTORY-PATH "data/used-word-history.txt")
(define used-word-history (convert-readable (read-words WORD-HISTORY-PATH) '()))
(define curr-history-path "")

(define (add-words f word-list)
  (cond [(null? used-word-history) (begin0 (f (sort word-list (λ (x y) (> (cdr x) (cdr y)))))
                                           (display-to-file (convert-writable used-word-history)
                                                            WORD-HISTORY-PATH
                                                            #:mode 'text
                                                            #:exists 'replace))]   
        [(null? word-list) (begin0 (f (sort used-word-history (λ (x y) (> (cdr x) (cdr y)))))
                                   (display-to-file (convert-writable used-word-history)
                                                    WORD-HISTORY-PATH
                                                    #:mode 'text
                                                    #:exists 'replace))]
        [else (let ([index (index-of used-word-history
                                     (car word-list)
                                     (λ (x y) (equal? (car x) (car y))))])
                (cond [(real? index) (f (list-update used-word-history
                                                     index
                                                     (λ (x) (cons (car x) (+ (cdar word-list) (cdr x))))))
                                     (add-words f (cdr word-list))]
                      [else (f (append used-word-history (list (car word-list))))
                            (add-words f (cdr word-list))]))]))

(define (string-filter-alphabetic str)
  (list->string (filter char-alphabetic? (string->list str))))

;;;;;;;;;;;;;;;;;;;;;;;;ONLINE-USERS-FRAME;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define current-user-name "")
(define filtered-user-list Active-users)
(define times-suggested 0)

(define (show-online-frame)
  (set! times-suggested 0)
  (define online-text
    (new text%
         [line-spacing 6]
         [auto-wrap #t]))
  (send online-text change-style delta)

  (define online-users-frame
    (new frame%
         [label "Active Users"]
         [width 700]
         [height 500]
         [alignment '(left top)]
         [stretchable-width #f]
         [stretchable-height #f]))

  (define search-text-field
    (new text-field%
         [parent online-users-frame]
         [label "Choose a user: "]
         [callback (λ (t e) (cond [(equal? (send e get-event-type) 'text-field)
                                   (send online-text lock #f)
                                   (send online-text move-position 'end)
                                   (set! current-user-name (send search-text-field get-value))
                                   (set! filtered-user-list (filter-users current-user-name Active-users))
                                   (send online-text erase)
                                   (send online-text change-style delta)
                                   (send online-text insert (convert-to-user-string filtered-user-list))
                                   (send online-editor-canvas set-editor online-text #t)
                                   (cond [(and (= (length filtered-user-list) 1) (< times-suggested 3))
                                          (set! times-suggested (+ times-suggested 1))
                                          (send search-text-field set-value (car filtered-user-list))])
                                   (send online-text lock #t)]
                                  [(equal? (send e get-event-type) 'text-field-enter)
                                   (send online-text lock #t)
                                   (let ([entered-name (send search-text-field get-value)])
                                     (cond [(member entered-name filtered-user-list)
                                            (set! TO-CHAT-WITH entered-name)
                                            (send online-users-frame show #f)
                                            (set! filtered-user-list Active-users)
                                            (set! START-CHAT? #t)]
                                           [else (message-box "User not found"
                                                              "The username you have entered is either\nincorrect or the user is not active"
                                                              #f
                                                              (list 'ok 'stop))]))]))]
                                               
         [horiz-margin 10]
         [vert-margin 10]
         [font our-font-2]))

  (define online-users-pane
    (new horizontal-pane%
         [parent online-users-frame]
         [vert-margin 10]
         [horiz-margin 10]
         [alignment '(left top)]))

  (define online-editor-canvas
    (new editor-canvas%
         [parent online-users-pane]
         [editor online-text]
         [min-width 550]
         [min-height 450]
         [horiz-margin 10]
         [vert-margin 2]))

  (define online-users-pane2
    (new vertical-pane%
         [parent online-users-pane]
         [vert-margin 10]
         [alignment '(center top)]))

  (define search-button
    (new button%
         [parent online-users-pane2]
         [label "Send Chat\nRequest"]
         [callback (λ (b e) (let ([entered-name (send search-text-field get-value)])
                              (cond [(member entered-name filtered-user-list)
                                     (set! TO-CHAT-WITH entered-name)
                                     (send online-users-frame show #f)
                                     (set! filtered-user-list Active-users)
                                     (set! START-CHAT? #t)]
                                    [else (message-box "User not found"
                                                       "The username you have entered is either\nincorrect or the user is not active"
                                                       #f
                                                       (list 'ok 'stop))])))]
         [min-width 80]
         [min-height 40]
         [font our-font-2]))

  (define view-account-button
    (new button%
         [parent online-users-pane2]
         [label "View Account"]
         [callback (λ (b e) (let ([entered-name (send search-text-field get-value)])
                              (cond [(member entered-name filtered-user-list)
                                     (set! TO-CHAT-WITH entered-name)
                                     (send online-users-frame show #f)
                                     (set! filtered-user-list Active-users)
                                     (set! VIEW-ACCOUNT? #t)]
                                    [else (message-box "User not found"
                                                       "The username you have entered is either\nincorrect or the user is not active"
                                                       #f
                                                       (list 'ok 'stop))])))]
         [min-width 85]
         [min-height 20]
         [font our-font-2]))

  (send online-text erase)
  (send online-text change-style delta)
  (send online-text insert (convert-to-user-string Active-users))
  (send online-text lock #t)
  (send online-users-frame show #t))

(define (online-users-mancala-frame)
  (define online-text
    (new text%
         [line-spacing 6]
         [auto-wrap #t]))
  (send online-text change-style delta)

  (define online-users-frame
    (new frame%
         [label "Active Users"]
         [width 700]
         [height 500]
         [alignment '(left top)]
         [stretchable-width #f]
         [stretchable-height #f]))

  (define search-text-field
    (new text-field%
         [parent online-users-frame]
         [label "Choose a user: "]
         [callback (λ (t e) (cond [(equal? (send e get-event-type) 'text-field)
                                   (send online-text lock #f)
                                   (send online-text move-position 'end)
                                   (set! current-user-name (send search-text-field get-value))
                                   (set! filtered-user-list (filter-users current-user-name Active-users))
                                   (send online-text erase)
                                   (send online-text change-style delta)
                                   (send online-text insert (convert-to-user-string filtered-user-list))
                                   (send online-editor-canvas set-editor online-text #t)
                                   (cond [(and (= (length filtered-user-list) 1) (< times-suggested 3))
                                          (set! times-suggested (+ times-suggested 1))
                                          (send search-text-field set-value (car filtered-user-list))])
                                   (send online-text lock #t)]
                                  [(equal? (send e get-event-type) 'text-field-enter)
                                   (send online-text lock #t)
                                   (let ([entered-name (send search-text-field get-value)])
                                     (cond [(member entered-name filtered-user-list)
                                            (set! TO-PLAY-WITH entered-name)
                                            (send online-users-frame show #f)
                                            (set! filtered-user-list Active-users)
                                            (set! START-MANCALA? #t)]
                                           [else (message-box "User not found"
                                                              "The username you have entered is either\nincorrect or the user is not active"
                                                              #f
                                                              (list 'ok 'stop))]))]))]
                                               
         [horiz-margin 10]
         [vert-margin 10]
         [font our-font-2]))

  (define online-users-pane
    (new horizontal-pane%
         [parent online-users-frame]
         [vert-margin 10]
         [horiz-margin 10]
         [alignment '(left top)]))

  (define online-editor-canvas
    (new editor-canvas%
         [parent online-users-pane]
         [editor online-text]
         [min-width 550]
         [min-height 450]
         [horiz-margin 10]
         [vert-margin 2]))

  (define search-button
    (new button%
         [parent online-users-pane]
         [label "Send Mancala\nRequest"]
         [callback (λ (b e) (let ([entered-name (send search-text-field get-value)])
                              (cond [(member entered-name filtered-user-list)
                                     (set! TO-PLAY-WITH entered-name)
                                     (send online-users-frame show #f)
                                     (set! filtered-user-list Active-users)
                                     (set! START-MANCALA? #t)]
                                    [else (message-box "User not found"
                                                       "The username you have entered is either\nincorrect or the user is not active"
                                                       #f
                                                       (list 'ok 'stop))])))]
         [min-width 80]
         [min-height 40]
         [font our-font-2]))

  (send online-text erase)
  (send online-text change-style delta)
  (send online-text insert (convert-to-user-string Active-users))
  (send online-text lock #t)
  (send online-users-frame show #t))

(define (filter-users curr-name active-users-list)
  (if (string=? curr-name "") active-users-list
      (filter (λ (x) (string-prefix? (string-downcase x)
                                     (string-downcase curr-name))) active-users-list)))

(define (convert-to-user-string l)
  (foldr (λ (x y) (string-append x "\n" y)) "" l))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;GUI;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define soft-frame (new frame%
                        [label "Querencia"]
                        [width 1500]
                        [height 730]
                        [style (list 'fullscreen-button 'fullscreen-aux)]
                        [alignment '(center center)]))

(define text-obj (new text%
                      [line-spacing 6]
                      [auto-wrap #t]))
(send text-obj change-style delta)

(define text-edit
  (new editor-canvas%
       [parent soft-frame]
       [editor text-obj]
       [label "text-editor"]))

(define msg
  (new message%
       [label "                        Ready for action                        "]
       [parent soft-frame]
       ))

(define curr-completed-message "")
(define curr-encrypted-message "")
(define curr-message "")
(define curr-subs-list '("" "" ""))
(define show-val #f)

(begin
  (define sub1
    (new button%
         [label ""]
         [parent soft-frame]
         [callback (lambda (b e)
                     (send t-field set-value (string-append curr-message (car curr-subs-list)))
                     (set! show-val #f))]
         [font text-field-font]
         [min-width 250]
         [stretchable-width #f]))
  
  (define sub2
    (new button%
         [label ""]
         [parent soft-frame]
         [callback (lambda (b e)
                     (send t-field set-value (string-append curr-message (cadr curr-subs-list)))
                     (set! show-val #f))]
         [font text-field-font]
         [min-width 250]
         [stretchable-width #f]))

  (define sub3
    (new button%
         [label ""]
         [parent soft-frame]
         [callback (lambda (b e)
                     (send t-field set-value (string-append curr-message (last curr-subs-list)))
                     (set! show-val #f))]
         [font text-field-font]
         [min-width 250]
         [stretchable-width #f]))

  (send sub1 show #f)
  (send sub2 show #f)
  (send sub3 show #f))

(define (myround n m)
  (/ (round (* n (expt 10 m))) (expt 10 m)))
(define initial-sending-range 1)
(define sending-frame
  (new frame%
       [label "Sending-file"]
       [width 300]
       [height 50]))
(define sending-msg
  (new message%
       [label "Sending file..."]
       [parent sending-frame]
       [vert-margin 5]
       [font our-font-2]))
(define sending-gauge
  (new gauge%
       [label "0.00%  "]
       [range initial-sending-range]
       [parent sending-frame]
       [vert-margin 5]))
(send sending-gauge set-value 0)

(define initial-receiving-range 1)
(define receiving-frame
  (new frame%
       [label "Receiving-file"]
       [width 300]
       [height 50]))
(define receiving-msg
  (new message%
       [label "Receiving file..."]
       [parent receiving-frame]
       [vert-margin 5]
       [font our-font-2]))
(define receiving-gauge
  (new gauge%
       [label "0.00%  "]
       [range initial-receiving-range]
       [parent receiving-frame]
       [vert-margin 5]))
(send receiving-gauge set-value 0)
                     
(define t-field (new text-field%
                     [parent soft-frame]
                     [label "Text"]
                     [callback (lambda (t e) (let* ([message (send t-field get-value)]
                                                    [split-msg (string-split message)])
                                               (cond [(equal? (send e get-event-type) 'text-field-enter)
                                                      (let* ([curr-date (date->string (current-date) #t)]
                                                             [to-send (string-append user-id "--->" curr-date "  :  " message "\n")])
                                                        (send text-obj lock #f)
                                                        (send text-obj move-position 'end)
                                                        (send sub1 show #f)
                                                        (send sub2 show #f)
                                                        (send sub3 show #f)
                                                        (send msg set-label "   message sent! :)")
                                                        (set! TYPING? #f)
                                                        (send t-field set-value "")
                                                        (send text-obj insert to-send)
                                                        (send text-obj lock #t)
                                                        (set! curr-completed-message message)
                                                        (set! curr-encrypted-message (encrypt ENCRYPTION-KEY message))
                                                        (set! chat-history (string-append chat-history "\n" to-send))
                                                        (cond [(not (directory-exists? (string-append "data/chat-histories/" user-id "-" CHATTING-WITH "/")))
                                                               (make-directory* (string-append "data/chat-histories/" user-id "-" CHATTING-WITH "/"))])
                                                        (display-to-file to-send curr-history-path #:mode 'text #:exists 'append)
                                                        (add-words (λ (y) (set! used-word-history y))
                                                                   (modify (map (λ (x) (string-filter-alphabetic x)) split-msg) '()))
                                                        (set! CURR-DATE curr-date)
                                                        (set! SEND-MESSAGE? #t)
                                                        (send text-edit set-editor text-obj #t))]

                                                     [(equal? (send e get-event-type) 'text-field)
                                                      (send text-obj lock #t)
                                                      (if (equal? message "")
                                                          (set! TYPING? #f)
                                                          (if (equal? (last (string->list message)) #\space)
                                                              (begin (set! TYPING? #f)
                                                                     (set! curr-message (string-append message " "))
                                                                     (send sub1 show #f)
                                                                     (send sub2 show #f)
                                                                     (send sub3 show #f))
                                                              (let* ([current-word (string-filter-alphabetic (string-downcase (last split-msg)))]
                                                                     [true-msg (string-append (string-join (drop-right split-msg 1)) " ")]
                                                                     [poss-subs (find-possible-subs 3 current-word used-word-history)]
                                                                     [subs (map (λ (x) (if (label-string? x) x "")) poss-subs)])
                                                                (set! curr-message true-msg)
                                                                (set! TYPING? #t)
                                                                (set! curr-subs-list poss-subs)
                                                                (send sub1 set-label (car subs))
                                                                (send sub2 set-label (cadr subs))
                                                                (send sub3 set-label (last subs))
                                                                (send sub1 show #t)
                                                                (send sub2 show #t)
                                                                (send sub3 show #t))))])))]
                     [font text-field-font]))

(define buttons
  (begin
    (new button%
         [parent soft-frame]
         [label send-pic]
         [callback (lambda (button event)
                     (let* ([message (send t-field get-value)]
                            [split-msg (string-split message)]
                            [curr-date (date->string (current-date) #t)]
                            [to-send (string-append user-id "--->" curr-date "  :  " message "\n")])
                       (send text-obj lock #f)
                       (send sub1 show #f)
                       (send sub2 show #f)
                       (send sub3 show #f)
                       (set! TYPING? #f)
                       (send msg set-label "                 message sent! :)")
                       (send t-field set-value "")
                       (send text-obj insert to-send)
                       (send text-obj lock #t)
                       (set! curr-completed-message message)
                       (set! curr-encrypted-message (encrypt ENCRYPTION-KEY message))
                       (set! chat-history (string-append chat-history "\n" to-send))
                       (display-to-file to-send curr-history-path #:mode 'text #:exists 'append)
                       (add-words (λ (y) (set! used-word-history y))
                                  (modify (map (λ (x) (string-filter-alphabetic x)) split-msg) '()))
                       (set! CURR-DATE curr-date)
                       (set! SEND-MESSAGE? #t)
                       (send text-edit set-editor text-obj #t)))])
    (new button%
         [parent soft-frame]
         [label "Choose a file"]
         [callback (λ (t e)
                     (let ([file-path (get-file "Choose a file")])
                       (cond [(not (equal? file-path #f))
                              (let ([byte-form (file->bytes file-path)])
                                (send msg set-label "                             Sending-file...")
                                (set! FILE-TO-SEND-PATH file-path)
                                (set! SENT-FILENAME (path->string (file-name-from-path file-path)))
                                (set! ENCRYPTED-FILENAME (encrypt ENCRYPTION-KEY SENT-FILENAME))
                                (set! SENT-FILE-SEXP byte-form)
                                (set! LIST-FILE-SEXP (split-data byte-form))
                                (set! initial-sending-range (length LIST-FILE-SEXP))
                                (send sending-gauge set-range initial-sending-range)
                                (send sending-gauge set-value 0)
                                (send sending-gauge set-label "0%")
                                (send sending-frame show #t)
                                (set! SEND-PACKET? #t))])))])
                                

    (new button%
         [parent soft-frame]
         [label "Clear history"]
         [callback (λ (t e)
                     (send text-obj lock #f)
                     (send text-obj erase)
                     (send text-obj change-style delta)
                     (send text-obj lock #t)
                     (set! chat-history "")
                     (send msg set-label "History cleared!")
                     (send text-edit set-editor text-obj #t)
                     (cond [(directory-exists? (string-append "data/chat-histories/" user-id "-" CHATTING-WITH "/"))
                            (delete-directory/files (string-append "data/chat-histories/" user-id "-" CHATTING-WITH "/"))]))])

    (new button%
         [parent soft-frame]
         [label exit]
         [callback (lambda (t e) (send soft-frame show #f))])))

;;;;;;;;;;;;;;;;;;;;;;;;;WORLD BUSINESS;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (register-user)
  (begin
    (define frame
      (new frame%
           [label "Register User"]
           [width 600]
           [height 250]
           [stretchable-width #t]
           [stretchable-height #t]))

    (define msg1
      (new message%
           [label "Enter your username and password,\nMake sure to remember it!"]
           [parent frame]
           [font our-font-1]))
           
    (define username-text
      (new text-field%
           [label "Username :"]
           [parent frame]
           [callback (λ (t e) (cond [(string-contains? (send username-text get-value) " ")
                                     (send error-message
                                           set-label
                                           "Username or password should not contain any spaces")]
                                    [else (send error-message set-label "")]))]
           [font our-font-2]))

    (define password-strength 
      (new message%
           [label "                                                                               "]
           [parent frame]
           [font our-font-2]))

    (define password-text
      (new text-field%
           [label "Password : "]
           [parent frame]
           [callback (lambda (t e) (cond [(equal? (send e get-event-type) 'text-field)
                                          (let* ([in-text (send password-text get-value)]
                                                 [len (string-length in-text)])
                                            (cond [(not (string-contains? in-text " "))
                                                   (send error-message set-label "")
                                                   (cond [(> len 16) (send password-strength set-label "very-strong")]
                                                         [(> len 11) (send password-strength set-label "strong")]
                                                         [(> len 8) (send password-strength set-label "moderate")]
                                                         [(> len 5) (send password-strength set-label "weak")]
                                                         [else (send password-strength set-label "Password must be more than 5 characters long")])]
                                                  [else (cond [(string=? (send error-message get-label) "")
                                                               (send password-strength set-label "")
                                                               (send error-message set-label "Username or password should not contain any spaces")])]))]))]
           [style (list 'single 'password)]
           [font our-font-2]))

    (define remember-me
      (new check-box%
           [label "Remember-me"]
           [parent frame]
           [callback (λ (c e) (let ([val (send remember-me get-value)])
                                (cond [val (set! REMEMBER-USER? #t)
                                           (set! user-remembered? #t)]
                                      [else (set! REMEMBER-USER? #t)
                                            (set! user-remembered? #t)])))]
           [vert-margin 5]))

    (define profile-pic-button
      (new button%
           [label "Choose profile picture"]
           [parent frame]
           [callback (λ (b e) (let* ([profile-pic-path (get-file "Choose profile pic"
                                                                #f #f #f #f null (list (list "Any" "*.jp*g;*.png")))]
                                     [ori-pic (bitmap/file profile-pic-path)]
                                     [profile-pic (resize-appropriately ori-pic 270)])
                                (set! USER-PROFILE-PIC profile-pic)
                                (save-image ori-pic "data/user-profile-pic.png")
                                (set-profile-pic profile-pic)
                                (set-original-profile-pic ori-pic)))]
           [vert-margin 10]
           [min-width 100]
           [font our-font-2])) 

    (define error-message
      (new message%
           [label "                                            "]
           [parent frame]
           [font our-font-1]))

    (define user-name-field
      (new text-field%
           [label "Enter your Name: "]
           [parent frame]
           [font our-font-2]
           [vert-margin 5]))

    (define nationality-field
      (new text-field%
           [label "Enter your nationality"]
           [parent frame]
           [font our-font-2]
           [vert-margin 5]))

    (define age-field
      (new text-field%
           [label "Enter your age: "]
           [parent frame]
           [font our-font-2]
           [vert-margin 5]))

    (define status-field
      (new text-field%
           [label "Choose Status: "]
           [parent frame]
           [init-value "Hey there! I'm in Querencia"]
           [font our-font-2]
           [vert-margin 5]))

    (new button%
         [label "Register"]
         [parent frame]
         [callback (lambda (b e) (let ([username (send username-text get-value)]
                                       [password (send password-text get-value)]
                                       [name (send user-name-field get-value)]
                                       [nationality (send nationality-field get-value)]
                                       [age (send age-field get-value)]
                                       [status (send status-field get-value)])
                                   (cond [(<= (string-length password) 5) (send password-text set-value "")
                                                                          (send error-message set-label "Password too short")]
                                         [(equal? username "") (send username-text set-value "")
                                                               (send password-strength set-value "")
                                                               (send error-message set-label "Username should not be null")]
                                         [(or (string=? name "")
                                              (string=? nationality "")
                                              (string=? age "")
                                              (not (number? (string->number age)))) (send error-message set-label "Please enter your details correctly")]
                                         [(or (string-contains? username " ")
                                              (string-contains? password " "))
                                          (send username-text set-value "")
                                          (send password-text set-value "")
                                          (send password-strength set-label "")
                                          (send error-message set-label "Username or password should not contain any spaces")]                                                       
                                         [else (set! user-id username)
                                               (set! user-password password)
                                               (let ([hashing-result (bytes->hex-string (sha256-bytes (open-input-string password)))])
                                                 (set! HASHED-PASSWORD hashing-result)
                                                 (set! USER-DETAILS (list name nationality age))
                                                 (set-user-details USER-DETAILS)
                                                 (set! USER-STATUS status)
                                                 (set-user-status USER-STATUS)
                                                 (set! REGISTER? #t)
                                                 (send frame show #f))])))]
         [font our-font-2])
    (send frame show #t)))

(define (do-login)
  (begin
    (define frame
      (new frame%
           [label "Login"]
           [width 600]
           [height 100]
           [stretchable-width #t]
           [stretchable-height #t]))

    (define username-text
      (new text-field%
           [label "Username :"]
           [parent frame]
           [init-value (if user-remembered? user-id "")]
           [font our-font-2]))

    (define error-message
      (new message%
           [label "                                              "]
           [parent frame]
           [font our-font-1]))

    (define password-text
      (new text-field%
           [label "Password :"]
           [parent frame]
           [callback (lambda (t e) (cond [(equal? (send e get-event-type) 'text-field-enter)
                                          (let* ([username (send username-text get-value)]
                                                 [password (send password-text get-value)]
                                                 [hashing-result (bytes->hex-string (sha256-bytes (open-input-string password)))])
                                            (set! login-username username)
                                            (set! login-password password)
                                            (set! HASHED-PASSWORD hashing-result)
                                            (set! LOGIN? #t)
                                            (send frame show #f))]))]
           [init-value (if user-remembered? user-password "")]
           [style (list 'single 'password)]
           [font our-font-2]))

    (define remember-me
      (new check-box%
           [label "Remember-me"]
           [parent frame]
           [callback (λ (c e) (let ([val (send remember-me get-value)])
                                (cond [val (set! REMEMBER-USER? #t)
                                           (set! user-remembered? #t)]
                                      [else (set! REMEMBER-USER? #f)
                                            (set! user-remembered? #f)])))]
           [value user-remembered?]
           [vert-margin 5]))

    (new button%
         [label "Login"]
         [parent frame]
         [callback (lambda (b e)
                     (let* ([username (send username-text get-value)]
                            [password (send password-text get-value)]
                            [hashing-result (bytes->hex-string (sha256-bytes (open-input-string password)))])
                       (set! login-username username)
                       (set! login-password password)
                       (set! HASHED-PASSWORD hashing-result)
                       (set! LOGIN? #t)
                       (send frame show #f)))]
         [font our-font-2])
    (send frame show #t)))

(define MADE-ONLINE? #f)
(define GONE-OFFLINE? #f)

(define (received-file-frame)
  (define fr
    (new frame%
       [label "File received"]
       [width 400]
       [height 50]))
  (define msg
    (new message%
         [parent fr]
         [label (string-append CHATTING-WITH " sent you a file.\n"
                               "File Name : "
                               RECEIVED-FILE-NAME "\n"
                               "Do you wish to open it?")]
         [font our-font-2]))
  (new button%
       [parent fr]
       [label "Yes"]
       [callback (λ (b e) (shell-execute "open"
                                         (string-append (path->string (current-directory))
                                                        "data\\chat-histories\\"
                                                        user-id "-"
                                                        CHATTING-WITH "\\"
                                                        "received -"
                                                         (begin (date-display-format 'iso-8601)
                                                                (date->string (current-date)))
                                                         " -"
                                                         CHATTING-WITH
                                                         " -"
                                                         RECEIVED-FILE-NAME)
                                         ""
                                         (current-directory)
                                         'sw_shownormal)
                   (date-display-format 'american)
                   (send fr show #f))]
       [vert-margin 5]
       [font our-font-2])
  (new button%
       [parent fr]
       [label "No"]
       [callback (λ (b e) (send fr show #f))]
       [vert-margin 5]
       [font our-font-2])
  (send fr show #t))

(define (data-tempered-frame)
  (define fr
    (new frame%
       [label "DATA TAMPERED WITH!"]
       [width 200]
       [height 100]))
  (define msg
    (new message%
         [parent fr]
         [label "WARNING! : The data sent to you has been modified by some authority\nPlease look into the matter"]
         [vert-margin 10]
         [horiz-margin 10]
         [font our-font-2]))
  (new button%
       [parent fr]
       [label "Ok"]
       [callback (λ (b e) (send fr show #f))]
       [font our-font-2])
  (send fr show #t))
         

(define (receive-msg-handler state sexp)
  (cond [(equal? sexp "Username already in use") (set! HOST-ONLINE-STATUS #t)
                                                 (message-box "Registeration-failed"
                                                              sexp
                                                              #f
                                                              (list 'ok 'stop))
                                                 (register-user)
                                                 "WELCOME"]
        
        [(and (list? sexp) (equal? (car sexp) "You have been successfully registered"))
         (set! HOST-ONLINE-STATUS #t)
         (display-to-file USER-STATUS "data/status.txt" #:mode 'text #:exists 'replace)
         (display-to-file (string-append (first USER-DETAILS)
                                         "\n"
                                         (second USER-DETAILS)
                                         "\n"
                                         (third USER-DETAILS)) "data/personal-information.txt" #:mode 'text #:exists 'replace)
         (cond [REMEMBER-USER?
                (display-to-file (string-append user-id
                                                "\n"
                                                user-password)
                                 "data/id-password.txt"
                                 #:mode 'text
                                 #:exists 'replace)
                (message-box "Successful registration"
                             (car sexp))
                "WELCOME"]
               [else (message-box "Successful registration"
                                  (car sexp))
                     "WELCOME"])]
        
        [(equal? sexp "You have logged-in successfully")
         (set! HOST-ONLINE-STATUS #t)
         (message-box "Successful login"
                      sexp)
         (set! user-id login-username)
         (set! user-password login-password)
         (cond [(and (not (file-exists? "data/id-password.txt"))
                     REMEMBER-USER?) (display-to-file (string-append user-id
                                                                     "\n"
                                                                     user-password)
                                                      "data/id-password.txt"
                                                      #:mode 'text
                                                      #:exists 'replace)])
         (set! MADE-ONLINE? #t)
         "GO-ONLINE"]
        
        [(equal? sexp "Invalid Credentials") (set! HOST-ONLINE-STATUS #t)
                                             (message-box "Login failed"
                                                          sexp
                                                          #f
                                                          (list 'ok 'stop))
                                             (set! login-username "")
                                             (set! login-password "")
                                             (do-login)
                                             "LOGIN"]

        [(equal? sexp "Password Successfully changed")
         (set! user-password NEW-PASSWORD)
         (set! HASHED-PASSWORD (bytes->hex-string (sha256-bytes (open-input-string user-password))))
         (display-to-file (string-append user-id "\n" user-password) "data/id-password.txt" #:mode 'text #:exists 'replace)
         (message-box "Successful"
                      "Your password has been changed successfully"
                      #f
                      (list 'ok))
         (set! OLD-PASSWORD "")
         (set! NEW-PASSWORD "")
         state]

        [(equal? sexp "Status Successfully changed")
         (set! USER-STATUS NEW-USER-STATUS)
         (set-user-status USER-STATUS)
         (display-to-file USER-STATUS "data/status.txt" #:mode 'text #:exists 'replace)
         (message-box "Successful"
                      "Your status has been successfully changed"
                      #f
                      (list 'ok))
         (set! NEW-USER-STATUS "")
         state]

        [(equal? sexp "Details Successfully changed")
         (set! USER-DETAILS NEW-USER-DETAILS)
         (set-user-details USER-DETAILS)
         (display-to-file (string-append (car USER-DETAILS)
                                         "\n"
                                         (second USER-DETAILS)
                                         "\n"
                                         (third USER-DETAILS)) "data/personal-information.txt" #:mode 'text #:exists 'replace)
         (message-box "Successful"
                      "Your Details have been changed successfully")
         (set! NEW-USER-DETAILS "")
         state]

        [(equal? sexp "Incorrect oldpass")
         (message-box "Failed"
                      "Invalid Credentials"
                      #f
                      (list 'ok 'stop))
         (change-password)
         state]

        [(equal? sexp "You are going offline") (set! GONE-OFFLINE? #t)
                                               "GOING-OFFLINE"]

        [(and (list? sexp) (equal? (car sexp) "give-your-profile-pic"))
         (make-package state
                       (list "SEND-PROFILE-PIC"
                             (file->bytes "data/user-profile-pic.png")
                             (cadr sexp)))]
        
        [(and (list? sexp) (equal? (car sexp) "User not active")) (begin
                                                                    (set! TO-CHAT-WITH "")
                                                                    (message-box "Cannot Connect"
                                                                                 (string-append (cadr sexp) " : " (car sexp)))
                                                                    state)]

        [(and (list? sexp) (equal? (car sexp) "Chat request sent")) (begin
                                                                      (set! TO-CHAT-WITH "")
                                                                      (message-box "Request Sent"
                                                                                   (string-append (cadr sexp) " : " (car sexp)))
                                                                      state)]
        
        [(and (list? sexp) (equal? (car sexp) "ANOTHER-USER-REQUESTING-CHAT"))
         (let ([user-decision-chat-box-result (message-box/custom (car sexp)
                                                                  (string-append (cadr sexp) " has requested to start a chat with you"
                                                                                 "\nIf you choose to Accept any ongoing chat\n"
                                                                                 "will be closed and this chat will begin")
                                                                  "Accept"
                                                                  "Reject"
                                                                  #f)])
           (cond [(= user-decision-chat-box-result 1)
                  (let* ([history-path (string-append "data/chat-histories/"
                                                      user-id
                                                      "-"
                                                      (cadr sexp) "/")]
                         [text-history-path (string-append history-path
                                                           user-id
                                                           "-"
                                                           (cadr sexp) ".txt")])
                    (cond [(directory-exists? history-path)
                           (cond [(file-exists? text-history-path) (cond [CHATTING?
                                                                          (set! curr-history-path text-history-path)
                                                                          (set! SAVE-PATH history-path)
                                                                          (set! chat-history (string-append (read-file text-history-path) "\n"))
                                                                          (send text-obj lock #f)
                                                                          (send text-obj erase)
                                                                          (send text-obj change-style delta)
                                                                          (send text-obj insert chat-history)
                                                                          (send text-edit set-editor text-obj #t)
                                                                          (send text-obj lock #t)
                                                                          (send soft-frame show #t)
                                                                          (begin0
                                                                            (make-package state (list "CHAT-REQUEST-APPROVED-BY-RECEIVER-WHILE-CHATTING"
                                                                                                      (cadr sexp)
                                                                                                      CHATTING-WITH))
                                                                            (set! CHATTING-WITH (cadr sexp)))]
                                                                         [else (set! CHATTING? #t)
                                                                               (set! curr-history-path text-history-path)
                                                                               (set! SAVE-PATH history-path)
                                                                               (set! chat-history (string-append (read-file text-history-path) "\n"))
                                                                               (send text-obj lock #f)
                                                                               (send text-obj erase)
                                                                               (send text-obj change-style delta)
                                                                               (send text-obj insert chat-history)
                                                                               (send text-edit set-editor text-obj #t)
                                                                               (send text-obj lock #t)
                                                                               (send soft-frame show #t)
                                                                               (begin0
                                                                                 (make-package state (list "CHAT-REQUEST-APPROVED-BY-RECEIVER"
                                                                                                           (cadr sexp)))
                                                                                 (set! CHATTING-WITH (cadr sexp)))])]
                                 [else (cond [CHATTING?
                                              (set! curr-history-path text-history-path)
                                              (set! SAVE-PATH history-path)
                                              (set! chat-history "")
                                              (send text-obj lock #f)
                                              (send text-obj erase)
                                              (send text-obj change-style delta)
                                              (send text-edit set-editor text-obj #t)
                                              (send text-obj lock #t)
                                              (send soft-frame show #t)
                                              (begin0
                                                (make-package state (list "CHAT-REQUEST-APPROVED-BY-RECEIVER-WHILE-CHATTING"
                                                                          (cadr sexp)
                                                                          CHATTING-WITH))
                                                (set! CHATTING-WITH (cadr sexp)))]
                                             [else (set! CHATTING? #t)
                                                   (set! curr-history-path text-history-path)
                                                   (set! SAVE-PATH history-path)
                                                   (set! chat-history "")
                                                   (send text-obj lock #f)
                                                   (send text-obj erase)
                                                   (send text-obj change-style delta)
                                                   (send text-edit set-editor text-obj #t)
                                                   (send text-obj lock #t)
                                                   (send soft-frame show #t)
                                                   (begin0
                                                     (make-package state (list "CHAT-REQUEST-APPROVED-BY-RECEIVER"
                                                                               (cadr sexp)))
                                                     (set! CHATTING-WITH (cadr sexp)))])])]
                          [else (make-directory* history-path)
                                (cond [CHATTING?
                                       (set! curr-history-path text-history-path)
                                       (set! SAVE-PATH history-path)
                                       (set! chat-history "")
                                       (send text-obj lock #f)
                                       (send text-obj erase)
                                       (send text-obj change-style delta)
                                       (send text-edit set-editor text-obj #t)
                                       (send text-obj lock #t)
                                       (send soft-frame show #t)
                                       (begin0
                                         (make-package state (list "CHAT-REQUEST-APPROVED-BY-RECEIVER-WHILE-CHATTING"
                                                                   (cadr sexp)
                                                                   CHATTING-WITH))
                                         (set! CHATTING-WITH (cadr sexp)))]
                                      [else (set! CHATTING? #t)
                                            (set! curr-history-path text-history-path)
                                            (set! SAVE-PATH history-path)
                                            (set! chat-history "")
                                            (send text-obj lock #f)
                                            (send text-obj erase)
                                            (send text-obj change-style delta)
                                            (send text-edit set-editor text-obj #t)
                                            (send text-obj lock #t)
                                            (send soft-frame show #t)
                                            (begin0
                                              (make-package state (list "CHAT-REQUEST-APPROVED-BY-RECEIVER"
                                                                        (cadr sexp)))
                                              (set! CHATTING-WITH (cadr sexp)))])]))]
                                 
                 [else (let ([reason (get-text-from-user "Reason"
                                                         "Any Reason?"
                                                         #f
                                                         (if CHATTING? "I am chatting with someone else" ""))])
                         (make-package state (list "CHAT-REQUEST-REJECTED-BY-RECEIVER"
                                                   reason
                                                   (cadr sexp))))]))]

        [(and (list? sexp) (equal? (car sexp) "CHAT-REQUEST-ACCEPTED"))
         (set! CHATTING? #t)
         (set! CHATTING-WITH (cadr sexp))
         (set! ENCRYPTION-KEY (last sexp))
         (let* ([history-path (string-append "data/chat-histories/"
                                             user-id
                                             "-"
                                             (cadr sexp) "/")]
                [text-history-path (string-append history-path
                                                  user-id
                                                  "-"
                                                  (cadr sexp) ".txt")])
           (cond [(directory-exists? history-path)
                  (cond [(file-exists? text-history-path) (set! chat-history (string-append (read-file text-history-path) "\n"))
                                                          (set! SAVE-PATH history-path)
                                                          (set! curr-history-path text-history-path)
                                                          (send text-obj lock #f)
                                                          (send text-obj erase)
                                                          (send text-obj change-style delta)
                                                          (send text-obj insert chat-history)
                                                          (send text-edit set-editor text-obj #t)
                                                          (send text-obj lock #t)
                                                          (message-box (car sexp)
                                                                       (string-append (cadr sexp) " has approved you chat-request"))
                                                          (send soft-frame show #t)
                                                          state]
                        [else (set! chat-history "")
                              (set! SAVE-PATH history-path)
                              (set! curr-history-path text-history-path)
                              (send text-obj lock #f)
                              (send text-obj erase)
                              (send text-obj change-style delta)
                              (send text-edit set-editor text-obj #t)
                              (send text-obj lock #t)
                              (message-box (car sexp)
                                           (string-append (cadr sexp) " has approved you chat-request"))
                              (send soft-frame show #t)
                              state])]
                 [else (make-directory* history-path)
                       (set! CURR-SAVE-PATH history-path)
                       (set! chat-history "")
                       (set! curr-history-path text-history-path)
                       (send text-obj lock #f)
                       (send text-obj erase)
                       (send text-obj change-style delta)
                       (send text-edit set-editor text-obj #t)
                       (send text-obj lock #t)
                       (message-box (car sexp)
                                    (string-append (cadr sexp) " has approved you chat-request"))
                       (send soft-frame show #t)
                       state]))]

        [(and (list? sexp) (equal? (car sexp) "CHAT-REQUEST-REJECTED"))
         (message-box (car sexp)
                      (string-append (last sexp) " has rejected your chat-request"
                                     "\nReason :" (cadr sexp))
                      #f
                      (list 'ok 'stop))
         (set! CHATTING? #f)
         (set! CHATTING-WITH "")
         state]
        
        [(and (list? sexp) (equal? (car sexp) "OTHER-USER-DISCONNECTED"))
         (message-box (car sexp)
                      (string-append (cadr sexp) " has disconnected from this chat")
                      #f
                      (list 'ok 'stop))
         (send soft-frame show #f)
         (set! CHATTING? #f)
         (set! CHATTING-WITH "")
         state]

        [(and (list? sexp) (equal? (car sexp) "RECEIVE-MESSAGE"))
         (let* ([decrypted-received-message (decrypt ENCRYPTION-KEY (cadr sexp))]
                [actual-message (string-append (last sexp) "--->" (third sexp) "  :  " decrypted-received-message "\n")])
           (send text-obj lock #f)
           (send text-obj insert actual-message)
           (send msg set-label "")
           (send text-edit set-editor text-obj #t)
           (send text-obj lock #t)
           (set! chat-history (string-append chat-history actual-message))
           (cond [(not (directory-exists? (string-append "data/chat-histories/" user-id "-" CHATTING-WITH "/")))
                  (make-directory* (string-append "data/chat-histories/" user-id "-" CHATTING-WITH "/"))])
           (display-to-file actual-message curr-history-path #:mode 'text #:exists 'append)
           (add-words (λ (y) (set! used-word-history y))
                      (modify (map (λ (x) (string-filter-alphabetic x)) (string-split decrypted-received-message)) '()))
           state)]

        [(and (list? sexp) (equal? (car sexp) "YOU-APPROVED-A-CHAT-REQUEST"))
         (set! CHATTING? #t)
         (set! ENCRYPTION-KEY (cadr sexp))
         state]

        [(and (list? sexp) (equal? (car sexp) "RECEIVE-ACTIVE-USERS-LIST"))
         (set! Active-users (sort (cadr sexp) string<?))
         (set! filtered-user-list Active-users)
         (show-online-frame)
         state]

        [(and (list? sexp) (equal? (car sexp) "RECEIVE-ACCOUNT"))
         (set! REQUESTED-USER-ACCOUNT (map (λ (x)
                                             (decrypt DEFAULT-ENCRYPTION-KEY x))
                                           (cdr sexp)))
         state]

        [(and (list? sexp) (equal? (car sexp) "RECEIVE-USER-PROFILE-PIC"))
         (cond [(not (directory-exists? "data/profile-pics/")) (make-directory* "data/profile-pics/")])
         (display-to-file (cadr sexp) (string-append "data/profile-pics/" TO-CHAT-WITH ".png") #:exists 'replace)
         (set! REQUESTED-USER-PROFILE-PIC (bitmap/file (string-append "data/profile-pics/" TO-CHAT-WITH ".png")))
         (set-requested-profile-pic REQUESTED-USER-PROFILE-PIC)
         (set-requested-details REQUESTED-USER-ACCOUNT)
         "USER-ACCOUNT-FRAME"]

        [(and (list? sexp) (equal? (car sexp) "PACKET-SENT"))
         (cond [(not (null? LIST-FILE-SEXP)) 
                (send sending-gauge set-value (- initial-sending-range (length LIST-FILE-SEXP)))
                (send sending-gauge set-label (string-append (~a (myround (* (/ (send sending-gauge get-value)
                                                                             initial-sending-range) 100.0) 2)) "%"))
                (set! SEND-PACKET? #t)
                state]
               [else (send msg set-label "                         File Sent!")
                     (send sending-gauge set-value initial-sending-range)
                     (send sending-gauge set-label "100%")
                     (send sending-frame show #f)
                     (let ([to-send (string-append user-id
                                                   "--->"
                                                   (date->string (current-date) #t)
                                                   "  :  "
                                                   SENT-FILENAME
                                                   " - "
                                                   "File sent to "
                                                   CHATTING-WITH
                                                   "\n")])
                       (send text-obj lock #f)
                       (send text-obj insert to-send)
                       (set! chat-history (string-append chat-history "\n" to-send))
                       (cond [(not (directory-exists? (string-append "data/chat-histories/" user-id "-" CHATTING-WITH "/")))
                              (make-directory* (string-append "data/chat-histories/" user-id "-" CHATTING-WITH "/"))])
                       (display-to-file to-send curr-history-path #:mode 'text #:exists 'append)
                       (send text-edit set-editor text-obj #t)
                       (send text-obj lock #t))
                     (set! CURR-SAVE-PATH (string-append SAVE-PATH
                                                         "sent -"
                                                         (begin (date-display-format 'iso-8601)
                                                                (date->string (current-date)))
                                                         " -"
                                                         CHATTING-WITH
                                                         " -"
                                                         SENT-FILENAME
                                                         ))
                     (date-display-format 'american)
                     (cond [(file-exists? CURR-SAVE-PATH) (delete-directory/files CURR-SAVE-PATH)])
                     (copy-directory/files FILE-TO-SEND-PATH CURR-SAVE-PATH)
                     state])]

        [(and (list? sexp) (equal? (car sexp) "RECEIVE-PACKET"))
         (cond [(boolean? (fourth sexp)) (set! RECEIVED-PACKETS (cons (last sexp) RECEIVED-PACKETS))
                                         (set! initial-receiving-range (fifth sexp))
                                         (send receiving-gauge set-range initial-receiving-range)
                                         (send receiving-gauge set-value (length RECEIVED-PACKETS))
                                         (send receiving-gauge set-label (string-append (~a (myround (* (/ (length RECEIVED-PACKETS)
                                                                                                        initial-receiving-range) 100.0) 2)) "%"))
                                         (send receiving-frame show #t)
                                         (send msg set-label "                              Receiving file...")
                                         state]
               [else (set! PROCESSING? #t)
                     (send msg set-label "                     Receiving file...")
                     (set! RECEIVED-PACKETS (cons (last sexp) RECEIVED-PACKETS))
                     (send receiving-gauge set-value (length RECEIVED-PACKETS))
                     (send receiving-gauge set-label "100%")
                     (send receiving-msg set-label "Integrating...")
                     (set! RECEIVED-FILE-NAME (decrypt ENCRYPTION-KEY (third sexp)))
                     (set! RECEIVED-FILE-SEXP (append-to-sexp RECEIVED-PACKETS #""))
                     (cond [(not (equal? (bytes->hex-string (sha256-bytes RECEIVED-FILE-SEXP)) (fourth sexp)))
                            (data-tempered-frame)])
                     (set! CURR-SAVE-PATH (string-append SAVE-PATH
                                                         "received -"
                                                         (begin (date-display-format 'iso-8601)
                                                                (date->string (current-date)))
                                                         " -"
                                                         CHATTING-WITH
                                                         " -"
                                                         RECEIVED-FILE-NAME
                                                         ))
                     (date-display-format 'american)
                     (cond [(not (directory-exists? (string-append "data/chat-histories/" user-id "-" CHATTING-WITH "/")))
                              (make-directory* (string-append "data/chat-histories/" user-id "-" CHATTING-WITH "/"))])
                     (display-to-file RECEIVED-FILE-SEXP CURR-SAVE-PATH #:exists 'replace)
                     (send receiving-frame show #f)
                     (send msg set-label "                      File received!")
                     (let ([to-send (string-append user-id
                                                   "--->"
                                                   (date->string (current-date) #t)
                                                   "  :  "
                                                   RECEIVED-FILE-NAME
                                                   " - "
                                                   "File received from "
                                                   CHATTING-WITH
                                                   "\n")])
                       (send text-obj lock #f)
                       (send text-obj insert to-send)
                       (set! chat-history (string-append chat-history "\n" to-send))
                       (display-to-file to-send curr-history-path #:mode 'text #:exists 'append)
                       (send text-edit set-editor text-obj #t)
                       (send text-obj lock #t))
                     (set! PROCESSING? #f)
                     (set! RECEIVED-PACKETS '())
                     (received-file-frame)
                     state])]

        [(and (list? sexp) (equal? (car sexp) "PARTNER-TYPING"))
         (send msg set-label (string-append "                  " (cadr sexp) " is typing..."))
         state]
        
        [else state]))

(define (main_frame stat)
  (cond [(equal? stat "STARTING-FRAME") (starting-frame)]
        [(equal? stat "RUN-MANCALA") (starting-frame)]
        [(equal? stat "RUN-WORD-SEARCH") (starting-frame)]
        [(equal? stat "ABOUT-USERS") (about-frame)]
        [(equal? stat "DECISION-FRAME") (decision-frame)]
        [(equal? stat "BE-A-USER") (decision-frame)]
        [(equal? stat "GO-TO-STARTING-FRAME") (decision-frame)]
        [(equal? stat "RUN-UNIVERSE") (decision-frame)]
        [(equal? stat "PLAY") (welcome_frame)]
        [(equal? stat "WELCOME") (welcome_frame)]
        [(equal? stat "LOGIN") (welcome_frame)]
        [(equal? stat "GO-TO-WALL") (wall-frame)]
        [(equal? stat "RUN-MANCALA-ONLINE") (wall-frame)]
        [(equal? stat "RUN-WORD-SEARCH-ONLINE") (wall-frame)]
        [(equal? stat "ONLINE-HELP") (online-help-frame)]
        [(equal? stat "START-THIS-CHAT") (wall-frame)]
        [(equal? stat "USER-ACCOUNT-FRAME") (user-account-frame)]
        [(equal? stat "POSSIBLY-GOING-OFFLINE") (wall-frame)]
        [(equal? stat "ONLINE-SETTINGS") (settings-frame)]
        [(equal? stat "CHANGE-PASSWORD") (settings-frame)]
        [(equal? stat "CHANGE-STATUS") (settings-frame)]
        [(equal? stat "CHANGE-PERSONAL-DETAILS") (settings-frame)]
        [(equal? stat "CHANGE-PROFILE-PIC") (settings-frame)]
        [(equal? stat "NEW-USER") (welcome_frame)]
        [(equal? stat "GO-ONLINE") (welcome_frame)]
        [else empty_frame]))

(define INFORM-UNIVERSE? #f)
(define STARTED-FRAME? #f)

(define (ask-user)
  (define ask-frame
    (new frame%
         [label "Signing-out"]
         [width 200]
         [height 150]))
  (define msg
    (new message%
         [parent ask-frame]
         [label "Do you want to Sign-out?"]
         [font our-font-2]))
  (define ask-panel
    (new horizontal-pane%
         [parent ask-frame]
         [min-width 100]
         [alignment '(center center)]))
  (define yes-button
    (new button%
         [parent ask-panel]
         [label "Yes"]
         [callback (λ (b e) (send ask-frame show #f)
                     (set! INFORM-UNIVERSE? #t))]
         [font our-font-2]
         [min-width 50]))
  (new button%
       [parent ask-panel]
       [label "No"]
       [callback (λ (b e) (send ask-frame show #f))]
       [font our-font-2]
       [min-width 50])
  (send ask-frame show #t))

(define (change-profile-pic)
  (define ask-frame
    (new frame%
         [label "Choose-profile-pic"]
         [width 200]
         [height 150]))
  (define msg
    (new message%
         [parent ask-frame]
         [label "Do you want to change your profile pic?"]
         [font our-font-2]))
  (define ask-panel
    (new horizontal-pane%
         [parent ask-frame]
         [min-width 100]
         [alignment '(center center)]))
  (define yes-button
    (new button%
         [parent ask-panel]
         [label "Yes"]
         [callback (λ (b e) (let* ([profile-pic-path (get-file "Choose profile pic"
                                                               #f #f #f #f null (list (list "Any" "*.jp*g;*.png")))]
                                   [ori-pic (bitmap/file profile-pic-path)]
                                   [profile-pic (resize-appropriately ori-pic 270)])
                              (set! USER-PROFILE-PIC profile-pic)
                              (set-profile-pic profile-pic)
                              (set-original-profile-pic ori-pic)
                              (save-image ori-pic "data/user-profile-pic.png")
                              (send ask-frame show #f)))]
                     
         [font our-font-2]
         [min-width 50]))
  (new button%
       [parent ask-panel]
       [label "No"]
       [callback (λ (b e) (send ask-frame show #f))]
       [font our-font-2]
       [min-width 50])
  (send ask-frame show #t))

(define (change-password)
  (define fr
    (new frame%
         [label "Change-password"]
         [width 600]
         [height 200]))
  (define msg
    (new message%
         [parent fr]
         [label "Enter old and new password"]
         [horiz-margin 10]
         [vert-margin 10]
         [font our-font-2]))
  (define old-pass-field
    (new text-field%
         [parent fr]
         [label "Old password: "]
         [init-value OLD-PASSWORD]
         [style (list 'password)]
         [font our-font-2]
         [vert-margin 5]))
  (define new-pass-field
    (new text-field%
         [parent fr]
         [label "New password: "]
         [callback (λ (t e) (cond [(equal? (send e get-event-type) 'text-field-enter)
                                   (let ([old-pass (send old-pass-field get-value)]
                                         [new-pass (send new-pass-field get-value)])
                                     (cond [(< (string-length new-pass) 5)
                                            (send msg1 set-label "Password must be at least 5 characters long")
                                            (send new-pass-field set-value "")]
                                           [(string-contains? new-pass " ")
                                            (send msg1 set-value "Password must not contain spaces")
                                            (send new-pass-field set-value "")]
                                           [else (set! OLD-PASSWORD old-pass)
                                                 (set! NEW-PASSWORD new-pass)
                                                 (set! CHANGE-PASSWORD? #t)
                                                 (send fr show #f)]))]
                                  [else (let* ([new-pass (send new-pass-field get-value)]
                                               [len (string-length new-pass)])
                                          (cond [(> len 16) (send msg1 set-label "very-strong")]
                                                [(> len 11) (send msg1 set-label "strong")]
                                                [(> len 8) (send msg1 set-label "moderate")]
                                                [(> len 5) (send msg1 set-label "weak")]
                                                [else (send msg1 set-label "Password must be more than 5 characters long")]))]))]
         [init-value NEW-PASSWORD]
         [style (list 'password)]
         [font our-font-2]
         [vert-margin 5]))

  (define msg1
    (new message%
         [parent fr]
         [label "                                                      "]
         [font our-font-2]
         [vert-margin 5]))

  (define bt
    (new button%
         [parent fr]
         [label "Change password"]
         [callback (λ (b e) (let ([old-pass (send old-pass-field get-value)]
                                  [new-pass (send new-pass-field get-value)])
                              (cond [(< (string-length new-pass) 5)
                                     (send msg1 set-label "Password must be at least 5 characters long")
                                     (send new-pass-field set-value "")]
                                    [(string-contains? new-pass " ")
                                     (send msg1 set-value "Password must not contain spaces")
                                     (send new-pass-field set-value "")]
                                    [else (set! OLD-PASSWORD old-pass)
                                          (set! NEW-PASSWORD new-pass)
                                          (set! CHANGE-PASSWORD? #t)
                                          (send fr show #f)])))]
         [font our-font-2]
         [min-width 60]
         [min-height 30]
         [vert-margin 3]))
  (send fr show #t))

(define (change-status)
  (define fr
    (new frame%
         [label "Change Status"]
         [width 700]
         [height 100]))
  (define status-field
    (new text-field%
         [parent fr]
         [label "Enter new Status: "]
         [font our-font-2]
         [vert-margin 10]
         [min-height 30]))
  (define bt
    (new button%
         [parent fr]
         [label "Change"]
         [callback (λ (b e) (let ([new-status (send status-field get-value)])
                              (set! NEW-USER-STATUS new-status)
                              (set-user-status USER-STATUS)
                              (set! CHANGE-STATUS? #t)
                              (send fr show #f)))]
         [font our-font-2]))
  (send fr show #t))

(define (change-personal-details)
  (define fr
    (new frame%
         [label "Change personal details"]
         [width 200]
         [height 100]))
  (define name-field
    (new text-field%
         [parent fr]
         [label "Name :"]
         [init-value (car USER-DETAILS)]
         [font our-font-2]
         [vert-margin 10]))
  (define nationality-field
    (new text-field%
         [parent fr]
         [label "Nationality: "]
         [init-value (cadr USER-DETAILS)]
         [font our-font-2]
         [vert-margin 5]))
  (define age-field
    (new text-field%
         [parent fr]
         [label "Age :"]
         [init-value (third USER-DETAILS)]
         [font our-font-2]
         [vert-margin 5]))
  (define msg
    (new message%
         [parent fr]
         [label "                            "]
         [font our-font-1]
         [vert-margin 5]))
         
  (define bt
    (new button%
         [parent fr]
         [label "Change"]
         [callback (λ (b e) (let ([name (send name-field get-value)]
                                  [nationality (send nationality-field get-value)]
                                  [age (send age-field get-value)])
                              (cond [(or (string=? name "")
                                         (string=? nationality "")
                                         (string=? age "")
                                         (not (number? (string->number age))))
                                     (send msg set-label "Please enter your details correctly")]
                                    [else (set! NEW-USER-DETAILS (list name nationality age))
                                          (set! CHANGE-USER-DETAILS? #t)
                                          (send fr show #f)])))]
         [font our-font-2]
         [vert-margin 5]))
  (send fr show #t))
  

(define loading-frame
  (new frame%
       [label "Loading world..."]
       [width 100]
       [height 30]))
(define loading-msg
  (new message%
       [parent loading-frame]
       [label "Loading new world...\nThis might take a few seconds"]))

(define REQUEST-MANCALA-LIST? #f)
(define RUN-MANCALA? #f)
(define RUN-MANCALA-ONLINE? #f)
(define ADD-USER? #f)
(define ADD-MANCALA-USER? #f)
(define ADD-TICKS 0)
(define IS-ONLINE? #f)
(define RUN-WORD-SEARCH? #f)

(define (online-world-make)
  (set! IS-ONLINE? #t)
  (set! RUN-MANCALA-ONLINE? #f)
  (set! RUN-WORD-SEARCH? #f)
  (big-bang "GO-TO-WALL"
    (name user-id)
    (to-draw main_frame)
    (stop-when (λ (stat) (begin0 (or (equal? stat "GOING-OFFLINE")
                                     (equal? stat "RUN-MANCALA-ONLINE")
                                     (equal? stat "RUN-WORD-SEARCH-ONLINE"))
                                 (cond [(and (equal? stat "GOING-OFFLINE")
                                             GONE-OFFLINE?) (set! GONE-OFFLINE? #f)
                                                            (world-make)]
                                       [(and (equal? stat "RUN-MANCALA-ONLINE")
                                             (not RUN-MANCALA-ONLINE?)) (set! RUN-MANCALA-ONLINE? #t)
                                                                        (set! ADD-MANCALA-USER? #t)
                                                                        (online-mancala-world-make)]
                                       [(and (equal? stat "RUN-WORD-SEARCH-ONLINE")
                                             (not RUN-WORD-SEARCH?)) (set! RUN-WORD-SEARCH? #t)
                                                                     (word-game-world)]))))
    (close-on-stop #t)
    (on-receive receive-msg-handler)
    (on-mouse (λ (stat x y inp)
                (cond [(or (mouse=? inp "enter")
                           (mouse=? inp "leave")) stat]
                      [(equal? stat "GO-TO-WALL") (wall_frame_update x y inp)]
                      [(equal? stat "START-THIS-CHAT")
                       (set! REQUEST-LIST? #t)
                       (wall_frame_update x y inp)]
                      [(equal? stat "POSSIBLY-GOING-OFFLINE") (ask-user) (wall_frame_update x y inp)]
                      [(equal? stat "ONLINE-SETTINGS") (online_settings_frame_update x y inp)]
                      [(equal? stat "ONLINE-HELP") (online-help-update x y inp)]
                      [(equal? stat "RUN-MANCALA-ONLINE") (wall_frame_update x y inp)]
                      [(equal? stat "RUN-WORD-SEARCH-ONLINE") (wall_frame_update x y inp)]
                      [(equal? stat "CHANGE-PASSWORD") (change-password) (online_settings_frame_update x y inp)]
                      [(equal? stat "CHANGE-STATUS") (change-status) (online_settings_frame_update x y inp)]
                      [(equal? stat "CHANGE-PERSONAL-DETAILS") (change-personal-details) (online_settings_frame_update x y inp)]
                      [(equal? stat "CHANGE-PROFILE-PIC") (change-profile-pic) (online_settings_frame_update x y inp)]
                      [(equal? stat "USER-ACCOUNT-FRAME") (user-acc-frame-update x y inp)])))
    (on-tick (λ (stat) (cond [INFORM-UNIVERSE? (cond [CHATTING? (set! INFORM-UNIVERSE? #f)
                                                                (set! CHATTING? #f)
                                                                (make-package stat (list "USER-GOING-OFFLINE"
                                                                                         CHATTING-WITH))]
                                                     [else (set! INFORM-UNIVERSE? #f)
                                                           (make-package stat (list "USER-GOING-OFFLINE"
                                                                                    #f))])]

                             [ADD-USER? (cond [(<= ADD-TICKS 7) (set! ADD-TICKS (+ ADD-TICKS 1)) stat]
                                              [else (set! ADD-USER? #f)
                                                    (set! ADD-TICKS 0)
                                                    (make-package stat (list "ADD-USER"
                                                                             user-id
                                                                             HASHED-PASSWORD
                                                                             (map (λ (x) (encrypt DEFAULT-ENCRYPTION-KEY x)) USER-DETAILS)
                                                                             (encrypt DEFAULT-ENCRYPTION-KEY USER-STATUS)))])]

                             [CHANGE-PASSWORD? (set! CHANGE-PASSWORD? #f)
                                               (make-package stat (list "CHANGE-USER-PASSWORD"
                                                                        (bytes->hex-string (sha256-bytes (open-input-string OLD-PASSWORD)))
                                                                        (bytes->hex-string (sha256-bytes (open-input-string NEW-PASSWORD)))))]

                             [CHANGE-STATUS? (set! CHANGE-STATUS? #f)
                                             (make-package stat (list "CHANGE-USER-STATUS"
                                                                      (encrypt DEFAULT-ENCRYPTION-KEY NEW-USER-STATUS)
                                                                      HASHED-PASSWORD))]

                             [CHANGE-USER-DETAILS? (set! CHANGE-USER-DETAILS? #f)
                                                   (make-package stat (list "CHANGE-USER-DETAILS"
                                                                            (map (λ (x) (encrypt DEFAULT-ENCRYPTION-KEY x)) NEW-USER-DETAILS)
                                                                            HASHED-PASSWORD))]
                                                                            
                             
                             [REQUEST-LIST? (set! REQUEST-LIST? #f)
                                            (make-package stat (list "GIVE-ACTIVE-USERS"))]

                             
                             [CHATTING? (cond [SEND-PACKET? (set! SEND-PACKET? #f)
                                                            (begin0
                                                              (make-package stat (list "SEND-THIS-PACKET"
                                                                                       CHATTING-WITH
                                                                                       ENCRYPTED-FILENAME
                                                                                       (if (null? (cdr LIST-FILE-SEXP))
                                                                                           (bytes->hex-string (sha256-bytes SENT-FILE-SEXP))
                                                                                           #f)
                                                                                       initial-sending-range
                                                                                       (car LIST-FILE-SEXP)))
                                                              (set! LIST-FILE-SEXP (cdr LIST-FILE-SEXP))
                                                              )]
                                              [SEND-MESSAGE? (set! SEND-MESSAGE? #f)
                                                             (make-package stat (list "SEND-THIS-MESSAGE"
                                                                                      CHATTING-WITH
                                                                                      CURR-DATE
                                                                                      curr-encrypted-message))]
                                              [TYPING? (make-package stat (list "USER-TYPING"
                                                                                CHATTING-WITH))]

                                              [(and (send soft-frame is-shown?)
                                                    (not STARTED-FRAME?)) (set! STARTED-FRAME? #t)
                                                                    stat]
                                              [(and STARTED-FRAME?
                                                    (not (send soft-frame is-shown?)))
                                               (set! CHATTING? #f)
                                               (set! STARTED-FRAME? #f)
                                               (begin0 (make-package stat (list "USER-DISCONNECTED-FROM-CHAT"
                                                                                CHATTING-WITH))
                                                       (set! CHATTING-WITH ""))]
                                              
                                              [else stat])]

                             [START-CHAT? (set! START-CHAT? #f)
                                          (make-package stat (list "CONNECT-WORLD"
                                                                   TO-CHAT-WITH))]

                             [VIEW-ACCOUNT? (set! VIEW-ACCOUNT? #f)
                                            (make-package stat (list "VIEW-USER-ACCOUNT"
                                                                     TO-CHAT-WITH))]

                             [PROCESSING? stat]

                             [else stat])))
    (register ip-address)
    ))
                                                    
(define (world-make)
  (set! IS-ONLINE? #f)
  (set! RUN-MANCALA? #f)
  (set! RUN-WORD-SEARCH? #f)
  (big-bang "STARTING-FRAME"
    (name "default")
    (to-draw main_frame)
    (stop-when (λ (stat) (begin0 (or (equal? stat "GO-ONLINE")
                                     (equal? stat "RUN-MANCALA")
                                     (equal? stat "RUN-WORD-SEARCH"))
                                 (cond [(and (equal? stat "GO-ONLINE")
                                             MADE-ONLINE?) (set! MADE-ONLINE? #f)
                                                           (online-world-make)]
                                       [(and (equal? stat "RUN-MANCALA")
                                             (not RUN-MANCALA?)) (set! RUN-MANCALA? #t)
                                                                 (mancala-world-make)]
                                       [(and (equal? stat "RUN-WORD-SEARCH")
                                             (not RUN-WORD-SEARCH?)) (set! RUN-WORD-SEARCH? #t)
                                                                     (word-game-world)]))))
    (close-on-stop #t)
    (on-receive receive-msg-handler)
    (on-mouse (λ (stat x y inp)
                (cond [(or (mouse=? inp "enter")
                           (mouse=? inp "leave")) stat]
                      [(equal? stat "STARTING-FRAME") (starting-frame-update x y inp)]
                      [(equal? stat "RUN-MANCALA") (starting-frame-update x y inp)]
                      [(equal? stat "RUN-WORD-SEARCH") (starting-frame-update x y inp)]
                      [(equal? stat "ABOUT-USERS") (about-users-update x y inp)]
                      [(equal? stat "PLAY") (welcome_frame_update x y inp)]
                      [(equal? stat "WELCOME") (welcome_frame_update x y inp)]
                      [(equal? stat "LOGIN") (do-login) (welcome_frame_update x y inp)]
                      [(equal? stat "NEW-USER") (register-user) (welcome_frame_update x y inp)])))
    (on-tick (λ (stat) (cond [REGISTER? (set! REGISTER? #f)
                                        (set! START-TICKS? #t)
                                        (make-package stat (list "REGISTER-USER"
                                                                 user-id
                                                                 HASHED-PASSWORD
                                                                 ))]
                             [LOGIN? (set! LOGIN? #f)
                                     (set! START-TICKS? #t)
                                     (make-package stat (list "LOGIN-USER"
                                                              login-username
                                                              HASHED-PASSWORD
                                                              (map (λ (x) (encrypt DEFAULT-ENCRYPTION-KEY x)) USER-DETAILS)
                                                              (encrypt DEFAULT-ENCRYPTION-KEY USER-STATUS)
                                                              ))]
                             [(not HOST-ONLINE-STATUS)
                              (cond [START-TICKS?
                                     (cond [(> TICKS 40) (set! START-TICKS? #f)
                                                         (begin0 "WELCOME"
                                                                 (message-box "Host not active"
                                                                              (string-append "Host servers are not active.\n"
                                                                                             "Please try again after some time.")
                                                                              #f
                                                                              (list 'ok 'stop)))]
                                           [else (set! TICKS (+ TICKS 1))
                                                 stat])]
                                    [else stat])]
                             [else stat])))
   
    (register ip-address)
    ))

(define (word-game-world)
  (set! GO-TO-HOME? #f)
  (big-bang "WORD-WELCOME"
    [name "WORD SEARCH"]
    [to-draw word-main-frame]
    [stop-when (λ (stat) (begin0 (equal? stat "GO-TO-HOME")
                                 (cond [(and (equal? stat "GO-TO-HOME")
                                             (not GO-TO-HOME?)) (set! GO-TO-HOME? #t)
                                                                (cond [IS-ONLINE? (set! ADD-USER? #t)
                                                                                  (online-world-make)]
                                                                      [else (world-make)])])))]
    [close-on-stop #t]
    [on-mouse (λ (stat x y inp)
                (cond [(or (mouse=? inp "enter")
                           (mouse=? inp "leave")) stat]
                      [(equal? stat "WORD-WELCOME") (word-welcome-frame-update x y inp)]
                      [(equal? stat "CREATE-LEVEL") (take-level-details) (word-welcome-frame-update x y inp)]
                      [(equal? stat "GO-TO-HOME") (word-welcome-frame-update x y inp)]
                      [(equal? stat "LEVEL-1") (level-1-frame-update x y inp)]
                      [(equal? stat "LEVEL_1-SOLN") (level-1-soln-frame-update x y inp)]
                      [(equal? stat "CONGRATULATIONS") (congrats-frame-update x y inp)]
                      [(equal? stat "GAMEOVER") (gameover-frame-update x y inp)]
                      [else (error "WRONG STAT PASSED IN ON-MOUSE")]))]
    [on-tick (λ (stat) (cond [(> diff timer-timings) (begin (send timer-1 stop) "GAMEOVER")]
                             [(and (null? level-1-word-list) (equal? stat "LEVEL-1") (equal? creation #t))
                              (send timer-1 stop) (set-creation #f) "WORD-WELCOME"]
                             [(and (null? level-1-word-list) (equal? stat "LEVEL-1")) (begin (send timer-1 stop) "CONGRATULATIONS")]
                             [create-level? (begin (set-create-level #f) (work 10) "LEVEL-1")]
                             [else stat]))]))

(define (deny-2-player)
  (message-box "Cannot Connect"
               "You are currently offline"
               #f (list 'ok 'stop)))
(define PLAYING-MANCALA? #f)

(define GO-TO-HOME? #f)
(define (mancala-world-make)
  (set! GO-TO-HOME? #f)
  (big-bang "MANCALA"
    (name "MANCALA")
    (to-draw mancala-main_frame)
    (stop-when (λ (stat) (begin0 (equal? stat "GO-TO-HOME")
                                 (cond [(and (equal? stat "GO-TO-HOME")
                                             (not GO-TO-HOME?)) (set! GO-TO-HOME? #t)
                                                                (world-make)]))))
    (close-on-stop #t)
    (on-mouse (λ (stat x y inp)
                (cond [(or (mouse=? inp "enter")
                                   (mouse=? inp "leave")) stat]
                      [(equal? stat "MANCALA") (mancala-frame-update stat x y inp)]
                      [(equal? stat "MANCALA-WELCOME") (mancala_welcome_frame_update x y inp)]
                      [(equal? stat "GAME-1-PLAYER") (mancala_game_frame_update stat x y inp)]
                      [(equal? stat "ASK-MANCALA-PARTNER") (deny-2-player) (mancala_welcome_frame_update x y inp)]
                      [(equal? stat "GAME-OVER") (mancala-game-over-update x y inp)]
                      [(equal? stat "SETTINGS") (mancala_settings_frame_update x y inp)]
                      [(equal? stat "HOW-TO-PLAY") (howtoplay_frame_update x y inp)]
                      [(equal? stat "GO-TO-HOME") (mancala-frame-update stat x y inp)]
                              
                      [else  (error "Wrong stat passed")])))
    (on-tick (λ (stat) (cond [GAME-OVER? (set-game-over #f)
                                         "GAME-OVER"]
                             [else (MOVING-CHANCE stat)])))))

(define (mancala-receive-handler stat sexp)
  (cond [(and (list? sexp) (equal? (car sexp) "User not active")) (begin
                                                                    (set! TO-PLAY-WITH "")
                                                                    (message-box "Cannot Connect"
                                                                                 (string-append (cadr sexp) " : " (car sexp)))
                                                                    stat)]
        
        [(and (list? sexp) (equal? (car sexp) "User playing with someone else"))
         (set! TO-PLAY-WITH "")
         (message-box "Cannot Connect"
                      (string-append (cadr sexp) " is playing with someone else") #f (list 'ok))
         "MANCALA-WELCOME"]

        [(and (list? sexp) (equal? (car sexp) "OTHER-USER-DISCONNECTED"))
         (set-playing-online #f)
         (message-box "OTHER-USER-DISCONNECTED"
                      (string-append PLAYING-WITH " has disconnected from this game")
                      #f (list 'ok 'stop))
         (set! PLAYING-WITH "")
         "MANCALA"]

        [(and (list? sexp) (equal? (car sexp) "RESTART-GAME"))
         (initialize)
         (message-box "Restarted"
                      "The game has restarted")
         "GAME-2-PLAYER"]

        [(and (list? sexp) (equal? (car sexp) "RECEIVE-MANCALA-MOVE"))
         (set-clicked-cup (modulo (+ 7 (cadr sexp)) 14))
         (set-made-move #t)
         stat]
        
        [(and (list? sexp) (equal? (car sexp) "ANOTHER-USER-REQUESTING-MANCALA"))
         (let ([user-decision (message-box/custom (car sexp)
                                                  (string-append (cadr sexp) " has requested to play mancala with you")
                                                  "Accept"
                                                  "Reject"
                                                  #f)])
           (cond [(= user-decision 1) (set! PLAYING-MANCALA? #t)
                                      (set! PLAYING-WITH (cadr sexp))
                                      (set-playing-online #t)
                                      (set-mancala (third sexp))
                                      (make-package stat
                                                    (list "MANCALA-REQUEST-APPROVED"
                                                          (cadr sexp)))]
                 [else (make-package stat (list "MANCALA-REQUEST-REJECTED"
                                                (cadr sexp)))]))]

        [(and (list? sexp) (equal? (car sexp) "MANCALA-REQUEST-ACCEPTED"))
         (set! TO-PLAY-WITH "")
         (set! PLAYING-WITH (cadr sexp))
         (set! PLAYING-MANCALA? #t)
         (set-playing-online #t)
         (set-first-move (third sexp))
         (initialize)
         (message-box "Request accepted"
                      (string-append (cadr sexp) " has accepted your Mancala request"))
         "GAME-2-PLAYER"]

        [(and (list? sexp) (equal? (car sexp) "MANCALA-REQUEST-REJECTED-BY-USER"))
         (set! TO-PLAY-WITH "")
         (message-box "Request rejected"
                      (string-append (cadr sexp) "  has rejected your chat request")
                      #f (list 'ok 'stop))
         "MANCALA-WELCOME"]

        [(and (list? sexp) (equal? (car sexp) "YOU-APPROVED-A-MANCALA-REQUEST"))
         (set-first-move (cadr sexp))
         (initialize)
         ;(displayln "Hello")
         "GAME-2-PLAYER"]

        [(and (list? sexp) (equal? (car sexp) "Mancala request sent"))
         (message-box "Request sent"
                      (string-append (cadr sexp) " : " (car sexp)))
         "MANCALA-WELCOME"]
         
        [(and (list? sexp) (equal? (car sexp) "RECEIVE-MANCALA-USERS-LIST"))
         (set! Active-users (sort (cadr sexp) string<?))
         (set! filtered-user-list Active-users)
         (online-users-mancala-frame)
         stat]

        [else stat]))

(define (ONLINE-MOVING-CHANCE stat)
  (cond [ADD-MANCALA-USER? (cond [(<= ADD-TICKS 7) (set! ADD-TICKS (+ ADD-TICKS 1)) stat]
                                 [else (set! ADD-MANCALA-USER? #f)
                                       (set! ADD-TICKS 0)
                                       (make-package stat (list "ADD-USER"
                                                                user-id
                                                                HASHED-PASSWORD
                                                                (map (λ (x) (encrypt DEFAULT-ENCRYPTION-KEY x))
                                                                     USER-DETAILS)
                                                                (encrypt DEFAULT-ENCRYPTION-KEY USER-STATUS)))])]
        [ONLINE-GAME-OVER? (set-online-game-over #f)
                           "ONLINE-GAME-OVER"]
        [GAME-OVER? (set-game-over #f)
                    "GAME-OVER"]
        [(equal? stat "GAME-1-PLAYER")
         (MOVING-CHANCE stat)]
        [REQUEST-MANCALA-LIST? (set! REQUEST-MANCALA-LIST? #f)
                               (make-package stat (list "GIVE-MANCALA-USERS"))]
        [START-MANCALA? (set! START-MANCALA? #f)
                        (make-package stat (list "CONNECT-MANCALA-WORLD"
                                                 TO-PLAY-WITH
                                                 mancala))]
        [SEND-MOVE? (set-send-move #f)
                    (make-package stat (list "SEND-THIS-MOVE"
                                             clicked-cup
                                             PLAYING-WITH))]
        [DISCONNECTING-FROM-GAME? (set-disconnect #f)
                                  (set-playing-online #f)
                                  (begin0
                                    (make-package "MANCALA" (list "USER-DISCONNECTING-FROM-GAME"
                                                                  PLAYING-WITH))
                                    (set! PLAYING-WITH ""))]
        [RESTARTING? (set-restart #f)
                     (make-package stat (list "RESTART-GAME"
                                              PLAYING-WITH))]
        [else (MAKE-MANCALA-MOVE stat)]))

(define (online-mancala-world-make)
  (set! GO-TO-HOME? #f)
  (big-bang "MANCALA"
    (name user-id)
    (to-draw mancala-main_frame)
    (stop-when (λ (stat) (begin0 (equal? stat "GO-TO-HOME")
                                 (cond [(and (equal? stat "GO-TO-HOME")
                                             (not GO-TO-HOME?)) (set! GO-TO-HOME? #t)
                                                                (set! ADD-USER? #t)
                                                                (online-world-make)]))))
    (close-on-stop #t)
    (on-receive mancala-receive-handler)
    (on-mouse (λ (stat x y inp)
                (cond [(or (mouse=? inp "enter")
                                   (mouse=? inp "leave")) stat]
                      [(equal? stat "MANCALA") (mancala-frame-update stat x y inp)]
                      [(equal? stat "MANCALA-WELCOME") (mancala_welcome_frame_update x y inp)]
                      [(equal? stat "GAME-1-PLAYER") (set-playing-online #f) (mancala_game_frame_update stat x y inp)]
                      [(equal? stat "GAME-2-PLAYER") (online-mancala_game_frame_update stat x y inp)]
                      [(equal? stat "ASK-MANCALA-PARTNER") (set! REQUEST-MANCALA-LIST? #t)
                                                           (mancala_welcome_frame_update x y inp)]
                      [(equal? stat "ONLINE-GAME-OVER") (mancala-online-game-over-update x y inp)]
                      [(equal? stat "SETTINGS") (mancala_settings_frame_update x y inp)]
                      [(equal? stat "HOW-TO-PLAY") (howtoplay_frame_update x y inp)]
                      [(equal? stat "GO-TO-HOME") (mancala-frame-update stat x y inp)]
                              
                      [else  (error "Wrong stat passed")])))
    (on-tick ONLINE-MOVING-CHANCE)
    (register ip-address)))

(define GO-TO-STARTING-FRAME? #f)

(define (host-ip-address)
  (define ip-frame
    (new frame%
       [label "Enter host IP-Address"]
       [width 300]
       [height 100]))
  (define ip-msg
    (new message%
       [parent ip-frame]
       [label "Enter IP-Address of host"]
       [horiz-margin 10]
       [vert-margin 10]
       [font our-font-2]))
    (define ip-field
      (new text-field%
           [parent ip-frame]
           [label ""]
           [callback (λ (t e) (cond [(equal? (send e get-event-type) 'text-field-enter)
                                     (set! ip-address (send ip-field get-value))
                                     (send ip-frame show #f)
                                     (set! GO-TO-STARTING-FRAME? #t)]))]
           [init-value ip-address]
           [horiz-margin 10]
           [vert-margin 5]
           [font our-font-2]))
    (new button%
         [parent ip-frame]
         [label "Proceed"]
         [callback (λ (b e)
                     (set! ip-address (send ip-field get-value))
                     (send ip-frame show #f)
                     (set! GO-TO-STARTING-FRAME? #t))]
         [font our-font-2]
         [vert-margin 5]
         [min-width 50])
  (send ip-frame show #t))

(define STARTED? #f)
(define UNIVERSE-RUNNING? #f)
(define OPEN-WORLD? #f)

(define (begin-world-make)
  (big-bang "DECISION-FRAME"
    (name "default")
    (to-draw main_frame)
    (stop-when (λ (stat) (begin0 (or (equal? stat "GO-TO-STARTING-FRAME")
                                     (equal? stat "RUN-UNIVERSE"))
                                 (cond [(and (equal? stat "GO-TO-STARTING-FRAME")
                                             (not STARTED?)) (set! STARTED? #t) (world-make)]
                                       [(and (equal? stat "RUN-UNIVERSE")
                                             (not UNIVERSE-RUNNING?)) (set! UNIVERSE-RUNNING? #t)
                                                                      (set! ip-address LOCALHOST)
                                                                      (set! OPEN-WORLD? #t)
                                                                      ;(send loading-frame show #t)
                                                                      (run-universe)]))))
    (close-on-stop #t)
    (on-mouse (λ (stat x y inp)
                (cond [(or (mouse=? inp "enter")
                           (mouse=? inp "leave")) stat]
                      [(equal? stat "DECISION-FRAME") (decision-frame-update x y inp)]
                      [(equal? stat "BE-A-USER") (host-ip-address) (decision-frame-update x y inp)]
                      [(equal? stat "RUN-UNIVERSE") (decision-frame-update x y inp)]
                      [(equal? stat "GO-TO-STARTING-FRAME") (decision-frame-update x y inp)])))
    (on-tick (λ (stat)
               (cond [OPEN-WORLD? (set! OPEN-WORLD? #f)
                                  "GO-TO-STARTING-FRAME"]
                     [GO-TO-STARTING-FRAME? (set! GO-TO-STARTING-FRAME? #f)
                                            "GO-TO-STARTING-FRAME"]
                     [else stat])))))
                

;;;;;;;;;;;;;;;;;;;;;;;;;;;;FINAL-CALL;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(begin-world-make)