#lang racket/gui
(provide (all-defined-out))
(require 2htdp/image)
(require 2htdp/batch-io)
(require 2htdp/universe)
(require racket/date)
;(require images/icons/style)
(require "encryption.rkt")
(define DEFAULT-ENCRYPTION-KEY '(#\k #\J #\H #\t #\r #\Q #\a #\m #\V #\g #\c #\O #\G #\o #\j #\Y #\i #\x #\y #\B #\p #\b #\I #\w #\f #\d #\M #\X #\E #\n #\C #\l #\N #\P #\F #\Z #\q #\D #\e #\A #\s #\T #\S #\u #\h #\L #\U #\R #\v #\z #\W #\K))

(define (convert-writable data)
  (foldr (λ (x y) (string-append (car x) " " (number->string (cdr x)) "\n" y)) "" data))

(define (convert-readable data ans)
  (cond [(null? data) (reverse ans)]
        [else (convert-readable (cddr data) (cons (cons (car data) (string->number (cadr data))) ans))]))

(define (format database ans)
  (cond [(null? database) (reverse ans)]
        [else (format (cddr database) (cons (cons (car database) (cadr database)) ans))]))

(define (rev-format database ans)
  (cond [(null? database) ans]
        [else (rev-format (cdr database) (string-append ans (caar database) " " (cdar database) "\n"))]))

(define DATABASE-PATH "host-data/database.txt")
(define DATABASE (make-hash (if (file-exists? DATABASE-PATH)
                                (format (read-words DATABASE-PATH) '())
                                '())))
(define ACTIVE-MEMBERS (make-hash))
(define PLAYING-MEMBERS '())
;(define PENDING-MESSAGES '())
(define (UPDATE-DATABASE new-data)
  (begin0 (hash-set! DATABASE (car new-data) (cdr new-data))
          (display-to-file (string-append (car new-data) " " (cdr new-data) "\n") DATABASE-PATH #:mode 'text #:exists 'append)))
(define CHATTING-USERS '())

(define user-account%
  (class object%
    (init username)
    (init passwd)
    (init name)
    (init nationality)
    (init age)
    (init status)
    (super-new)
    (define user-password passwd)
    (define USERNAME username)
    (define NAME name)
    (define NATIONALITY nationality)
    (define AGE age)
    (define STATUS status)
    (define/public (change-user-password old-pass new-pass)
      (cond [(equal? old-pass user-password)
             (set! user-password new-pass)
             'done]
            [else 'Incorrect-password]))
    (define/public (change-user-status pass new-status)
      (cond [(equal? pass user-password)
             (set! STATUS new-status)
             'done]
            ))
    (define/public (change-user-details pass new-details)
      (cond [(equal? pass user-password)
             (let ([decrypted-details (map (λ (x) (decrypt DEFAULT-ENCRYPTION-KEY x)) new-details)])
               (set! NAME (first decrypted-details))
               (set! NATIONALITY (second decrypted-details))
               (set! AGE (third decrypted-details))
               'done)]))
    (define/public (get-username)
      USERNAME)
    (define/public (get-name)
      NAME)
    (define/public (get-nationality)
      NATIONALITY)
    (define/public (get-age)
      AGE)
    (define/public (get-status)
      STATUS)))

(define (create-account user-data)
  (let ([decrypted-data (map (λ (x) (decrypt DEFAULT-ENCRYPTION-KEY x)) (third user-data))])
    (new user-account%
         [username (first user-data)]
         [passwd (second user-data)]
         [name (car decrypted-data)]
         [nationality (cadr decrypted-data)]
         [age (last decrypted-data)]
         [status (decrypt DEFAULT-ENCRYPTION-KEY (last user-data))])))

(define (add-world univ wrld)
         (make-bundle (cons wrld univ)
               (list (make-mail wrld "Connected with host"))
               '()))

(define (switch univ wrld m)
  (cond [(and (list? m) (equal? (car m) "REGISTER-USER"))
         (cond [(hash-has-key? DATABASE (second m)) (make-bundle univ
                                                             (list (make-mail wrld
                                                                              "Username already in use"))
                                                             '())]
               [else (UPDATE-DATABASE (cons (second m) (third m)))
                            (make-bundle univ
                                         (list (make-mail wrld
                                                          (list "You have been successfully registered"
                                                                ;(second m)
                                                                ;(last m)
                                                                )))
                                         '())])]
        
        [(and (list? m) (equal? (car m) "LOGIN-USER"))
         (cond [(member (cons (second m) (third m)) (hash->list DATABASE)) (hash-set! ACTIVE-MEMBERS (second m) (create-account (cdr m)))
                                                                           (make-bundle univ
                                                                                        (list (make-mail wrld
                                                                                                         "You have logged-in successfully"
                                                                                                         ))
                                                                                        '())]
               [else (make-bundle univ
                                  (list (make-mail wrld
                                                   "Invalid Credentials"))
                                  '())])]

        [(and (list? m) (equal? (car m) "ADD-USER"))
         (hash-set! ACTIVE-MEMBERS (second m) (create-account (cdr m)))
         (make-bundle univ '() '())]
        
        [(and (list? m) (equal? (car m) "CONNECT-WORLD"))
         (cond [(hash-has-key? ACTIVE-MEMBERS (cadr m)) (make-bundle univ
                                                                     (list (make-mail (findf (λ (x) (equal? (iworld-name x) (cadr m))) univ)
                                                                                      (list "ANOTHER-USER-REQUESTING-CHAT"
                                                                                            (iworld-name wrld)))
                                                                           (make-mail wrld
                                                                                      (list "Chat request sent" (cadr m))))
                                                                     '())]
               [else (make-bundle univ
                                  (list (make-mail wrld
                                                   (list "User not active" (cadr m))))
                                  '())])]

        [(and (list? m) (equal? (car m) "CONNECT-MANCALA-WORLD"))
         (cond [(hash-has-key? ACTIVE-MEMBERS (cadr m))
                (cond [(member (cadr m) PLAYING-MEMBERS) (make-bundle univ
                                                                      (list (make-mail wrld
                                                                                      (list "User playing with someone else"
                                                                                            (cadr m))))
                                                                      '())]
                      [else (make-bundle univ
                                         (list (make-mail (findf (λ (x) (equal? (iworld-name x) (cadr m))) univ)
                                                          (list "ANOTHER-USER-REQUESTING-MANCALA"
                                                                (iworld-name wrld)
                                                                (third m)))
                                               (make-mail wrld
                                                          (list "Mancala request sent" (cadr m))))
                                         '())])]
                                                         
               [else (make-bundle univ
                                  (list (make-mail wrld
                                                   (list "User not active" (cadr m))))
                                  '())])]

        [(and (list? m) (equal? (car m) "RESTART-GAME"))
         (let ([deliver-to (findf (λ (x) (equal? (iworld-name x) (cadr m))) univ)])
           (cond [(not (equal? deliver-to #f))
                  (make-bundle univ
                               (list (make-mail deliver-to
                                                (list "RESTART-GAME"
                                                      (iworld-name wrld))))
                               '())]
                 [else (make-bundle univ
                                    (list (make-mail wrld
                                                     (list "OTHER-USER-DISCONNECTED" (cadr m))))
                                    '())]))]
        

        [(and (list? m) (equal? (car m) "VIEW-USER-ACCOUNT"))
         (cond [(hash-has-key? ACTIVE-MEMBERS (cadr m))
                (let* ([requested-acc (hash-ref ACTIVE-MEMBERS (cadr m))]
                       [name (send requested-acc get-name)]
                       [nationality (send requested-acc get-nationality)]
                       [age (send requested-acc get-age)]
                       [status (send requested-acc get-status)])
                  (make-bundle univ
                               (list (make-mail wrld
                                                (cons "RECEIVE-ACCOUNT"
                                                      (map (λ (x) (encrypt DEFAULT-ENCRYPTION-KEY x))
                                                           (list name nationality age status))))
                                     (make-mail (findf (λ (x) (equal? (iworld-name x) (cadr m))) univ)
                                                (list "give-your-profile-pic"
                                                      (iworld-name wrld))))
                               '()))]
               [else (make-bundle univ
                                  (list (make-mail wrld
                                                   (list "User not active" (cadr m))))
                                  '())])]

        [(and (list? m) (equal? (car m) "SEND-PROFILE-PIC"))
         (let ([deliver-to (findf (λ (x) (equal? (iworld-name x) (third m))) univ)])
           (cond [(not (equal? deliver-to #f))
                  (make-bundle univ
                               (list (make-mail deliver-to
                                                (list "RECEIVE-USER-PROFILE-PIC"
                                                      (cadr m))))
                               '())]
                 [else (make-bundle univ
                                  (list (make-mail wrld
                                                   (list "User not active" (cadr m))))
                                  '())]))]

        [(and (list? m) (equal? (car m) "CHANGE-USER-PASSWORD"))
         (let* ([user-acc (hash-ref ACTIVE-MEMBERS (iworld-name wrld))]
                [response (send user-acc change-user-password (second m) (third m))])
           (cond [(equal? response 'done) (hash-set! DATABASE (iworld-name wrld) (third m))
                                          (display-to-file (rev-format (hash->list DATABASE) "") DATABASE-PATH #:mode 'text #:exists 'replace)
                                          (make-bundle univ
                                                       (list (make-mail wrld
                                                                        "Password Successfully changed"))
                                                       '())]
                 [else (make-bundle univ
                                    (list (make-mail wrld
                                                     (list "Incorrect oldpass")))
                                    '())]))]

        [(and (list? m) (equal? (car m) "CHANGE-USER-STATUS"))
         (let ([response (send (hash-ref ACTIVE-MEMBERS (iworld-name wrld))
                               change-user-status
                               (third m)
                               (decrypt DEFAULT-ENCRYPTION-KEY (cadr m))
                               )])
           (cond [(equal? response 'done) (make-bundle univ
                                                       (list (make-mail wrld
                                                                        "Status Successfully changed"))
                                                       '())]))]

        [(and (list? m) (equal? (car m) "CHANGE-USER-DETAILS"))
         (let ([response (send (hash-ref ACTIVE-MEMBERS (iworld-name wrld))
                               change-user-details
                               (third m)
                               (cadr m)
                               )])
           (cond [(equal? response 'done) (make-bundle univ
                                                       (list (make-mail wrld
                                                                        "Details Successfully changed"))
                                                       '())]))]

        [(and (list? m) (equal? (car m) "USER-GOING-OFFLINE"))
         (hash-remove! ACTIVE-MEMBERS (iworld-name wrld))
         (cond [(equal? (cadr m) #f)
                (make-bundle univ
                             (list (make-mail wrld
                                              "You are going offline"))
                             '())]
               [else (set! CHATTING-USERS (remove wrld CHATTING-USERS (λ (x y) (or (and (equal? wrld (car y)) (equal? x (cdr y)))
                                                                              (and (equal? wrld (cdr y)) (equal? x (car y)))))))
                     (let ([deliver-to (findf (λ (x) (equal? (iworld-name x) (cadr m))) univ)])
                       (make-bundle univ
                                    (list (make-mail wrld
                                                     "You are going offline")
                                          (make-mail deliver-to
                                                     (list "OTHER-USER-DISCONNECTED"
                                                           (iworld-name wrld))))
                                    '()))])]
                
        [(and (list? m) (equal? (car m) "CHAT-REQUEST-APPROVED-BY-RECEIVER"))
         (let ([deliver-to (findf (λ (x) (equal? (iworld-name x) (cadr m))) univ)]
               [encryption-key (encryption-key-generator)])
           (if (not (equal? deliver-to #f))
               (begin (set! CHATTING-USERS (cons (cons wrld deliver-to) CHATTING-USERS))
                      (make-bundle univ
                                   (list (make-mail deliver-to
                                                (list "CHAT-REQUEST-ACCEPTED"
                                                      (iworld-name wrld)
                                                      encryption-key))
                                         (make-mail wrld
                                                    (list "YOU-APPROVED-A-CHAT-REQUEST"
                                                          encryption-key)))
                                   '()))
               (make-bundle univ
                            (list (make-mail wrld
                                             (list "User not active" (cadr m))))
                            '())))]

        [(and (list? m) (equal? (car m) "MANCALA-REQUEST-APPROVED"))
         (let* ([deliver-to (findf (λ (x) (equal? (iworld-name x) (cadr m))) univ)]
                [fmove (random 2)]
                [l (if (= fmove 0) (list #t #f) (list #f #t))])
           (cond [(member (cadr m) PLAYING-MEMBERS)
                  (make-bundle univ
                               (list (make-mail wrld
                                                (list "User playing with someone else"
                                                      (cadr m))))
                               '())]
                 [(not (equal? deliver-to #f))
                  (set! PLAYING-MEMBERS (append (list (iworld-name wrld) (cadr m))
                                                PLAYING-MEMBERS))
                  (make-bundle univ
                               (list (make-mail deliver-to
                                                (list "MANCALA-REQUEST-ACCEPTED"
                                                      (iworld-name wrld)
                                                      (car l)))
                                     (make-mail wrld
                                                    (list "YOU-APPROVED-A-MANCALA-REQUEST"
                                                          (cadr l))))
                               '())]
                 [else (make-bundle univ
                                    (list (make-mail wrld
                                                     (list "User not active" (cadr m))))
                                    '())]))]

        [(and (list? m) (equal? (car m) "MANCALA-REQUEST-REJECTED"))
         (let ([deliver-to (findf (λ (x) (equal? (iworld-name x) (cadr m))) univ)])
           (cond [(not (equal? deliver-to #f))
                  (make-bundle univ
                               (list (make-mail deliver-to
                                                (list "MANCALA-REQUEST-REJECTED-BY-USER"
                                                      (iworld-name wrld))))
                               '())]))]
                               
        [(and (list? m) (equal? (car m) "SEND-THIS-MOVE"))
         (let ([deliver-to (findf (λ (x) (equal? (iworld-name x) (third m))) univ)])
           (cond [(not (equal? deliver-to #f))
                  (make-bundle univ
                               (list (make-mail deliver-to
                                                (list "RECEIVE-MANCALA-MOVE"
                                                      (cadr m))))
                               '())]
                 [else (make-bundle univ
                                    (list (make-mail wrld
                                                     (list "OTHER-USER-DISCONNECTED"
                                                           (third m))))
                                    '())]))]

        [(and (list? m) (equal? (car m) "USER-DISCONNECTING-FROM-GAME"))
         (let ([deliver-to (findf (λ (x) (equal? (iworld-name x) (cadr m))) univ)])
           (set! PLAYING-MEMBERS (remove* (list (cadr m) (iworld-name wrld)) PLAYING-MEMBERS))
           (cond [(not (equal? deliver-to #f))
                  (make-bundle univ
                               (list (make-mail deliver-to
                                                (list "OTHER-USER-DISCONNECTED"
                                                      (iworld-name wrld))))
                               '())]))]
                  
        [(and (list? m) (equal? (car m) "CHAT-REQUEST-APPROVED-BY-RECEIVER-WHILE-CHATTING"))
         (let ([old-partner (findf (λ (x) (equal? (iworld-name x) (last m))) univ)]
               [new-partner (findf (λ (x) (equal? (iworld-name x) (cadr m))) univ)]
               [encryption-key (encryption-key-generator)])
           (cond [(and (not (equal? new-partner #f)) (not (equal? old-partner #f)))
                  (set! CHATTING-USERS (remove old-partner CHATTING-USERS (λ (x y) (or (and (equal? wrld (car y)) (equal? x (cdr y)))
                                                                                       (and (equal? wrld (cdr y)) (equal? x (car y)))))))
                  (set! CHATTING-USERS (cons (cons wrld new-partner) CHATTING-USERS))
                  (make-bundle univ
                               (list (make-mail new-partner
                                                (list "CHAT-REQUEST-ACCEPTED"
                                                      (iworld-name wrld)
                                                      encryption-key))
                                     (make-mail old-partner
                                                (list "OTHER-USER-DISCONNECTED"
                                                      (iworld-name wrld)))
                                     (make-mail wrld
                                                (list "YOU-APPROVED-A-CHAT-REQUEST"
                                                      encryption-key)))
                               '())]
                 [(equal? new-partner #f) (make-bundle univ
                                                       (list (make-mail wrld
                                                                        (list "User not active" (cadr m))))
                                                       '())]
                 [(equal? old-partner #f) (make-bundle univ
                                                       (list (make-mail wrld
                                                                        (list "User not active" (last m))))
                                                       '())]))]
        
        [(and (list? m) (equal? (car m) "CHAT-REQUEST-REJECTED-BY-RECEIVER"))
         (let ([deliver-to (findf (λ (x) (equal? (iworld-name x) (last m))) univ)])
           (if (not (equal? deliver-to #f))
               (if (equal? (cadr m) "") (make-bundle univ
                                                  (list (make-mail deliver-to
                                                                   (list "CHAT-REQUEST-REJECTED"
                                                                         "*no-reason-specified*"
                                                                         (iworld-name wrld))))
                                                  '())
                   (make-bundle univ
                                (list (make-mail deliver-to
                                                 (list "CHAT-REQUEST-REJECTED"
                                                       (cadr m)
                                                       (iworld-name wrld))))
                                '()))
               (make-bundle univ
                            (list (make-mail wrld
                                             (list "User not active" (last m))))
                            '())))]

        [(and (list? m) (equal? (car m) "SEND-THIS-MESSAGE"))
         (let ([deliver-to (findf (λ (x) (equal? (iworld-name x) (cadr m))) univ)])
           (if (not (equal? deliver-to #f))
               (make-bundle univ
                            (list (make-mail deliver-to
                                             (list "RECEIVE-MESSAGE"
                                                   (last m)
                                                   (third m)
                                                   (iworld-name wrld))))
                            '())
               (make-bundle univ
                            (list (make-mail wrld
                                             (list "User not active" (last m))))
                            '())))]

        [(and (list? m) (equal? (car m) "USER-TYPING"))
         (let ([deliver-to (findf (λ (x) (equal? (iworld-name x) (cadr m))) univ)])
           (if (not (equal? deliver-to #f))
               (make-bundle univ
                            (list (make-mail deliver-to
                                             (list "PARTNER-TYPING"
                                                   (iworld-name wrld))))
                            '())
               (make-bundle univ
                            (list (make-mail wrld
                                             (list "OTHER-USER-DISCONNECTED"
                                                   (cadr m))))
                            '())))]

        [(and (list? m) (equal? (car m) "USER-DISCONNECTED-FROM-CHAT"))
         (let ([deliver-to (findf (λ (x) (equal? (iworld-name x) (cadr m))) univ)])
           (cond [(equal? deliver-to wrld) (set! CHATTING-USERS (remove wrld CHATTING-USERS
                                                                         (λ (x y) (or (equal? x (car y))
                                                                                      (equal? x (cdr y))))))
                                            (make-bundle univ
                                                         '()
                                                         '())]
                 [(not (equal? deliver-to #f))
                  (set! CHATTING-USERS (remove wrld CHATTING-USERS
                                               (λ (x y) (or (and (equal? deliver-to (car y)) (equal? x (cdr y)))
                                                            (and (equal? deliver-to (cdr y)) (equal? x (car y)))))))
                  (make-bundle univ
                               (list (make-mail deliver-to
                                                (list "OTHER-USER-DISCONNECTED"
                                                      (iworld-name wrld))))
                               '())]))]

        [(and (list? m) (equal? (car m) "GIVE-ACTIVE-USERS"))
         (make-bundle univ
                      (list (make-mail wrld
                                       (list "RECEIVE-ACTIVE-USERS-LIST"
                                             (hash-keys ACTIVE-MEMBERS))))
                      '())]

        [(and (list? m) (equal? (car m) "GIVE-MANCALA-USERS"))
         (make-bundle univ
                      (list (make-mail wrld
                                       (list "RECEIVE-MANCALA-USERS-LIST"
                                             (hash-keys ACTIVE-MEMBERS))))
                      '())]

        [(and (list? m) (equal? (car m) "SEND-THIS-PACKET"))
        (let ([deliver-to (findf (λ (x) (equal? (iworld-name x) (cadr m))) univ)])
          (cond [(not (equal? deliver-to #f))
                 (make-bundle univ
                              (list (make-mail deliver-to
                                               (append
                                                (list "RECEIVE-PACKET"
                                                     (iworld-name wrld))
                                                     (cddr m)))
                                    (make-mail wrld
                                               (list "PACKET-SENT")))
                              '())]
                [else (make-bundle univ
                                   (list "OTHER-USER-DISCONNECTED"
                                         (iworld-name wrld))
                                   '())]))]
        
        [else (displayln (car m)) (make-bundle univ
                                               (list (make-mail wrld
                                                                (list "something-gone-wrong")))
                                               '())]))
                                                                               

(define (disco-expr univ wrld)
  (let* ([partner-element (findf (λ (x) (or (equal? wrld (car x))
                                            (equal? wrld (cdr x)))) CHATTING-USERS)]
         [partner (cond [(equal? partner-element #f) #f]
                        [(equal? (car partner-element) wrld) (cdr partner-element)]
                        [else (car partner-element)])])
    (if (not (equal? partner #f)) (begin (set! CHATTING-USERS (remove wrld CHATTING-USERS (λ (x y) (or (equal? x (car y))
                                                                                                       (equal? x (cdr y))))))
                                         (hash-remove! ACTIVE-MEMBERS (iworld-name wrld))
                                         (make-bundle (remove wrld univ)
                                                      (list (make-mail partner
                                                                       (list "OTHER-USER-DISCONNECTED"
                                                                             (iworld-name wrld))))
                                                      '()))
        (begin (hash-remove! ACTIVE-MEMBERS (iworld-name wrld))
               (make-bundle univ
                            '()
                            (list wrld))))))
        
                 
(define (start-server)
  (universe '() (on-new add-world) (on-msg switch) (on-disconnect disco-expr)))
           
(define (run-universe) (start-server))
