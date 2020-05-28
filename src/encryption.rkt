#lang racket
(require racket/base)
(require file/sha1)
(provide (all-defined-out))
(require math/number-theory)

(define (get-pq n k)
  (let ([prime-list (next-primes n (* 2 k))])
    (cons (list-ref prime-list (- k 1)) (last prime-list))))

(define (akmult x y)
  (akmult-iter x y 0))
(define (akmult-iter x y product)
  (cond [(= x 0) product]
        [(even? x) (akmult-iter (quotient x 2) (* 2 y) product)]
        [else (akmult-iter (quotient x 2) (* 2 y) (+ product y))]))

(define (get-coprime range offset n)
  (let ([candidate (+ offset (random range))])
    (if (= (gcd candidate n) 1) candidate
        (get-coprime range offset n))))

(define (encode x p q e)
  (modular-expt x e (akmult p q)))

(define (decode m p q e N)
  (let ([d (modular-inverse e (akmult (- p 1) (- q 1)))])
    (modular-expt m d N)))

(define (encode-string str)
  (let* ([char-num-list (map char->integer (string->list str))]
         [p-q (get-pq (expt 2 100) (length char-num-list))]
         [p (car p-q)]
         [q (cdr p-q)]
         [prod (akmult (- p 1) (- q 1))]
         [N (- (+ prod p q) 1)]
         [e (get-coprime 25 15 prod)])
    (list (map (λ (x) (encode x p q e)) char-num-list) e N)))

(define (decode-to-string l e N)
  (let* ([p-q (get-pq (expt 2 100) (length l))]
         [p (car p-q)]
         [q (cdr p-q)])
    (list->string (map (λ (x) (integer->char (decode x p q e N))) l))))

(define key-list (append (build-list 26 (λ (x) (integer->char (+ x 97))))
                         (build-list 26 (λ (x) (integer->char (+ x 65))))))

(define (encryption-key-generator)
  (shuffle key-list))

(define (change-form l1 l2 text)
  (foldr (λ (x y) (string-append (cond [(not (char-alphabetic? x)) (~a x)]
                                       [else (~a (list-ref l1 (index-of l2 x)))]) y)) "" (string->list text)))

(define (encrypt key plaintext)
  (change-form key key-list plaintext))

(define (decrypt key ciphertext)
  (change-form key-list key ciphertext))

(define MAX-PACKET-SIZE (* 600 1024))

(define (split-data byte-str)
  (define (split-data-helper bstr ans)
    (cond [(<= (bytes-length bstr) MAX-PACKET-SIZE)
           (cons bstr ans)]
          [else (split-data-helper (subbytes bstr MAX-PACKET-SIZE)
                                   (cons (subbytes bstr 0 MAX-PACKET-SIZE)
                                         ans))]))
  (split-data-helper byte-str '()))

(define (append-to-sexp l ans)
  (cond [(null? l) ans]
        [else (append-to-sexp (cdr l) (bytes-append ans (car l)))]))
  