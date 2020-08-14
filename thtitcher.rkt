;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Roger's demos for Irena#1c|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; (c) 2020 Irena Pietruszka.

(require 2htdp/image)

(define (random! n)                       ;; Nat -> Nat
  ;; produce random integer in range 0..n-1
  (let ((Lehmer-modulus   2147483647)
        (Lehmer-multiplier     48271))
    (begin
      (set! *random-state* (modulo (* *random-state* Lehmer-multiplier) Lehmer-modulus))
      (modulo *random-state* n))))

(define *random-state* 48271)

(define (random-seed! n)                 ;; Nat ->
  (set! *random-state* n))

(define prev empty)

(define (random-stitches n img prev-x prev-y)
  (cond [(zero? n) img]
        [else
         (local [
                 (define (r x) (- (random! x) (/ (sub1 x) 2)))
                 (define (next p n)         ;; position step-size -> position
                   ;; produce next position from p
                   (local [ (define try (make-posn (+ (posn-x p) (r n)) (+ (posn-y p) (r n)))) ]
                     (if (member try prev)  ;; avoid previously used position
                         (next p n)
                         (begin (set! prev (cons try prev))
                                try))))
                 (define up   (next (make-posn prev-x prev-y) 9))
                 (define down (next (make-posn (posn-x up) (posn-y up)) 11))
                 (define (->c x)
                   (local [ (define c (min 1600 (max 0 (+ 800 (* 20 x))))) ]
                     (cond [ (> c 1400) (- c 400) ]
                           [ (< c 200 ) (+ c 400) ]
                           [ else c ])))
                 ]
           (random-stitches
            (sub1 n)
            (let ((upx (->c (posn-x up)))
                  (upy (->c (posn-y up)))
                  (downx (->c (posn-x down)))
                  (downy (->c (posn-y down))))
              (if (or (and (= upx downx) (or (= upx 400) (= upx 1200)))
                      (and (= upy downy) (or (= upy 400) (= upy 1200))))
                  (add-line img upx upy downx downy 'red)
                  (add-line img upx upy downx downy 'black)))
            (posn-x down) (posn-y down)))]))

(define (stitch n)
  (begin
    (display *random-state*)
    (begin (set! prev empty)
           (random-stitches n (square 1600 "solid" "white") 0 0))))

(display "enter (stitch <number>)\n")
