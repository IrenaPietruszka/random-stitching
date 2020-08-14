#lang racket

;; (c) 2020 Irena Pietruszka. See https://en.wikipedia.org/wiki/Boids

(require 2htdp/image)
(require 2htdp/universe)
(require racket/format)

(define speed-limit 1)
(define accel-limit 1)
(define sep-radius 30)           ;; separation: radius at which boids avoid others
(define coh-radius 360)          ;; cohesion:   radius at which boids approach others
(define ali-radius 360)          ;; alignment:  radius at which boids align with others
(define sep-factor 0.15)
(define coh-factor 0.1)
(define ali-factor 0.25)

(define pen (make-pen 'black 2 'solid 'round 'round))
(define canvas (rectangle 1000 1000 'solid 'white))
(define ticks 0)
(define frames (make-vector 9 (square 1000 'solid 'white)))
(define date (seconds->date (current-seconds) #t))
(define stack (make-vector 1000 #f))

(define minx 1000)
(define maxx 0)
(define miny 1000)
(define maxy 0)

(struct boid (pos speed accel))  ;; complexes interpreted as x y

(random-seed 1)

(define (flock circle)
  (let init-flock ((flock (list)) (n 0))
    (if (= n 200) (list->vector flock)
        (let try ()
          (let* ((theta (* 2 pi (random)))
                 (pos (make-polar (* 300 (sqrt (random))) theta))
                 (x (if circle (+ 500 (real-part pos))
                        (+ 250 (* 20 (modulo n 25)))))
                 (y (if circle (+ 500 (imag-part pos))
                        (+ 250 (* 60 (quotient n 25))))))
            (for
                ([b flock])
              (when (< (magnitude (- (make-rectangular x y) (boid-pos b))) 6) (try)))
            (init-flock (cons
                         (boid (make-rectangular x y)
                               (if circle (make-polar 0.1 (- theta (/ pi 2)))  ;; direction
                                   (make-rectangular 0.1 0.1))
                               0) flock) (add1 n)))))))

(define (set-clip-extent boids)
  (for ([b boids])
    (let ((pos (boid-pos b)))
      (set! minx (min minx (real-part pos)))
      (set! maxx (max maxx (real-part pos)))
      (set! miny (min miny (imag-part pos)))
      (set! maxy (max maxy (imag-part pos))))))

(set-clip-extent (flock #t))

(define (tick boids)
  (set! stack (cons boids stack))
  (for/vector ( [current boids] )
    (let-values ([(sep-force coh-force ali-force)
                  (for/fold
                   ([sep-force 0] [coh-force 0] [ali-force 0])
                   ([target boids] #:unless (eq? current target))
                    (let* ((separation (- (boid-pos current) (boid-pos target)))
                           (distance   (magnitude separation)))
                      (if (< distance sep-radius)
                          (values (+ sep-force separation) coh-force ali-force)
                          (values sep-force
                                  (if (< distance coh-radius)
                                      (+ coh-force separation)  coh-force)
                                  (if (< distance ali-radius)
                                      (+ ali-force (boid-speed target))  ali-force)))))])
      (let* ((adjust (lambda (force factor)
                       (if (zero? (magnitude force))  force
                           (/ (* force factor) (magnitude force)))))
             (accel  (+ (boid-accel current) (adjust sep-force sep-factor)))
             (accel  (- accel (adjust coh-force coh-factor)))
             (accel  (- accel (adjust ali-force ali-factor)))
             (trim   (lambda (val limit)
                       (if (> (magnitude val)  limit)
                           (* val (/ limit (magnitude val)))  val)))
             (accel  (trim accel accel-limit))
             (speed  (+ (boid-speed current) accel))
             (speed  (trim speed speed-limit))
             (pos    (+ (boid-pos current) speed)))
        (boid pos speed accel)))))

(define (render-boid b img)               ;; Boid Img -> Img
  ;; produce image with b added to img
  (let* ((ext   (make-polar 60 (angle (boid-speed b))))
         (start (- (boid-pos b) ext))
         (end   (+ (boid-pos b) ext)))
    (add-line img (real-part start) (imag-part start) (real-part end) (imag-part end) pen)))

(define current-frame #f)

(define (draw stack)
  (let ((frame (for/fold
                ([canvas canvas])
                ([b (vector-ref stack ticks)])
                 (render-boid b canvas))))
    (set! current-frame frame)
    (scale 0.5 (place-image
                (text (string-append
                       (number->string ticks) "       saved: "
                       (~a (reverse saved))) 30 "black") 250 12 frame))))

(define saved (list))

(define (show)
  (let* ((frames (for/vector ([f frames])
                   (scale 0.4 (crop (- minx 60) (- miny 60) (+ (- maxx minx) 120) (+ (- maxy miny) 120) f))))
         (image  (beside (square 450 'solid 'white)
                         (above
                          (beside (vector-ref frames 0) (vector-ref frames 1) (vector-ref frames 2))
                          (beside (vector-ref frames 3) (vector-ref frames 4) (vector-ref frames 5))
                          (beside (vector-ref frames 6) (vector-ref frames 7) (vector-ref frames 8))))))
    (newline) (newline)
    (display (reverse saved)) (newline)
    (display image) (newline)))
  
(define (save boids)
  (vector-set! frames (length saved) current-frame)
  (set! saved (cons ticks saved))
  (set-clip-extent boids)
  (show))

(define (key stack a-key)
  (cond
    [(or (key=? a-key "right") (key=? a-key ">") (key=? a-key "."))
     (vector-set! stack (add1 ticks) (tick (vector-ref stack ticks)))
     (set! ticks (add1 ticks))  stack]
    [(or (key=? a-key "left") (key=? a-key "<") (key=? a-key ","))
     (set! ticks (sub1 ticks))  stack]
    [(key=? a-key "\\")
     (vector-set! stack 0 (flock #f))
     (set! ticks 0)  stack]
    [(key=? a-key "\b")
     (set! saved (cdr saved))
     (vector-set! frames (length saved) (square 1000 'solid 'white))
     (show)  stack]
    [(or (key=? a-key "=") (key=? a-key "+"))
     (save (vector-ref stack ticks))  stack]
    [(key=? a-key "s") (stop-with stack)]
    [else stack]))

(define (run)
  (vector-set! stack 0 (flock #t))
  (big-bang stack
    (to-draw   draw)
    (on-key    key)
    (close-on-stop #t))
  
  (let* ((frames (for/vector ([f frames])
                   (crop (- minx 60) (- miny 60) (+ (- maxx minx) 120) (+ (- maxy miny) 120) f)))
         (image  (above
                  (beside (vector-ref frames 0) (vector-ref frames 1) (vector-ref frames 2))
                  (beside (vector-ref frames 3) (vector-ref frames 4) (vector-ref frames 5))
                  (beside (vector-ref frames 6) (vector-ref frames 7) (vector-ref frames 8)))))
    (save-svg-image
     image
     (string-replace (format "boids~a.svg" (reverse saved)) " " "-")))
  (let* ((frames (for/vector ([f frames])
                   (crop (- minx 60) (- miny 60) (+ (- maxx minx) 120) (+ (- maxy miny) 120) f))))
    (do ((i 0 (add1 i))) ((= i (length saved)))
      (save-svg-image
       (above (square 200 'solid 'white) (vector-ref frames i))
       (string-replace (format "boids ~a.svg" (list-ref (reverse saved) i)) " " "-")))))

(for-each display '("                                                      +\n"
                    "                                                      =  add to set   <- delete\n\n"
                    "                                                                      \\  square\n\n"
                    "                                                      <  previous     >  next\n\n"
                    "                                                      s  save and stop\n"))
(run)
