#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require rackunit rackunit/gui)

(define WORM-SIZE 10)
(define WORM-R (/ WORM-SIZE 2))

(define-struct pos (x y) #:transparent)

(define (add-pos l r)
  (pos (+ (pos-x l) (pos-x r)) (+ (pos-y l) (pos-y r))))

; seg => pos pos
(define-struct seg (start end) #:transparent)

(define-struct world (size trail key food len))

(define (trail->pos trail len)
  (let ((dots (apply append (map seg->pos trail))))
    (if (> (length dots) len)
        (take dots len)
        dots)))

; a series of point from seg start -> seg end
(define (seg->pos seg)
  (let ((start (seg-start seg))
        (end (seg-end seg)))
    (case (dir-seg seg)
      [("left") (pos-gen (step-gen (pos-x start) -1) (const-gen (pos-y start)) (seg-dist seg))]
      [("right") (pos-gen (step-gen (pos-x start) 1) (const-gen (pos-y start)) (seg-dist seg))]
      [("up") (pos-gen (const-gen (pos-x start)) (step-gen (pos-y start) -1) (seg-dist seg))]
      [("down") (pos-gen (const-gen (pos-x start)) (step-gen (pos-y start) 1) (seg-dist seg))])))

(define (const-gen v)
  (lambda (idx)
    v))

(define (step-gen start dir)
  (lambda (idx)
    (+ (* dir idx) start)))

(define (pos-gen x-gen y-gen len)
  (cond
    [(zero? len) '()]
    [else (cons (pos (x-gen (sub1 len)) (y-gen (sub1 len))) (pos-gen x-gen y-gen (sub1 len)))]))

(define (seg-dist seg)
  (let ((s (seg-start seg))
        (e (seg-end seg)))
    (abs (+ (- (pos-x e) (pos-x s)) (- (pos-y e) (pos-y s))))))

(define (draw-body-seg scene lpos)
  (cond
    [(null? lpos) scene]
    [else
     (let ((grid (grid->pos (car lpos))))
       (place-image (circle WORM-R "solid" "gray") (pos-x grid) (pos-y grid)
                    (draw-body-seg scene (cdr lpos))))]))

(define (grid->pos p)
  (pos (+ (* WORM-SIZE (pos-x p)) WORM-R) (+ (* WORM-SIZE (pos-y p)) WORM-R)))

(define (draw-worm scene trail len)
  (let ((head (seg-end (car trail))))
    (place-image (circle WORM-R "solid" "blue") (+ (* WORM-SIZE (pos-x head)) WORM-R) (+ (* WORM-SIZE (pos-y head)) WORM-R)
                 (draw-body-seg scene (trail->pos trail (sub1 len))))))

(define (draw-food scene pos)
  (if (not (null? pos))
      (place-image (square (- WORM-SIZE 2) "solid" "green") (+ (* WORM-SIZE (pos-x pos)) WORM-R) (+ (* WORM-SIZE (pos-y pos)) WORM-R) scene)
      scene))

(define (render w)
  (let ((scene (empty-scene (world-size w) (world-size w)))) 
     (draw-worm (draw-food scene (world-food w)) (world-trail w) (world-len w)))) ; worm over the food

(define (dir-pos s e)
  (cond
    [(= (pos-x s) (pos-x e))
     (if (> (pos-y e) (pos-y s))
         "down"
         "up")]
    [(= (pos-y s) (pos-y e))
     (if (> (pos-x e) (pos-x s))
         "right"
         "left")]
    [else
     (error "invalid postions" s e)]))

(define (dir-seg s)
  (dir-pos (seg-start s) (seg-end s)))

(define (extension-for-seg seg)
    (extension-for (dir-seg seg)))

; extend trail on its direction
(define (extend-trail trail)
  (let* ((head-seg (car trail))
        (ext (extension-for-seg head-seg)))
    (cons (seg (seg-start head-seg) (add-pos (seg-end head-seg) ext)) (cdr trail))))

; move along the trail
(define (tick-update w)
  (let* [(eaten? (and (not (null? (world-food w))) (pt-on-trail? (world-food w) (world-trail w) (world-len w))))
         (next-trail (if (null? (world-key w))
                         (extend-trail (world-trail w))
                         (next-trail-for-dir (world-trail w) (world-key w))))
         (next-food (if (null? (world-food w))
                        (gen-food (/ (world-size w) WORM-SIZE) (world-trail w) (world-len w))
                        (if eaten?
                           null
                           (world-food w))))
         (next-len (if eaten?
                       (add1 (world-len w))
                       (world-len w)))]
    (struct-copy world w [trail (cut-by next-trail next-len)] [key null] [food next-food] [len next-len])))

(define EXTS
  (list (cons "up" (make-pos 0 -1))
        (cons "down" (make-pos 0 1))
        (cons "left" (make-pos -1 0))
        (cons "right" (make-pos 1 0))))

; dir = key
(define (extension-for dir)
  (if (dict-has-key? EXTS dir)
      (dict-ref EXTS dir)
      (make-pos 0 0)))

; same dir or opposite dir
(define (along? d1 d2)
  (or (equal? d1 d2) (equal? (add-pos (extension-for d1) (extension-for d2)) (make-pos 0 0))))

(define (next-trail-for-dir trail dir)
  (let* ((head-seg (car trail))
         (moving-dir (dir-seg head-seg))
         (step (extension-for dir)))
    (if (along? dir moving-dir)
        trail; no turning
        (cons (seg (seg-end head-seg) (add-pos (seg-end head-seg) step)) trail))))

(define (cut-by trail len)
  (cond
    [(null? trail) '()]
    [(<= len 0) '()] ; discard the rest
    [else
     (let ((seg (car trail)))
       (cons seg (cut-by (cdr trail) (- len (seg-dist seg)))))]))

; add new seg if key is in different direction
(define (key-detect w k)
  (let ((dir (dir-seg (car (world-trail w)))))
    (if (along? dir k)
        w
        (struct-copy world w [key k]))))

; food generator
(define (gen-food size trail len)
  (let ((pt (pos (random 0 size) (random 0 size))))
    (if (pt-on-trail? pt trail len)
        (gen-food size trail len)
        pt)))

(define (pt-on-trail? pt trail len)
  (if (null? trail)
      #f
      (or (equal? pt (seg-end (car trail))) ; head overlap
          (list? (member pt (trail->pos trail len)))))) ; body overlap

; head out of screen detect
; or head onto the body
(define (end-game? w)
  (let* ((size (/ (world-size w) WORM-SIZE))
        (head (seg-end (car (world-trail w))))
        (x (pos-x head))
        (y (pos-y head)))
    (or (< x 0) (< y 0) (> x size) (> y size)
        (pt-on-trail? head (cdr (world-trail w)) (world-len w)))))       

(define INIT-SEG (seg (pos 0 0) (pos 1 0)))

; main
(big-bang (world 100 (list INIT-SEG) null null 1) ; init world - snake length 1
  [to-draw render]
  [on-tick tick-update 1] ; second
  [on-key key-detect]
  [stop-when end-game?])
