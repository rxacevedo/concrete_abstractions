(require (lib "fungraph.ss" "concabs"))
(load "quilting.scm")
(define half-turn (lambda (square) (quarter-turn-right (quarter-turn-right square))))
(define quarter-turn-left (lambda (square) (quarter-turn-right (half-turn square))))
(define side-by-side (lambda (square1 square2) (quarter-turn-right (stack (quarter-turn-left square2) (quarter-turn-left square1)))))
(define pinwheel (lambda (square) (stack (side-by-side (quarter-turn-right square) (half-turn square)) (side-by-side square (quarter-turn-left square)))))
(define filled-triangle2 (overlay (filled-triangle -1/2 -1/2 -1/2 1/2 1/2 -1/2) 
                                   (overlay (overlay (filled-triangle -1 1 -1/2 1/2 1 1) 
                                                     (filled-triangle -1/2 1/2 1 1 1 1/2)) 
                                            (overlay (filled-triangle 1/2 -1/2 1/2 1/2 1 1/2) 
                                                     (filled-triangle 1 -1 1/2 -1/2 1 1/2)))))

(define hourglass (overlay (filled-triangle -1 -1 0 0 1 -1) (filled-triangle -1 1 0 0 1 1)))

(define average (lambda (arg0 arg1) (/ (+ arg0 arg1) 2)))

(define foo
       (lambda (x y)
         (if (<= y 0)
             (+ x y)
             (/ x y))))

;(define power (lambda (base exponent) 
;               (if (= exponent 1)
;                  base
;                 (power (* base base) (- exponent 1)))))

(define power
  (lambda (base exponent)
    (if (= exponent 0)
        1
     (power (* base (* base (- exponent 1)))))))

(define square
         (lambda (n)
(if (= n 0) 0
               (+ (square (- n 1))
                  (- (+ n n) 1)))))