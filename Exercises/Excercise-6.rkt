;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Excercise-6) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; sq : Real Real Real -> Number
; GIVEN: Numerical coefficients of quadratic equation (a,b,c)
; RETURNS: Root of the quadratic equation
; Examples:
; (quadratic-root 1 4 4)  => -2
; (quadratic-root 1 0 -4)  => 2

(define (quadratic-root a b c)
  (/ (- (sqrt (- (* b b)
                 (* 4 a c)))
        b)
     (* 2 a)))

(quadratic-root 1 0 -4)
(quadratic-root 1 1 2)