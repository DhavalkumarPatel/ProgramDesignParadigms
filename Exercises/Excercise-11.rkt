;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Excercise-11) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(define-struct point (x y))
;; A Point is a (make-point Number Number).
;; It represents a position on the screen.
;; Interpretation:
;; x = the x-coordinate on the screen (in pixels from the left).
;; y = the y-coordinate on the screen (in pixels from the top).

; make-point : Number Number -> Point
(make-point 5 10)

; point? : Point -> boolean
(point? (make-point 5 10))

; point-x : Point -> Number
(point-x (make-point 5 10))

; point-y : Point -> Number
(point-y (make-point 5 10))

#|
(define-struct struct1 (name id age))
(struct1-name (make-struct1 "DSP" 1 25))
(struct1? (make-struct1 "DSP" 1 25))
|#