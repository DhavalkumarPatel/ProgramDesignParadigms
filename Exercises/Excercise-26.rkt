;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Excercise-26) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(define-struct point (x y))
;; A Point is a (make-point Number Number).
;; It represents a position on the screen.
;; Interpretation:
;; x = the x-coordinate on the screen (in pixels from the left).
;; y = the y-coordinate on the screen (in pixels from the top).

; check-list : ListOfPoint -> Image
;RETURNS: 300x300 scene with circles of radius 10 at all the points given in list.
(define (draw-circles lst)
  (cond
    [(empty? lst) (empty-scene 300 300)]
    [else (place-image (circle 10 "solid" "blue") (point-x (first lst)) (point-y (first lst)) (draw-circles (rest lst)))]))

(draw-circles (list (make-point 20 20) (make-point 40 40) (make-point 60 60) (make-point 80 80)))
