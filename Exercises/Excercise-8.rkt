;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Excercise-8) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; circle-area : Real -> Real
; GIVEN: Radius of the circle
; RETURNS: area of the circle by equation pi * r^2
; Example:
; (circle-area 1) => 1 * pi
; (circle-area 5) => 25 * pi
; (circle-area 7) => 49 * pi

(define (circle-area r) (* pi (expt r 2)))

(circle-area 1)
(circle-area 5)
(circle-area 7)