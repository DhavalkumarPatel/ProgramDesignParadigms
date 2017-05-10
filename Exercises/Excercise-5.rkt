;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Excercise-5) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; sq : Number -> Real
; RETURNS: the square of argument.
; Examples:
; (sq 0)  => 0
; (sq 5)  => 25
; (sq -5)  => 25

(define (sq x)
  (* x x))

(sq 0+5i)
(sq 5)
(sq -5)