;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Excercise-19) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; rectangle-from-proportions: PosReal PosReal -> Rectangle
;; RETURNS: a solid blue rectangle, whose width in pixels is given
;; by the first argument, and whose proportion (ratio of height to
;; width, i.e. height = width * proportion) is given by the second
;; argument.

(define (rectangle-from-proportions width proportion) (rectangle width (* width proportion) "solid" "blue"))
(rectangle-from-proportions 50 1)

