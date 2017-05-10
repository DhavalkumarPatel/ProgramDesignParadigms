;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Excercise-25) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; check-list : ListOfBoolean -> Boolean
;RETURNS: True if all booleans in the list are true.
;Examples:
;(check-list empty) => false
;(check-list (list false)) => false
;(check-list (list true)) => true
;(check-list (list false true false)) => false
;(check-list (list true false true)) => false
;(check-list (list true true true false)) => false
;(check-list (list true true true true)) => true

(define (check-list lst)
  (cond
    [(empty? lst) true]
    [(first lst) (check-list (rest lst))]
    [else false]))

(check-list empty)
(check-list (list false))
(check-list (list true))
(check-list (list false true false))
(check-list (list true false true))
(check-list (list true true true false))
(check-list (list true true true true))