;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Excercise-10) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; sum-of-larger-nos : Number Number Number -> Number
; RETURNS: Sum of the two larger numbers from three arguments.
; Examples:
; (sum-of-larger-nos 5 10 15) => 25
; (sum-of-larger-nos -5 20 -50) => 15

(define (sum-of-larger-nos a b c)
  (if (> a b)
      (if(> b c)
         (+ a b)
         (+ a c))
      (if (> a c)
          (+ a b)
          (+ b c))))
  

(sum-of-larger-nos 5 10 15)
(sum-of-larger-nos -5 20 -50)