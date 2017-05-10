;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Excercise-9) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(remainder 19 7)
(modulo 19 7)

(remainder -19 7)
(modulo -19 7)

(remainder 19 -7)
(modulo 19 -7)

(remainder -19 -7)
(modulo -19 -7)

(define (is-even-modulo? x)
  (= 0 (modulo x 2)))

(define (is-even-remainder? x)
  (= 0 (remainder x 2)))

(is-even-modulo? -6)
(is-even-remainder? -6)
(even? -6)

#|
REM(N, D) = N - D * (N / D)
MOD(N, D) = REM( D + REM( N, D ), D )
|#