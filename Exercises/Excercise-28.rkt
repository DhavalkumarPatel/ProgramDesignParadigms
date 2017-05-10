;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Excercise-28) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(define (draw-string-image lst)
  (cond
    [(empty? lst) (text "" 24 "blue")]
    [else (above (text (merge-string (first lst)) 24 "blue") (draw-string-image (rest lst)))]))

(define (merge-string lst)
  (cond
    [(empty? lst) ""]
    [else (string-append (first lst) " " (merge-string (rest lst)))]))

(draw-string-image (list (list "Dhaval" "Brijesh" "Nikunj") (list "Dhaval" "Pranay" "Mohit")))