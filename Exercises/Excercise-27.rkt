;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Excercise-27) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(define (draw-string-image lst)
  (text (merge-string lst) 24 "blue"))

(define (merge-string lst)
  (cond
    [(empty? lst) ""]
    [else (string-append (first lst) " " (merge-string (rest lst)))]))

(draw-string-image (list "Dhaval" "Brijesh" "Nikunj"))

(define (draw-string-image-1 lst)
  (cond
    [(empty? lst) (text "" 24 "blue")]
    [else (beside (text (first lst) 24 "blue") (text " " 24 "blue") (draw-string-image-1 (rest lst)))]))

(draw-string-image-1 (list "Dhaval" "Brijesh" "Nikunj"))