;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Excercise-32) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(define (sum-of-distance lstOfPoint)
  (cond
    [(empty? lstOfPoint) 0]
    [else (+ (distance-from-origin (first lstOfPoint)) (sum-of-distance (rest lstOfPoint)))]))

(define (distance-from-origin point)
  (sqrt (+ (expt (point-x point) 2) (expt (point-y point) 2))))

(define-struct point (x y))

(sum-of-distance (list (make-point 3 4) (make-point 3 4) (make-point 3 4)))