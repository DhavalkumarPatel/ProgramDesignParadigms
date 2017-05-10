;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Excercise-21) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(define-struct person (first-name last-name age height weight))

(define (person-image person)
  (place-image
   (circle (/ (person-height person) 10) "solid" "blue")
   (/ (person-height person) 4)
   (/ (person-height person) 10)
   (place-image
     (rectangle (/ (person-height person) 3.33) (/ (person-height person) 2.5) "solid" "blue")
     (/ (person-height person) 4)
     (/ (person-height person) 2.5)
     (place-image
      (rectangle (/ (person-height person) 10) (/ (person-height person) 2.5) "solid" "blue")
      (/ (person-height person) 5)
      (/ (person-height person) 1.25)
      (empty-scene (/ (person-height person) 2) (person-height person))))))

(place-image (text "Dhaval Patel" 24 "olive") 50 50 (empty-scene 100 100))












(person-image (make-person "Dhaval" "Patel" 25 500 100))