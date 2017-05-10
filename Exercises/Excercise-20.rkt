;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Excercise-20) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(place-image
 (circle 15 "solid" "blue") 250 15
 (place-image
  (rectangle 40 50 "solid" "blue") 250 55
  (place-image
   (rectangle 20 20 "solid" "blue") 220 40
   (place-image
    (rectangle 20 20 "solid" "blue") 280 40
    (place-image
     (rectangle 15 40 "solid" "blue") 240 100
     (place-image
      (rectangle 15 40 "solid" "blue") 260 100
      (empty-scene 500 150)))))))