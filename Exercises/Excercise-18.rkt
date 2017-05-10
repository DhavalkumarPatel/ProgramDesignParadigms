;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Excercise-18) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
Ex18: Give the dimensions of the next 2 rectangles in the sequence. Write down a formula that describes the n-th element in this sequence.
Write down a contract, purpose statement, examples, and definition for a function rec-sequence that takes an
argument n, where n is a number that tells the function to return the nth element in this sequence. Test the function!

(rectangle 2 4 "solid" "blue")
(rectangle 4 8 "solid" "blue")
(rectangle 8 16 "solid" "blue")
(rectangle 16 32 "solid" "blue")