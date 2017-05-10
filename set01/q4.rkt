;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname q4) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; q4.rkt: Inserts "_" at the ith position of the given string

(require rackunit)
(require "extras.rkt")

(provide string-insert) 

;; DATA DEFINITIONS: none

;; string-insert: String NonNegInt -> String          
;; GIVEN: a string str and a number i
;; RETURNS: the string with "_" inserted at the ith position of str string
;; WHERE: i is a number between 0 and the length of str string(inclusive)

;; EXAMPLES:
;; (string-insert "HelloWorld" 5) = "Hello_World"
;; (string-insert "Dhaval" 0) = "_Dhaval"
;; (string-insert "Dhaval" 6) = "Dhaval_"

;; DESIGN STRATEGY: Combine simpler functions

(define (string-insert str i)
   (string-append (substring str 0 i) "_" (substring str i (string-length str))))

;; TESTS:
(begin-for-test
  (check-equal? (string-insert "HelloWorld" 5) "Hello_World" 
    "\"_\" should be inserted at 5th position of \"HelloWorld\"")
  (check-equal? (string-insert "Dhaval" 0) "_Dhaval"
    "\"_\" should be inserted at 0th position of \"Dhaval\"")
  (check-equal? (string-insert "Dhaval" 6) "Dhaval_"
    "\"_\" should be inserted at 6th position of \"Dhaval\""))