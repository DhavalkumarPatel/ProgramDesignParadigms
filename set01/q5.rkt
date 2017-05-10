;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname q5) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; q5.rkt: Deletes the ith position of the given string

(require rackunit)
(require "extras.rkt")

(provide string-delete) 

;; DATA DEFINITIONS: none

;; string-delete: String NonNegInt -> String          
;; GIVEN: a string str and a number i
;; RETURNS: the string with ith position deleted from str
;; WHERE: str is a non-empty string and i is a number between 0 (inclusive) and the length of str(exclusive).

;; EXAMPLES:
;; (string-delete "HelloWorld" 5) = "Helloorld"
;; (string-delete "Dhaval" 0) = "haval"
;; (string-delete "Dhaval" 5) = "Dhava"

;; DESIGN STRATEGY: Combine simpler functions

(define (string-delete str i)
   (string-append (substring str 0 i) (substring str (+ i 1) (string-length str))))

;; TESTS:
(begin-for-test
  (check-equal? (string-delete "HelloWorld" 5) "Helloorld" 
    "5th position should be deleted from \"HelloWorld\"")
  (check-equal? (string-delete "Dhaval" 0) "haval"
    "0th position should be deleted from \"Dhaval\"")
  (check-equal? (string-delete "Dhaval" 5) "Dhava"
    "5th position should be deleted from \"Dhaval\""))