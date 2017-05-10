;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname q2) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; q2.rkt: Extracts the first 1String from a non-empty string

(require rackunit)
(require "extras.rkt")

(provide string-first) 

;; DATA DEFINITIONS: none

;; string-first: String -> String          
;; GIVEN: the string str
;; RETURNS: the first 1String from str
;; WHERE: str is not empty

;; EXAMPLES:
;; (string-first "I am here") = "I"
;; (string-first " Hello") = " "
;; (string-first "\tHello") = "\t"

;; DESIGN STRATEGY: Combine simpler functions

(define (string-first str)
  (string-ith str 0))

;; TESTS:
(begin-for-test
  (check-equal? (string-first "I am here") "I" 
    "The first 1String of \"I am here\" should be \"I\"")
  (check-equal? (string-first " Hello") " " 
    "The first 1String of \" Hello\" should be \"H\"")
  (check-equal? (string-first "\tHello") "\t" 
    "The first 1String of \"\tHello\" should be \"\t\""))