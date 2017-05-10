;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Excercise-15) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(define-struct student (id name major))
;; A Student is a (make-student Number String String).
;; It represents the student information.
;; Interpretation:
;; id = Unique ID of the student.
;; name = Name of the Student.
;; major = Major in which student is studying.

; make-student : Number String String -> Student
(make-student 1 "Dhaval" "CS")

; student? : Student -> boolean
(student? (make-student 1 "Dhaval" "CS"))

; student-id : Student -> Number
(student-id (make-student 1 "Dhaval" "CS"))

; student-name : Student -> String
(student-name (make-student 1 "Dhaval" "CS"))

; student-major : Student -> String
(student-major (make-student 1 "Dhaval" "CS"))