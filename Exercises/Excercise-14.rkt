;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Excercise-14) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(define-struct student (id name major))
(make-student 1 "Dhaval" "CS")
(student? (make-student 1 "Dhaval" "CS"))
(student-id (make-student 1 "Dhaval" "CS"))
(student-name (make-student 1 "Dhaval" "CS"))
(student-major (make-student 1 "Dhaval" "CS"))