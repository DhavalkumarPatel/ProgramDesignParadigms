;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname All-Exercises) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
------------------------------------------------------------------------------
EXERCISE-1

; calulate-seconds-in-year = : Number -> Number
; Calculate seconds in year
(define (calculate-seconds-in-year days) (* days 24 60 60))
(calculate-seconds-in-year 366)

------------------------------------------------------------------------------
EXERCISE-2

(> (/ 100 3) (/ (+ 100 3) (+ 3 3)))

------------------------------------------------------------------------------
EXERCISE-3

; f->c : Real -> Real
; GIVEN: a temperature in degrees Fahrenheit as an argument
; RETURNS: the equivalent temperature in degrees Celsius.
; Examples:
; (f->c 32)  => 0
; (f->c 100) => 37.77777777777777....

(define (f->c f)
  (* (- f 32)
     (/ 5 9)))
(f->c 32)
(f->c 100)

------------------------------------------------------------------------------
EXERCISE-4

; tip : Real Real[0.0,1.0] -> Real
; GIVEN: the amount of the bill in dollars and the
; percentage of tip
; RETURNS: the amount of the tip in dollars.
; Examples:
; (tip 10 0.15)  => 1.5
; (tip 20 0.17)  => 3.4

(define (tip bill-amount tip-percentage)
  (* bill-amount tip-percentage))

(tip 10 0.15)
(tip 20 0.17)

------------------------------------------------------------------------------
EXERCISE-5

; sq : Number -> Real
; RETURNS: the square of argument.
; Examples:
; (sq 0)  => 0
; (sq 5)  => 25
; (sq -5)  => 25

(define (sq x)
  (* x x))

(sq 0+5i)
(sq 5)
(sq -5)


------------------------------------------------------------------------------
EXERCISE-6

; sq : Real Real Real -> Number
; GIVEN: Numerical coefficients of quadratic equation (a,b,c)
; RETURNS: Root of the quadratic equation
; Examples:
; (quadratic-root 1 4 4)  => -2
; (quadratic-root 1 0 -4)  => 2

(define (quadratic-root a b c)
  (/ (- (sqrt (- (* b b)
                 (* 4 a c)))
        b)
     (* 2 a)))

(quadratic-root 1 0 -4)
(quadratic-root 1 1 2)

------------------------------------------------------------------------------
EXERCISE-7

; circumference : Real -> Real
; GIVEN: the radius r of a circle 
; RETURNS: its circumference, using the formula 2 * pi * r.
; Examples:
; (circumference 1)  =>  2*pi
; (circumference 0)  =>  0

(define (circumference r) (* 2 pi r))

(circumference 1)
(circumference 0)

------------------------------------------------------------------------------
EXERCISE-8

; circle-area : Real -> Real
; GIVEN: Radius of the circle
; RETURNS: area of the circle by equation pi * r^2
; Example:
; (circle-area 1) => 1 * pi
; (circle-area 5) => 25 * pi
; (circle-area 7) => 49 * pi

(define (circle-area r) (* pi (expt r 2)))

(circle-area 1)
(circle-area 5)
(circle-area 7)

------------------------------------------------------------------------------
EXERCISE-9

(remainder 19 7)
(modulo 19 7)

(remainder -19 7)
(modulo -19 7)

(remainder 19 -7)
(modulo 19 -7)

(remainder -19 -7)
(modulo -19 -7)

(define (is-even-modulo? x)
  (= 0 (modulo x 2)))

(define (is-even-remainder? x)
  (= 0 (remainder x 2)))

(is-even-modulo? -6)
(is-even-remainder? -6)
(even? -6)

#|
REM(N, D) = N - D * (N / D)
MOD(N, D) = REM( D + REM( N, D ), D )
|#

------------------------------------------------------------------------------
EXERCISE-10

; sum-of-larger-nos : Number Number Number -> Number
; RETURNS: Sum of the two larger numbers from three arguments.
; Examples:
; (sum-of-larger-nos 5 10 15) => 25
; (sum-of-larger-nos -5 20 -50) => 15

(define (sum-of-larger-nos a b c)
  (if (> a b)
      (if(> b c)
         (+ a b)
         (+ a c))
      (if (> a c)
          (+ a b)
          (+ b c))))
  

(sum-of-larger-nos 5 10 15)
(sum-of-larger-nos -5 20 -50)

------------------------------------------------------------------------------
EXERCISE-11

(define-struct point (x y))
;; A Point is a (make-point Number Number).
;; It represents a position on the screen.
;; Interpretation:
;; x = the x-coordinate on the screen (in pixels from the left).
;; y = the y-coordinate on the screen (in pixels from the top).

; make-point : Number Number -> Point
(make-point 5 10)

; point? : Point -> boolean
(point? (make-point 5 10))

; point-x : Point -> Number
(point-x (make-point 5 10))

; point-y : Point -> Number
(point-y (make-point 5 10))

#|
(define-struct struct1 (name id age))
(struct1-name (make-struct1 "DSP" 1 25))
(struct1? (make-struct1 "DSP" 1 25))
|#

------------------------------------------------------------------------------
EXERCISE-12

(define-struct point (x y))
(make-point 5 3)
(point? 5)
(point? true)
(point? (make-point 2 1))
(point-x (make-point 8 5))
(point-y (make-point 42 15))

------------------------------------------------------------------------------
EXERCISE-13

(define-struct point (x y))
(make-point true false)
(point-x (make-point true false))

------------------------------------------------------------------------------
EXERCISE-14

(define-struct student (id name major))
(make-student 1 "Dhaval" "CS")
(student? (make-student 1 "Dhaval" "CS"))
(student-id (make-student 1 "Dhaval" "CS"))
(student-name (make-student 1 "Dhaval" "CS"))
(student-major (make-student 1 "Dhaval" "CS"))

------------------------------------------------------------------------------
EXERCISE-15

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

------------------------------------------------------------------------------
EXERCISE-16

(define image1 (bitmap "image1.png"))
(above image1 image1 image1)
(beside image1 image1 image1)

------------------------------------------------------------------------------
EXERCISE-17

(rectangle 2 4 "solid" "blue")
(rectangle 4 8 "solid" "blue")
(rectangle 8 16 "solid" "blue")
(rectangle 16 32 "solid" "blue")

------------------------------------------------------------------------------
EXERCISE-18: Give the dimensions of the next 2 rectangles in the sequence. Write down a formula that describes the n-th element in this sequence.
Write down a contract, purpose statement, examples, and definition for a function rec-sequence that takes an
argument n, where n is a number that tells the function to return the nth element in this sequence. Test the function!

(rectangle 2 4 "solid" "blue")
(rectangle 4 8 "solid" "blue")
(rectangle 8 16 "solid" "blue")
(rectangle 16 32 "solid" "blue")

------------------------------------------------------------------------------
EXERCISE-19

;; rectangle-from-proportions: PosReal PosReal -> Rectangle
;; RETURNS: a solid blue rectangle, whose width in pixels is given
;; by the first argument, and whose proportion (ratio of height to
;; width, i.e. height = width * proportion) is given by the second
;; argument.

(define (rectangle-from-proportions width proportion) (rectangle width (* width proportion) "solid" "blue"))
(rectangle-from-proportions 50 1)


------------------------------------------------------------------------------
EXERCISE-20

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

------------------------------------------------------------------------------
EXERCISE-21

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

------------------------------------------------------------------------------
EXERCISE-22

(cons 1 (cons 2 (cons 3 (cons 4 (cons 5 empty)))))

------------------------------------------------------------------------------
EXERCISE-23

(cons true (cons false (cons true (cons false (cons true empty)))))

------------------------------------------------------------------------------
EXERCISE-24

(define (multiply lst)
      (cond
        [(empty? lst) 1]
        [else (* (first lst) (multiply (rest lst)))]))

(multiply (list 1 2 3 4 5))

------------------------------------------------------------------------------
EXERCISE-25

; check-list : ListOfBoolean -> Boolean
;RETURNS: True if all booleans in the list are true.
;Examples:
;(check-list empty) => false
;(check-list (list false)) => false
;(check-list (list true)) => true
;(check-list (list false true false)) => false
;(check-list (list true false true)) => false
;(check-list (list true true true false)) => false
;(check-list (list true true true true)) => true

(define (check-list lst)
  (cond
    [(empty? lst) true]
    [(first lst) (check-list (rest lst))]
    [else false]))

(check-list empty)
(check-list (list false))
(check-list (list true))
(check-list (list false true false))
(check-list (list true false true))
(check-list (list true true true false))
(check-list (list true true true true))

------------------------------------------------------------------------------
EXERCISE-26

(define-struct point (x y))
;; A Point is a (make-point Number Number).
;; It represents a position on the screen.
;; Interpretation:
;; x = the x-coordinate on the screen (in pixels from the left).
;; y = the y-coordinate on the screen (in pixels from the top).

; check-list : ListOfPoint -> Image
;RETURNS: 300x300 scene with circles of radius 10 at all the points given in list.
(define (draw-circles lst)
  (cond
    [(empty? lst) (empty-scene 300 300)]
    [else (place-image (circle 10 "solid" "blue") (point-x (first lst)) (point-y (first lst)) (draw-circles (rest lst)))]))

(draw-circles (list (make-point 20 20) (make-point 40 40) (make-point 60 60) (make-point 80 80)))


------------------------------------------------------------------------------
EXERCISE-27

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

------------------------------------------------------------------------------
EXERCISE-28

(define (draw-string-image lst)
  (cond
    [(empty? lst) (text "" 24 "blue")]
    [else (above (text (merge-string (first lst)) 24 "blue") (draw-string-image (rest lst)))]))

(define (merge-string lst)
  (cond
    [(empty? lst) ""]
    [else (string-append (first lst) " " (merge-string (rest lst)))]))

(draw-string-image (list (list "Dhaval" "Brijesh" "Nikunj") (list "Dhaval" "Pranay" "Mohit")))

------------------------------------------------------------------------------
EXERCISE-29

(beside/align "bottom"
              (ellipse 20 70 "solid" "lightsteelblue")
              (ellipse 20 50 "solid" "mediumslateblue")
              (ellipse 20 30 "solid" "slateblue")
              (ellipse 20 10 "solid" "navy"))

------------------------------------------------------------------------------
EXERCISE-30

(define (neg-list lstOfBoolean)
  (if
   (empty? lstOfBoolean)
   empty
   (cons (not (first lstOfBoolean)) (neg-list (rest lstOfBoolean)))))

(neg-list (list true false false))

------------------------------------------------------------------------------
EXERCISE-31

(define (draw-circles lstOfNumber)
  (if
   (empty? lstOfNumber)
   empty
   (cons (circle (first lstOfNumber) "solid" "blue") (draw-circles (rest lstOfNumber)))))

(draw-circles (list 1 2 3 4 5 6))

------------------------------------------------------------------------------
EXERCISE-32

(define (sum-of-distance lstOfPoint)
  (cond
    [(empty? lstOfPoint) 0]
    [else (+ (distance-from-origin (first lstOfPoint)) (sum-of-distance (rest lstOfPoint)))]))

(define (distance-from-origin point)
  (sqrt (+ (expt (point-x point) 2) (expt (point-y point) 2))))

(define-struct point (x y))

(sum-of-distance (list (make-point 3 4) (make-point 3 4) (make-point 3 4)))