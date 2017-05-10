;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname class-lists) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; class-lists.rkt: Gives the class list of specific professor
;; from two preofessor's mixed class list. Each professor keep
;; their class lists on slips, one student on each slip.

(require rackunit)
(require "extras.rkt")

(provide
 felleisen-roster 
 shivers-roster)

;; check the location of file for automated testing
;; (check-location "04" "class-lists.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS:

;; colors of the slips
(define YELLOW "yellow")
(define BLUE "blue")


;; DATA DEFINITIONS:


(define-struct slip (color name1 name2))
;; A Slip is a (make-slip Color String String)
;; INTERPRETATION:
;; color is the color of the slip on which professors keep their class lists
;; name1 and name2 represent the first name and last name of the student
;; but in any order.

;; TEMPLATE:
;; slip-fn: Slip -> ??
#|
(define (slip-fn s)
  (...
   (slip-color s)
   (slip-name1 s)
   (slip-name2 s)))
|#

;; examples of slips, for testing

(define SLIP-OF-FELLEISEN-FL-DHAVAL (make-slip YELLOW "Dhaval" "Patel"))
(define SLIP-OF-FELLEISEN-LF-DHAVAL (make-slip YELLOW "Patel" "Dhaval"))
(define SLIP-OF-FELLEISEN-FL-BRIJESH (make-slip YELLOW "Brijesh" "Patel"))
(define SLIP-OF-FELLEISEN-LF-BRIJESH (make-slip YELLOW "Patel" "Brijesh"))

(define SLIP-OF-SHIVER-FL-DHAVAL (make-slip BLUE "Dhaval" "Patel"))
(define SLIP-OF-SHIVER-LF-DHAVAL (make-slip BLUE "Patel" "Dhaval"))
(define SLIP-OF-SHIVER-FL-BRIJESH (make-slip BLUE "Brijesh" "Patel"))
(define SLIP-OF-SHIVER-LF-BRIJESH (make-slip BLUE "Patel" "Brijesh"))


;; A Color is one of
;; -- YELLOW
;; -- BLUE
;; INTERPRETATION:
;; YELLOW is the color of paper slip on which Professor Felleisen keeps class list
;; BLUE is the color of paper slip on which Professor Shivers keeps class list

;; TEMPLATE:
;; color-fn : Color -> ??
#|
(define (color-fn c)
  (cond
    [(string=? c YELLOW) ...]
    [(string=? c BLUE) ...]))
|#


;; A ListOfSlip is either
;; -- empty
;; -- (cons Slip ListOfSlip)

;; TEMPLATE:
;; los-fn: ListOfSlip -> ??
;; (define (los-fn los)
;;   (cond
;;     [(empty? los) ...]
;;     [else (...
;;             (slip-fn (first los))
;;             (los-fn (rest los)))]))


;; example of ListOfSlip, for testing

(define MIXED-LIST
  (list
   SLIP-OF-FELLEISEN-FL-DHAVAL
   SLIP-OF-FELLEISEN-LF-DHAVAL
   SLIP-OF-FELLEISEN-FL-BRIJESH
   SLIP-OF-FELLEISEN-LF-BRIJESH
   SLIP-OF-SHIVER-FL-DHAVAL
   SLIP-OF-SHIVER-LF-DHAVAL
   SLIP-OF-SHIVER-FL-BRIJESH
   SLIP-OF-SHIVER-LF-BRIJESH))

(define FELLEISEN-LIST-WITH-DUPLICATION
  (list
   SLIP-OF-FELLEISEN-FL-DHAVAL
   SLIP-OF-FELLEISEN-LF-DHAVAL
   SLIP-OF-FELLEISEN-FL-BRIJESH
   SLIP-OF-FELLEISEN-LF-BRIJESH))

(define SHIVER-LIST-WITH-DUPLICATION
  (list
   SLIP-OF-SHIVER-FL-DHAVAL
   SLIP-OF-SHIVER-LF-DHAVAL
   SLIP-OF-SHIVER-FL-BRIJESH
   SLIP-OF-SHIVER-LF-BRIJESH))

(define FELLEISEN-LIST
  (list
   SLIP-OF-FELLEISEN-LF-DHAVAL
   SLIP-OF-FELLEISEN-LF-BRIJESH))

(define SHIVER-LIST
  (list
   SLIP-OF-SHIVER-LF-DHAVAL
   SLIP-OF-SHIVER-LF-BRIJESH))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; felleisen-roster: ListOfSlip -> ListOfSlip
;; GIVEN: a list of slips
;; RETURNS: a list of slips containing all the students in Professor
;; Felleisen's class, without duplication.
;; EXAMPLES:
;; (felleisen-roster MIXED-LIST) = FELLEISEN-LIST

;; DESIGN STRATEGY: combine simpler functions
(define (felleisen-roster los)
  (slips-without-duplication
   (slips-of-specific-professor los YELLOW)))

;; TESTS:

(begin-for-test
  (check-equal?
   (felleisen-roster MIXED-LIST)
   FELLEISEN-LIST
   "felleisen-roster should list out his students only without duplication."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; shivers-roster: ListOfSlip -> ListOfSlip
;; GIVEN: a list of slips
;; RETURNS: a list of slips containing all the students in Professor
;; Shivers' class, without duplication.
;; EXAMPLES:
;; (shivers-roster MIXED-LIST) = SHIVER-LIST

;; DESIGN STRATEGY: combine simpler functions
(define (shivers-roster los)
  (slips-without-duplication
   (slips-of-specific-professor los BLUE)))

;; TESTS:

(begin-for-test
  (check-equal?
   (shivers-roster MIXED-LIST)
   SHIVER-LIST
   "shivers-roster should list out his students only without duplication."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; slips-of-specific-professor: ListOfSlip Color -> ListOfSlip
;; GIVEN: a list of mixed slips of two professors and a color
;; RETURNS: a list of slips containing all the students of one professor
;; separated from given list of mixed slips as per the given color.
;; EXAMPLES:
;; (slips-of-specific-professor MIXED-LIST YELLOW) = FELLEISEN-LIST-WITH-DUPLICATION
;; (slips-of-specific-professor MIXED-LIST BLUE) = SHIVER-LIST-WITH-DUPLICATION

;; DESIGN STRATEGY: Use template for ListOfSlip on los
(define (slips-of-specific-professor los c)
  (cond
    [(empty? los) empty]
    [else (if (slip-of-specific-professor? (first los) c)
              (cons (first los) (slips-of-specific-professor (rest los) c))
              (slips-of-specific-professor (rest los) c))]))

;; TESTS:

(begin-for-test
  (check-equal?
   (slips-of-specific-professor MIXED-LIST YELLOW)
   FELLEISEN-LIST-WITH-DUPLICATION
   "slips-of-specific-professor should list out Felleisen's students only.")
  
  (check-equal?
   (slips-of-specific-professor MIXED-LIST BLUE)
   SHIVER-LIST-WITH-DUPLICATION
   "slips-of-specific-professor should list out Shiver's students only."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; slip-of-specific-professor?: Slip Color -> Boolean
;; GIVEN: a slip and a color 
;; RETURNS: whether the color of the given slip is same as the given color or not.
;; EXAMPLES:
;; (slip-of-specific-professor? SLIP-OF-FELLEISEN-FL-DHAVAL YELLOW) = true
;; (slip-of-specific-professor? SLIP-OF-FELLEISEN-FL-DHAVAL BLUE) = false

;; DESIGN STRATEGY: Use template for Slip on slip
(define (slip-of-specific-professor? slip color)
  (string=? (slip-color slip) color))

;; TESTS:

(begin-for-test
  (check-equal?
   (slip-of-specific-professor? SLIP-OF-FELLEISEN-FL-DHAVAL YELLOW)
   true
   "SLIP-OF-FELLEISEN-FL-DHAVAL should have a Yellow color.")
  
  (check-equal?
   (slip-of-specific-professor? SLIP-OF-FELLEISEN-FL-DHAVAL BLUE)
   false
   "SLIP-OF-FELLEISEN-FL-DHAVAL should not have a Yellow color."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; slips-without-duplication: ListOfSlip -> ListOfSlip
;; GIVEN: a list of slips of one professor
;; RETURNS: a list of slips after removing duplicated student from the given list.
;; EXAMPLES:
;; (slips-without-duplication FELLEISEN-LIST-WITH-DUPLICATION) = FELLEISEN-LIST

;; DESIGN STRATEGY: Use temlate for ListOfSlip on los
(define (slips-without-duplication los)
  (cond
    [(empty? los) empty] 
    [else (if (student-already-in-list? (first los) (rest los))                          
              (slips-without-duplication (rest los))
              (cons (first los) (slips-without-duplication (rest los))))]))

;; TESTS:
(begin-for-test
  (check-equal?
   (slips-without-duplication FELLEISEN-LIST-WITH-DUPLICATION)
   FELLEISEN-LIST
   "FELLEISEN-LIST-WITH-DUPLICATION should return FELLEISEN-LIST."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; student-already-in-list?: Slip ListOfSlip -> Boolean
;; GIVEN: a slip s and list of slips los
;; WHERE: all the slips are of any one professor
;; RETURNS: whether or not he student represented by slip s is present in the list los.
;; EXAMPLES:
;; (student-already-in-list? SLIP-OF-FELLEISEN-FL-DHAVAL FELLEISEN-LIST) = true
;; (student-already-in-list? (make-slip YELLOW "Nikunj" "Patel") FELLEISEN-LIST) = false

;; DESIGN STRATEGY: Use template for ListOfSlip on los
(define (student-already-in-list? slip los)
  (cond
    [(empty? los) false] 
    [else (if (same-student? slip (first los))
              true
              (student-already-in-list? slip (rest los)))]))

;; TESTS:

(begin-for-test
  (check-equal?
   (student-already-in-list? SLIP-OF-FELLEISEN-FL-DHAVAL FELLEISEN-LIST)
   true
   "SLIP-OF-FELLEISEN-FL-DHAVAL should be present in FELLEISEN-LIST.")
  
  (check-equal?
   (student-already-in-list? (make-slip YELLOW "Nikunj" "Patel") FELLEISEN-LIST)
   false
   "student-already-in-list? should return false if student is not present in list."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; same-student?: Slip Slip -> Boolean
;; GIVEN: two slips of any one professor
;; RETURNS: wheteher both the slips represents the same student or not
;; EXAMPLES:
;; (same-student? SLIP-OF-FELLEISEN-FL-DHAVAL SLIP-OF-FELLEISEN-FL-DHAVAL) = true
;; (same-student? SLIP-OF-FELLEISEN-FL-DHAVAL SLIP-OF-FELLEISEN-LF-DHAVAL) = true
;; (same-student? SLIP-OF-FELLEISEN-FL-DHAVAL SLIP-OF-FELLEISEN-FL-BRIJESH) = false

;; DESIGN STRATEGY: Use Template for Slip on s1 and s2
(define (same-student? s1 s2)
  (or
   (same-name? (slip-name1 s1) (slip-name2 s1)
               (slip-name1 s2) (slip-name2 s2))
   (same-name? (slip-name1 s1) (slip-name2 s1)
               (slip-name2 s2) (slip-name1 s2))))

;; TESTS:

(begin-for-test
  (check-equal?
   (same-student? SLIP-OF-FELLEISEN-FL-DHAVAL SLIP-OF-FELLEISEN-FL-DHAVAL)
   true
   "SLIP-OF-FELLEISEN-FL-DHAVAL and SLIP-OF-FELLEISEN-FL-DHAVAL are same student.")
  
  (check-equal?
   (same-student? SLIP-OF-FELLEISEN-FL-DHAVAL SLIP-OF-FELLEISEN-LF-DHAVAL)
   true
   "SLIP-OF-FELLEISEN-FL-DHAVAL and SLIP-OF-FELLEISEN-LF-DHAVAL are same student.")
  
  (check-equal?
   (same-student? SLIP-OF-FELLEISEN-FL-DHAVAL SLIP-OF-FELLEISEN-FL-BRIJESH)
   false
   "SLIP-OF-FELLEISEN-FL-DHAVAL and SLIP-OF-FELLEISEN-FL-BRIJESH are different student."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; same-name?: String String String String -> Boolean
;; GIVEN: name1 and name2 of two students
;; RETURNS: whether the names are of same student or not, name1 should match
;; with name1 of other student and same with name2
;; EXAMPLES:
;; (same-name? "Dhaval" "Patel" "Dhaval" "Patel") = true
;; (same-name? "Dhaval" "Patel" "dhaval" "patel") = false
;; (same-name? "Dhaval" "Patel" "Brijesh" "Patel") = false

;; DESIGN STRATEGY: combine simpler functions
(define (same-name? s1n1 s1n2 s2n1 s2n2)
  (and
   (string=? s1n1 s2n1)
   (string=? s1n2 s2n2)))

;; TESTS:

(begin-for-test
  (check-equal?
   (same-name? "Dhaval" "Patel" "Dhaval" "Patel")
   true
   "same-name? should return true for same names")
  
  (check-equal?
   (same-name? "Dhaval" "Patel" "dhaval" "patel")
   false
   "same-name? should return false for same names but different in case.")
  
  (check-equal?
   (same-name? "Dhaval" "Patel" "Brijesh" "Patel")
   false
   "same-name? should return false for different names."))