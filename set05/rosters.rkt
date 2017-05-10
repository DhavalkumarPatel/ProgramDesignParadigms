;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname rosters) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; rosters.rkt: Produces the class roster for each class that
;; has at least one student enrolled from the given list of
;; (student, class) pairs.

(require rackunit)
(require "extras.rkt")

(provide
 make-enrollment
 enrollment-student
 enrollment-class
 make-roster
 roster-classname
 roster-students
 roster=?
 rosterset=?
 enrollments-to-rosters)

;; check the location of file for automated testing
;; (check-location "05" "rosters.rkt")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; DATA DEFINITIONS:

;; Student is Any data type that can be compared for equality with equal?.

;; A ListOfStudent is either
;; -- empty
;; -- (cons Student ListOfStudent)

;; TEMPLATE:
;; los-fn: ListOfStudent -> ??
;; (define (los-fn los)
;;   (cond
;;     [(empty? los) ...]
;;     [else (... (first los)
;;                (los-fn (rest los)))]))

;; A SetOfStudent is a ListOfStudent.
;; INTERPRETATION: the list of students without duplication and two SetOfStudents
;; are considered equal if they have the same members.

;; examples of SetOfStudent, for testing Student is considered as a String

(define SET-OF-STUDENTS-1 (list "John" "Feng" "Amy"))
(define SET-OF-STUDENTS-2 (list "Feng" "John" "Amy"))
(define SET-OF-STUDENTS-3 (list "Kathryn" "Amy"))
(define SET-OF-STUDENTS-4 (list "Kathryn"))


;; Class is Any data type that can be compared for equality with equal?.

;; examples of Class, for testing Class is considered as a String

(define CLASS-PDP "PDP")
(define CLASS-NET "Networks")
(define CLASS-DBMS "DBMS")


(define-struct enrollment (student class))
;; An Enrollment is a (make-enrollment Student Class)
;; INTERPRETATION:
;; (make-enrollment s c) represents the assertion that student s is
;; enrolled in class c.

;; TEMPLATE:
;; enrollment-fn: Enrollment -> ??
#|
(define (enrollment-fn enrlmnt)
  (...
   (enrollment-student enrlmnt)
   (enrollment-class enrlmnt)))
|#

;; examples of Enrollment, for testing
(define ENRLMNT-JOHN-PDP (make-enrollment "John" CLASS-PDP))
(define ENRLMNT-KATH-NET (make-enrollment "Kathryn" CLASS-NET))
(define ENRLMNT-FENG-PDP (make-enrollment "Feng" CLASS-PDP))
(define ENRLMNT-AMY-PDP (make-enrollment "Amy" CLASS-PDP))
(define ENRLMNT-AMY-NET (make-enrollment "Amy" CLASS-NET))
(define ENRLMNT-KATH-DBMS (make-enrollment "Kathryn" CLASS-DBMS))
(define ENRLMNT-DHAV-DBMS (make-enrollment "Dhaval" CLASS-DBMS))

;; A ListOfEnrollment is either
;; -- empty
;; -- (cons Enrollment ListOfEnrollment)

;; TEMPLATE:
;; loe-fn: ListOfEnrollment -> ??
;; (define (loe-fn loe)
;;   (cond
;;     [(empty? loe) ...]
;;     [else (...
;;             (enrollment-fn (first loe))
;;             (loe-fn (rest loe)))]))


;; A SetOfEnrollment is a ListOfEnrollment.
;; INTERPRETATION: the list of enrollments without duplication and two SetOfEnrollments
;; are considered equal if they have the same members.

;; examples of SetOfEnrollment, for testing

(define SET-OF-ENRLMNT (list ENRLMNT-JOHN-PDP
                          ENRLMNT-KATH-NET
                          ENRLMNT-FENG-PDP
                          ENRLMNT-AMY-PDP
                          ENRLMNT-AMY-NET))


(define-struct roster (classname students))
;; A ClassRoster is a (make-roster Class SetOfStudent)
;; INTERPRETATION:
;; (make-roster c ss) represents that the students in class c are exactly
;; the students in set ss.
;; classname represents the class
;; students is the set of students enrolled in the class.

;; TEMPLATE:
;; roster-fn: ClassRoster -> ??
#|
(define (roster-fn roster)
  (...
   (roster-classname roster)
   (roster-students roster)))
|#

;; examples of ClassRoster, for testing

(define ROSTER-PDP-1 (make-roster CLASS-PDP SET-OF-STUDENTS-1))
(define ROSTER-PDP-2 (make-roster CLASS-PDP SET-OF-STUDENTS-2))
(define ROSTER-NET (make-roster CLASS-NET SET-OF-STUDENTS-3))
(define ROSTER-DBMS-1 (make-roster CLASS-DBMS SET-OF-STUDENTS-4))
(define ROSTER-DBMS-2 (make-roster CLASS-DBMS (cons "Dhaval" SET-OF-STUDENTS-4)))

;; A ListOfClassRoster is either
;; -- empty
;; -- (cons ClassRoster ListOfClassRoster)

;; TEMPLATE:
;; lor-fn: ListOfClassRoster -> ??
;; (define (lor-fn lor)
;;   (cond
;;     [(empty? lor) ...]
;;     [else (...
;;             (roster-fn (first lor))
;;             (lor-fn (rest lor)))]))


;; A SetOfClassRoster is a ListOfClassRoster.
;; INTERPRETATION: the list of rosters without duplication and two SetOfClassRosters
;; are considered equal if they have the same members.

;; examples of SetOfClassRoster, for testing

(define SET-OF-ROSTER-PDP-1-NET (list ROSTER-PDP-1 ROSTER-NET))
(define SET-OF-ROSTER-PDP-2-NET (list ROSTER-PDP-2 ROSTER-NET))
(define SET-OF-ROSTER-PDP-1-NET-DBMS-1 (list ROSTER-PDP-1 ROSTER-NET ROSTER-DBMS-1))
(define SET-OF-ROSTER-PDP-1-NET-DBMS-2 (list ROSTER-PDP-1 ROSTER-NET ROSTER-DBMS-2))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; roster=?: ClassRoster ClassRoster -> Boolean
;; RETURNS: true if the two arguments represent the same roster
;; EXAMPLES:
;; (roster=? ROSTER-PDP-1 ROSTER-PDP-1) = true
;; (roster=? ROSTER-PDP-1 ROSTER-PDP-2) = true
;; (roster=? ROSTER-PDP-1 ROSTER-DBMS) = false

;; DESIGN STRATEGY: Use template for ClassRoster on r1 and r2
(define (roster=? r1 r2)
  (and (equal? (roster-classname r1)
               (roster-classname r2))
       (set-equal? (roster-students r1)
                   (roster-students r2))))


;; set-equal?: SetOfStudent SetOfStudent -> Boolean
;; GIVEN: Two sets of students ss1 and ss2
;; RETURNS: true if ss1 and ss2 have the same members.
;; EXAMPLES: 
;; (set-equal? SET-OF-STUDENTS-1 SET-OF-STUDENTS-1) = true
;; (set-equal? SET-OF-STUDENTS-1 SET-OF-STUDENTS-2) = true
;; (set-equal? SET-OF-STUDENTS-1 SET-OF-STUDENTS-3) = false

;; DESIGN STRATEGY: Use HOF set-equal-with-compare-function? on ss1 and ss2
(define (set-equal? ss1 ss2)
  (set-equal-with-compare-function? ss1 ss2 equal?))


;; set-equal-with-compare-function?: SetOfX SetOfX (X X -> Boolean) -> Boolean
;; GIVEN: Two sets of X set1, set2 and compare function for checking equality of X
;; RETURNS: true if set1 and set2 have the same members where elements of the set are
;; compared for equality with given compare function.
;; EXAMPLES:
#|
(set-equal-with-compare-function? SET-OF-STUDENTS-1 SET-OF-STUDENTS-1 equal?) = true
(set-equal-with-compare-function? SET-OF-STUDENTS-1 SET-OF-STUDENTS-2 equal?) = true
(set-equal-with-compare-function? SET-OF-STUDENTS-1 SET-OF-STUDENTS-3 equal?) = false

(set-equal-with-compare-function?
 SET-OF-ROSTER-PDP-1-NET SET-OF-ROSTER-PDP-1-NET roster=?) = true
(set-equal-with-compare-function?
 SET-OF-ROSTER-PDP-1-NET SET-OF-ROSTER-PDP-2-NET roster=?) = true
(set-equal-with-compare-function?
 SET-OF-ROSTER-PDP-1-NET SET-OF-ROSTER-PDP-1-NET-DBMS roster=?) = false
|#

;; DESIGN STRATEGY: Use HOF sub-set? on set1 and set2
(define (set-equal-with-compare-function? set1 set2 compare-function)
  (and (sub-set? set1 set2 compare-function)
       (sub-set? set2 set1 compare-function)))


;; sub-set?: SetOfX SetOfX (X X -> Boolean) -> Boolean
;; GIVEN: Two sets of X set1, set2 and compare function for checking equality of X
;; RETURNS: true if set1 is subset of set2 where elements of the set are compared for
;; equality with given compare function.
;; EXAMPLES:
#|
(sub-set? SET-OF-STUDENTS-1 SET-OF-STUDENTS-1 equal?) = true
(sub-set? SET-OF-STUDENTS-1 SET-OF-STUDENTS-2 equal?) = true
(sub-set? SET-OF-STUDENTS-1 SET-OF-STUDENTS-3 equal?) = false

(sub-set? SET-OF-ROSTER-PDP-1-NET SET-OF-ROSTER-PDP-1-NET roster=?) = true
(sub-set? SET-OF-ROSTER-PDP-1-NET SET-OF-ROSTER-PDP-2-NET roster=?) = true
(sub-set? SET-OF-ROSTER-PDP-1-NET SET-OF-ROSTER-PDP-1-NET-DBMS roster=?) = false
|#

;; DESIGN STRATEGY: Use HOF andmap on set1
(define (sub-set? set1 set2 compare-function)
  (andmap
   ;; X -> Boolean
   ;; GIVEN: element of set1
   ;; RETURNS: true if given element is present in set2, where elements of the
   ;; set are compared for equality with compare-function.
   (lambda (set1-element)
     (is-in-set? set1-element set2 compare-function))
   set1))


;; is-in-set?: X SetOfX (X X -> Boolean) -> Boolean
;; GIVEN: element of some set of X, set of X and compare function for checking
;; equality of X
;; RETURNS: true if the given element is present in given set where elements of
;; the set are compared for equality with given compare function.
;; EXAMPLES:
#|
(is-in-set? "John" SET-OF-STUDENTS-1 equal?) = true
(is-in-set? "Kathryn" SET-OF-STUDENTS-1 equal?) = false

(is-in-set? ROSTER-NET SET-OF-ROSTER-PDP-1-NET roster=?) = true
(is-in-set? ROSTER-DBMS SET-OF-ROSTER-PDP-1-NET roster=?) = false
|#

;; DESIGN STRATEGY: Use HOF ormap on set
(define (is-in-set? element set compare-function)
  (ormap
   ;; X -> Boolean
   ;; GIVEN: element of set, set-element 
   ;; RETURNS: true if set-element and element are equal where they are compared
   ;; for equality with compare-function.
   (lambda (set-element)
     (compare-function set-element element))
   set))

;; TESTS:

(begin-for-test
  (check-true
   (roster=? ROSTER-PDP-1 ROSTER-PDP-1)
   "Rosters with same class and same set of students should be same.")
  
   (check-true
   (roster=? ROSTER-PDP-1 ROSTER-PDP-2)
   "Rosters with same class and same set of students (any order) should be same.")
   
  (check-false
   (roster=? ROSTER-PDP-1 ROSTER-DBMS-1)
   "Rosters with diffirent class or different set of students should not be same."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rosterset=?: SetOfClassRoster SetOfClassRoster -> Boolean
;; RETURNS: true if the two arguments represent the same set of rosters
;; EXAMPLES: 
;; (rosterset=? SET-OF-ROSTER-PDP-1-NET SET-OF-ROSTER-PDP-1-NET) = true
;; (rosterset=? SET-OF-ROSTER-PDP-1-NET SET-OF-ROSTER-PDP-2-NET) = true
;; (rosterset=? SET-OF-ROSTER-PDP-1-NET SET-OF-ROSTER-PDP-1-NET-DBMS) = false

;; DESIGN STRATEGY: Use HOF set-equal-with-compare-function? on rs1 and rs2
(define (rosterset=? rs1 rs2)
  (set-equal-with-compare-function? rs1 rs2 roster=?))

;; TESTS:
(begin-for-test
  (check-true
   (rosterset=? SET-OF-ROSTER-PDP-1-NET SET-OF-ROSTER-PDP-1-NET)
   "Rostersets with same class and same set of students should be same.")
  
   (check-true
   (rosterset=? SET-OF-ROSTER-PDP-1-NET SET-OF-ROSTER-PDP-2-NET)
   "Rostersets with same class and same set of students (any order) should be same.")
   
  (check-false
   (rosterset=? SET-OF-ROSTER-PDP-1-NET SET-OF-ROSTER-PDP-1-NET-DBMS-1)
   "Rostersets with diffirent class or different set of students should not be same."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; enrollments-to-rosters: SetOfEnrollment -> SetOfClassRoster
;; GIVEN: a set of enrollments
;; RETURNS: the set of class rosters for the given enrollments
;; EXAMPLES:
#|
(enrollments-to-rosters SET-OF-ENRLMNT) = SET-OF-ROSTER-PDP-1-NET
(enrollments-to-rosters SET-OF-ENRLMNT) = SET-OF-ROSTER-PDP-2-NET
|#
;; DESIGN STRATEGY: Use HOF foldr on es
(define (enrollments-to-rosters es)
  (foldr enrollemnt-to-rosters empty es))

;; enrollemnt-to-rosters: Enrollment SetOfClassRoster -> SetOfClassRoster
;; GIVEN: an enrollment enrlmnt and set of class rosters rs
;; RETURNS: the set of class rosters after adding given enrollment in rs,
;; if class is already present in rs then student added to that class else
;; class with given student will be added in rs.
;; EXAMPLES:
#|
(enrollemnt-to-rosters ENRLMNT-KATH-DBMS SET-OF-ROSTER-PDP-1-NET)
  = SET-OF-ROSTER-PDP-1-NET-DBMS-1
|#
;; DESIGN STRATEGY: Use template for Enrollment on enrlmnt
(define (enrollemnt-to-rosters enrlmnt rs)
  (if (class-in-rosters? (enrollment-class enrlmnt) rs)
      (rosters-after-adding-student enrlmnt rs)
      (rosters-after-adding-class enrlmnt rs)))

;; class-in-rosters?: Class SetOfClassRoster -> Boolean
;; GIVEN: the class and set of class rosters rs
;; RETURNS: true if the given class is already present in given class rosters.
;; EXAMPLES:
#|
(class-in-rosters? CLASS-PDP SET-OF-ROSTER-PDP-1-NET) = true
(class-in-rosters? CLASS-DBMS SET-OF-ROSTER-PDP-1-NET) = fasle
|#
;; DESIGN STRATEGY: Use HOF ormap on rs
(define (class-in-rosters? class rs)
  (ormap
   ;; ClassRoster -> Boolean
   ;; GIVEN: a class roster
   ;; RETURNS: true if classname of given roster and class are equal.
   (lambda (roster)
     (equal? (roster-classname roster) class))
   rs))

;; rosters-after-adding-student: Enrollment SetOfClassRoster -> SetOfClassRoster
;; GIVEN: an enrollment enrlmnt and set of class rosters rs
;; RETURNS: the set of class rosters after adding enrollment student in set of
;; students of matching class roster from the given list rs.
;; EXAMPLES:
#|
(rosters-after-adding-student ENRLMNT-DHAV-DBMS SET-OF-ROSTER-PDP-1-NET-DBMS-1)
  = SET-OF-ROSTER-PDP-1-NET-DBMS-2
|#
;; DESIGN STRATEGY: Use HOF map on rs
(define (rosters-after-adding-student enrlmnt rs)
  (map
   ;; ClassRoster -> ClassRoster
   ;; GIVEN: a class roster
   ;; RETURNS: a class roster after adding an enrollment student in given class roster
   ;; if enrollment and roster class are equal else return the given roster.
   (lambda (roster)
     (roster-after-adding-student enrlmnt roster))
   rs))


;; roster-after-adding-student: Enrollment ClassRoster -> ClassRoster
;; GIVEN: an enrollment enrlmnt and a class roster
;; RETURNS: the class roster after adding enrollment student in set of students
;; of given class roster if enrollment and roster class are equal else return
;; the given roster.
;; EXAMPLES:
;; (roster-after-adding-student ENRLMNT-DHAV-DBMS ROSTER-DBMS-1) = ROSTER-DBMS-2

;; DESIGN STRATEGY: Use template for Enrollment on enrlmnt and ClassRoster on roster
(define (roster-after-adding-student enrlmnt roster)
  (if (equal? (enrollment-class enrlmnt) (roster-classname roster))
      (make-roster (roster-classname roster)
                   (cons (enrollment-student enrlmnt) (roster-students roster)))
      roster))


;; rosters-after-adding-class: Enrollment SetOfClassRoster -> SetOfClassRoster
;; GIVEN: an enrollment enrlmnt and set of class rosters rs
;; RETURNS: the set of class rosters after adding new roster as per given enrollment
;; in rs.
;; EXAMPLES:
;; (rosters-after-adding-class ENRLMNT-KATH-DBMS SET-OF-ROSTER-PDP-1-NET)
;;   = SET-OF-ROSTER-PDP-1-NET-DBMS-1

;; DESIGN STRATEGY: Use template for Enrollment on enrlmnt
(define (rosters-after-adding-class enrlmnt rs)
  (cons
   (make-roster (enrollment-class enrlmnt)
                (cons (enrollment-student enrlmnt) empty))
   rs))

;; TESTS:
(begin-for-test
  (check rosterset=?
         (enrollments-to-rosters SET-OF-ENRLMNT)
         SET-OF-ROSTER-PDP-1-NET
         "Enrollments to Rosters should be equal.")
  
  (check rosterset=?
         (enrollments-to-rosters SET-OF-ENRLMNT)
         SET-OF-ROSTER-PDP-2-NET
         "Enrollments to Rosters should not be equal.")
  
  (check rosterset=?
         (enrollemnt-to-rosters ENRLMNT-KATH-DBMS SET-OF-ROSTER-PDP-1-NET)
         SET-OF-ROSTER-PDP-1-NET-DBMS-1
         "enrollemnt-to-rosters should add enrollment in set.")
  
  (check rosterset=?
         (rosters-after-adding-student ENRLMNT-DHAV-DBMS SET-OF-ROSTER-PDP-1-NET-DBMS-1)
         SET-OF-ROSTER-PDP-1-NET-DBMS-2
         "rosters-after-adding-student should add student.")
  
  (check roster=?
         (roster-after-adding-student ENRLMNT-DHAV-DBMS ROSTER-DBMS-1)
         ROSTER-DBMS-2
         "roster-after-adding-student should add student.")
  
  (check rosterset=?
         (rosters-after-adding-class ENRLMNT-KATH-DBMS SET-OF-ROSTER-PDP-1-NET)
         SET-OF-ROSTER-PDP-1-NET-DBMS-1
         "rosters-after-adding-class should add new class.")
  
  (check-true
   (class-in-rosters? CLASS-PDP SET-OF-ROSTER-PDP-1-NET)
   "class-in-rosters? should return true.")
  
  (check-false
   (class-in-rosters? CLASS-DBMS SET-OF-ROSTER-PDP-1-NET)   
   "class-in-rosters? should return false."))