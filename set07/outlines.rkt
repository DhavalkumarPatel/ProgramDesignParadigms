;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname outlines) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; outlines.rkt: converts the tree representation of a document (outline) to
;; the flat representation and also checks the legality of flat representation
;; of any outline.

(require rackunit)
(require "extras.rkt")

(provide
 legal-flat-rep?
 tree-rep-to-flat-rep)

;; check the location of file for automated testing
;; (check-location "07" "outlines.rkt")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; DATA DEFINITIONS:

(define-struct section (str secs))
;; A Section is a (make-section String ListOfSection)
;; INTERPRETATION:
;; (make-section str secs) is a section where
;;    str is the header text of the section
;;    secs is the list of subsections of the section

;; A ListOfSection is one of
;; -- empty
;; -- (cons Section ListOfSection)

;; TEMPLATE:
;; sec-fn : Section -> ??
#|
(define (sec-fn s)
  (...
   (section-str s)
   (los-fn (section-secs s))))
|#

;; los-fn : ListOfSection -> ??
#|
(define (los-fn los)
  (cond
    [(empty? los) ...]
    [else (... (sec-fn (first los))
               (los-fn (rest los)))]))
|#

;; An Outline is a ListOfSection

;; examples of Outline
(define OUTLINE-REP
  (list 
   (make-section "The first section"
                 (list
                  (make-section "A subsection with no subsections" empty)
                  (make-section "Another subsection"
                                (list
                                 (make-section "This is a subsection of 1.2" empty)
                                 (make-section "This is another subsec of 1.2" empty)))
                  (make-section "The last subsection of 1" empty)))
   (make-section "Another section"
                 (list
                  (make-section "More stuff" empty)
                  (make-section "Still more stuff" empty)))))


;; A NonEmptyListOfPosInt is one of
;; -- (cons PosInt empty)
;; -- (cons PosInt NonEmptyListOfPosInt)

;; TEMPLATE:
;; nelopi-fn : NonEmptyListOfPosInt -> ??
#|
(define (nelopi-fn ne-lst)
  (cond
    [(empty? (rest ne-lst)) (... (first ne-lst))]
    [else (...
           (first ne-lst)
           (nelopi-fn (rest ne-lst)))]))
|#


(define-struct line (sec-number str))
;; A Line is a (make-line NonEmptyListOfPosInt String)
;; INTERPRETATION:
;; (make-line sec-number str) is a line where
;;    sec-number is the non empty list of positive integers represents the section number.
;;    str is the header text of the section

;; TEMPLATE:
;; line-fn : Line -> ??
#|
(define (line-fn l)
  (...
   (nelopi-fn (line-sec-number l))
   (line-str t)))
|#


;; A ListOfLine is
;; -- empty
;; -- (cons Line ListOfLine)

;; TEMPLATE:
;; lol-fn : ListOfLine -> ??
#|
(define (lol-fn lol)
  (cond
    [(empty? lol) ...]
    [else (... (line-fn (first lol))
               (lol-fn (rest lol)))]))
|#

;; A FlatRep is a ListOfLine

;; examples of FlatRep 

(define FLAT-REP
  (list
   (make-line (list 1) "The first section")
   (make-line (list 1 1) "A subsection with no subsections")
   (make-line (list 1 2) "Another subsection")
   (make-line (list 1 2 1) "This is a subsection of 1.2")
   (make-line (list 1 2 2) "This is another subsec of 1.2")
   (make-line (list 1 3) "The last subsection of 1")
   (make-line (list 2) "Another section")
   (make-line (list 2 1) "More stuff")
   (make-line (list 2 2) "Still more stuff")))

;; A ListOfPosInt is one of
;; -- empty
;; -- (cons PosInt ListOfPosInt)

;; TEMPLATE:
;; lopi-fn : ListOfPosInt -> ??
#|
(define (lopi-fn lst)
  (cond
    [(empty? lst) ...]
    [else (...
           (first lst)
           (lopi-fn (rest lst)))]))
|#

;; CONSTANTS

(define SEC-ONE 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; legal-flat-rep? : ListOfLine -> Boolean
;; GIVEN: a list of lines
;; RETURNS: true if it is a legal flat representation of an outline.
;; EXAMPLES:
;; (legal-flat-rep? empty) = true
;; (legal-flat-rep? FLAT-REP) = true

;; DESIGN STRATEGY: : Use template for ListOfLine on lol
(define (legal-flat-rep? lol)
  (cond
    [(empty? lol) true]
    [else (and (equal? (line-sec-number (first lol)) (list SEC-ONE))
               (legal-line? (rest lol) (first lol)))]))


;; legal-line? : ListOfLine Line -> Boolean
;; GIVEN: a sublist of lines lol and a line prev-line
;; WHERE: prev-line is the line occured before the first line of lol in lol0.
;; RETURNS: true if the given sublist of line lol represents the legal flat
;;          representation of an outline.
;; EXAMPLES:
;; (legal-line? (list (make-line (list 2) "")) (make-line (list 1) "")) = true
;; (legal-line? (list (make-line (list 3) "")) (make-line (list 1) "")) = false
;; (legal-line? (list (make-line (list 1 2) "")) (make-line (list 1) "")) = false

;; DESIGN STRATEGY: Use template for ListOfLine on lol
(define (legal-line? lol prev-line)
  (cond
    [(empty? lol) true]
    [else (and (legal-section-number? (line-sec-number (first lol))
                                      (line-sec-number prev-line))
               (legal-line? (rest lol) (first lol)))]))


;; legal-section-number? : NonEmptyListOfPosInt NonEmptyListOfPosInt -> Boolean
;; GIVEN: the section numbers list of current line lon, and of previous line prev-lon
;; WHERE: lon and prev-lon are the subpart of the list lon0 and prev-lon0 respectively,
;;        and all the previous section numbers of both the list lon and prev-lon in
;;        lon0 and prev-lon0, are same. 
;; RETURNS: true if the section numbers of current line lon0 represents a legal flat
;;          representation of outline.
;; EXAMPLES:
;; (legal-section-number? (list 1 1) (list 1)) = true
;; (legal-section-number? (list 1 2) (list 1 1)) = true
;; (legal-section-number? (list 1 1 1) (list 1 1)) = true
;; (legal-section-number? (list 2) (list 1 1)) = true

;; DESIGN STRATEGY: Use template for NonEmptyListOfPosInt on lon
(define (legal-section-number? lon prev-lon)
  (cond
    [(empty? (rest lon)) (= (first lon) (add1 (first prev-lon)))]
    [else (and (= (first lon) (first prev-lon))
               (if (empty? (rest prev-lon))
                   (= (second lon) SEC-ONE)
                   (legal-section-number? (rest lon) (rest prev-lon))))]))

;; TESTS:
(begin-for-test
  (check-true
   (legal-flat-rep? empty)
   "Empty Flat representation is legal.")
  (check-true
   (legal-flat-rep? FLAT-REP)
   "Flat representation is legal."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; tree-rep-to-flat-rep : Outline -> FlatRep
;; GIVEN: the representation of an outline as a list of Sections
;; RETURNS: the flat representation of the outline
;; EXAMPLES:
;; (tree-rep-to-flat-rep OUTLINE-REP) = FLAT-REP

;; DESIGN STRATEGY: call a more general function
(define (tree-rep-to-flat-rep los)
  (sections-to-lines los empty SEC-ONE))


;; sections-to-lines : ListOfSection ListOfPosInt PosInt -> FlatRep
;; GIVEN: the list of section/subsections los, the secton number list lon and a section
;;        number sn
;; WHERE: los is the subpart of whole outline los0, lon contains the section numbers of
;;        all the parent sections of the first section of los in los0 and sn is a section
;;        number of the first section of los.
;; RETURNS: the flat representation of the list of sections los0
;; EXAMPLES:
;; (sections-to-lines (list (make-section "Another section")) empty 2)
;;      = (list (make-line (list 2) "Another section"))

;; DESIGN STRATEGY: Use template for ListOfSection on los
(define (sections-to-lines los lon sn)
  (cond
    [(empty? los) empty]
    [else (append (cons (make-line (append lon (list sn)) (section-str (first los)))
                        (sections-to-lines (section-secs (first los))
                                           (append lon (list sn))
                                           SEC-ONE))
                  (sections-to-lines (rest los) lon (add1 sn)))]))

;; TESTS:
(begin-for-test
  (check-equal?
   (tree-rep-to-flat-rep OUTLINE-REP)
   FLAT-REP
   "Flat representation of an outline should produce the same document."))