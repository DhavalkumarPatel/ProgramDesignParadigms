;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname editor) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; editor.rkt: Generates the tiny text editor

(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)

(provide
 make-editor
 editor-pre
 editor-post
 editor?
 edit) 

;; check the location of file for automated testing 
;; (check-location "02" "editor.rkt")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; DATA DEFINITIONS:

(define-struct editor (pre post))

;; An Editor is a
;; (make-editor String String)
;; INTERPRETATION:
;; (make-editor s t) means the text in the editor is
;; (string-append s t) with the cursor displayed between s and t

;; editor-fn : Editor -> ??
#|
(define (editor-fn ed)
  (...
   (editor-pre ed)
   (editor-post ed))
|#


;; A KeyEvent is a String
;; INTERPRETATION: It is one of the string from a collection KeyEvent,
;; specified as an enumeration in 2htdp/universe module


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; edit Editor KeyEvent -> Editor          
;; GIVEN: the editor ed and key event ke 
;; RETURNS: the another editor with a single-character ke added to the end
;; of the pre field of ed unless 
;; ke is a backspace ("\b") then deletes the character immediately to
;; the left of the cursor (if any),
;; if ke is a tab key ("\t") or a return key ("\r") then ignores them,
;; if ke is "left" key then moves the cursor one character to the left (if any),
;; if ke is "right" key then moves the cursor one character to the right (if any)
;; else ingnores other longer than one letter key events.

;; EXAMPLES\TESTS:
(begin-for-test
  (check-equal? 
   (edit (make-editor "abc" "def") "x")
   (make-editor "abcx" "def"))
  
  (check-equal? 
   (edit (make-editor "abc" "def") "\b")
   (make-editor "ab" "def"))
  
  (check-equal? 
   (edit (make-editor "abc" "def") "\t")
   (make-editor "abc" "def"))
  
  (check-equal? 
   (edit (make-editor "abc" "def") "\r")
   (make-editor "abc" "def"))
  
  (check-equal? 
   (edit (make-editor "abc" "def") "left")
   (make-editor "ab" "cdef"))
  
  (check-equal? 
   (edit (make-editor "abc" "def") "right")
   (make-editor "abcd" "ef"))
  
  (check-equal? 
   (edit (make-editor "abc" "def") "up")
   (make-editor "abc" "def")))

;; DESIGN STRATEGY: Cases on KeyEvent

(define (edit ed ke)
  (cond
    [(key=? ke "\t") ed]
    [(key=? ke "\r") ed]
    [(key=? ke "\b") (editor-after-backspace-key ed)]
    [(= (string-length ke) 1) (editor-after-other-single-character-key ed ke)]
    [(key=? ke "left") (editor-after-left-key ed)]
    [(key=? ke "right") (editor-after-right-key ed)]
    [else ed]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; editor-after-backspace-key Editor -> Editor          
;; GIVEN: the editor ed
;; RETURNS: the another editor after deleting the character immediately to
;; the left of the cursor (if there is any)

;; EXAMPLES\TESTS:

(begin-for-test
  (check-equal? 
   (editor-after-backspace-key (make-editor "abc" "def"))
   (make-editor "ab" "def"))
  
  (check-equal? 
   (editor-after-backspace-key (make-editor "" "def"))
   (make-editor "" "def")))

;; DESIGN STRATEGY: Use template for Editor on ed

(define (editor-after-backspace-key ed)
  (make-editor
   (string-delete-last (editor-pre ed))
   (editor-post ed)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; editor-after-other-single-character-key Editor KeyEvent -> Editor          
;; GIVEN: the editor ed and KeyEvent ke
;; RETURNS: the another editor after adding a single-character ke to the end
;; of the pre field of ed
;; WHERE: ke is a single-character key except "\t", "\r", and "\b"

;; EXAMPLES\TESTS:

(begin-for-test
  (check-equal? 
   (editor-after-other-single-character-key (make-editor "abc" "def") "a")
   (make-editor "abca" "def"))
  
  (check-equal? 
   (editor-after-other-single-character-key (make-editor "" "def") "a")
   (make-editor "a" "def"))
  
  (check-equal? 
   (editor-after-other-single-character-key (make-editor "abc" "def") " ")
   (make-editor "abc " "def")))

;; DESIGN STRATEGY: Use template for Editor on ed

(define (editor-after-other-single-character-key ed ke)
  (make-editor
   (string-append (editor-pre ed) ke)
   (editor-post ed)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; editor-after-left-key Editor -> Editor          
;; GIVEN: the editor ed
;; RETURNS: the another editor after moving the cursor one character to
;; the left (if there is any)

;; EXAMPLES\TESTS:

(begin-for-test
  (check-equal? 
   (editor-after-left-key (make-editor "abc" "def"))
   (make-editor "ab" "cdef"))
  
  (check-equal? 
   (editor-after-left-key (make-editor "a" "def"))
   (make-editor "" "adef"))
  
  (check-equal? 
   (editor-after-left-key (make-editor "" "def"))
   (make-editor "" "def")))

;; DESIGN STRATEGY: Use template for Editor on ed

(define (editor-after-left-key ed)
  (make-editor
   (string-delete-last (editor-pre ed))
   (string-append
    (string-last (editor-pre ed))
    (editor-post ed))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; editor-after-right-key Editor -> Editor          
;; GIVEN: the editor ed
;; RETURNS: the another editor after moving the cursor one character to
;; the right (if there is any)

;; EXAMPLES\TESTS:

(begin-for-test
  (check-equal? 
   (editor-after-right-key (make-editor "abc" "def"))
   (make-editor "abcd" "ef"))
  
  (check-equal? 
   (editor-after-right-key (make-editor "abc" "d"))
   (make-editor "abcd" ""))
  
  (check-equal? 
   (editor-after-right-key (make-editor "abc" ""))
   (make-editor "abc" "")))

;; DESIGN STRATEGY: Use template for Editor on ed

(define (editor-after-right-key ed)
  (make-editor
   (string-append
    (editor-pre ed)
    (string-first (editor-post ed)))
   (string-delete (editor-post ed) 0)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; string-delete-last String -> String          
;; GIVEN: the string str
;; RETURNS: the string with last position deleted from str (if there is any)

;; EXAMPLES\TESTS:

(begin-for-test
  (check-equal? 
   (string-delete-last "abc")
   "ab")
  
  (check-equal? 
   (string-delete-last "a")
   "")
  
  (check-equal? 
   (string-delete-last "")
   ""))

;; DESIGN STRATEGY: Combine simpler functions

(define (string-delete-last str)
  (if
   (> (string-length str) 0)
   (string-delete str (- (string-length str) 1))
   str))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; string-delete: String NonNegInt -> String          
;; GIVEN: a string str and a number i
;; RETURNS: the string with ith position deleted from str
;; WHERE: i is a number between 0 (inclusive) and the length of str(exclusive).

;; EXAMPLES\TESTS:

(begin-for-test
  (check-equal? 
   (string-delete "abc" 0)
   "bc")
  
  (check-equal? 
   (string-delete "abc" 2)
   "ab")
  
  (check-equal? 
   (string-delete "" 0)
   ""))

;; DESIGN STRATEGY: Combine simpler functions

(define (string-delete str ind)
  (if
   
   (> (string-length str) 0)
   
   (string-append
    (substring str 0 ind)
    (substring str (+ ind 1) (string-length str)))
   
   str))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; string-first String -> String          
;; GIVEN: the string str
;; RETURNS: the string at first position of str (if there is any)

;; EXAMPLES\TESTS:

(begin-for-test
  (check-equal? 
   (string-first "abc")
   "a")
  
  (check-equal? 
   (string-first "a")
   "a")
  
  (check-equal? 
   (string-first "")
   ""))

;; DESIGN STRATEGY: Combine simpler functions

(define (string-first str)
  (if
   (> (string-length str) 0)
   (substring str 0 1)
   ""))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; string-last String -> String          
;; GIVEN: the string str
;; RETURNS: the string at last position of str (if there is any)

;; EXAMPLES\TESTS:

(begin-for-test
  (check-equal? 
   (string-last "abc")
   "c")
  
  (check-equal? 
   (string-last "a")
   "a")
  
  (check-equal? 
   (string-last "")
   ""))

;; DESIGN STRATEGY: Combine simpler functions

(define (string-last str)
  (if
   (> (string-length str) 0)
   (substring str (- (string-length str) 1) (string-length str))
   ""))

