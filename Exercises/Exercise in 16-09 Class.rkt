;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname dsp) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(define-struct suitcase (length hight width))
;; A suitcase is a (make-suitcase PosReal PosReal PosReal)

;; INTERPRETATION:
;; length representes the length of the suitcase in cm
;; height representes the height of the suitcase in cm
;; width representes the width of the suitcase in cm

;; another way to write an interpretation:
;; (make-suitcase l h w) represents a suitcase
;; with length l
;; with height h
;; and with width w (all in cm).


;;suitcase-fn : Suitcase -> ??
#;(define (suitcase-fn su)
  (... (suitcase-length su)
       (suitcase-height su)
       (suitcase-width su)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct scanner (height width))

;; A Scanner is a (make-scanner PosReal PosReal)

;; INTERPRETATION:
;; (make-scanner h w) represents a scanner
;; with height h
;; and width w (in cm).

;;scanner-fn : Scanner -> ??
#;(define (scanner-fn sc)
  (... (scanner-height sc)
       (scanner-width sc)))

;; EXAMPLES?TEST
(check-equal?
 (suitcase-first-scanner
  (make-suitcase 12 35 24)
  (make-suitcase 15 23))
 false))

;; STRATEGY: Use template for Suitcase on su
(deifne (suitcase-fits-scanner))