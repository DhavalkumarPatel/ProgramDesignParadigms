;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname pretty) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; pretty.rkt: Produces a representation of the expression as a sequence of lines,
;; with each line represented as a string of length not greater than the width.

;; The expression rendered on a single line if it fits within the specified width.
;; Otherwise, it will be rendered in a stacked fashion, like
;;	(+ expr1
;;	   expr2
;;	   ...
;;	   exprN)
;; It will raise an error if the given expression cannot fit within the allotted space.

(require rackunit)
(require "extras.rkt")

(provide
 expr-to-strings
 make-sum-exp
 sum-exp-exprs
 make-diff-exp
 diff-exp-exprs)

;; check the location of file for automated testing
;; (check-location "08" "pretty.rkt")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; CONSTANTS

(define OPEN-BRCKT "(")
(define CLOSE-BRCKT ")")
(define SUM-EXPR "(+")
(define DIFF-EXPR "(-")
(define LINE-BREAKER "|")
(define EMPTY-STR "")
(define EMPTY-STR-LEN 0)
(define SPACE " ")
(define SPACE-LEN 1)
(define IS-LAST-EXPR true)
(define INIT-VAL 0)
(define ERROR-MSG "not enough room")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; DATA DEFINITIONS:

(define-struct sum-exp (exprs))
(define-struct diff-exp (exprs))

;; An Expr is one of
;; -- (make-sum-exp NELOExpr)
;; -- (make-diff-exp NELOExpr)
;; -- Integer
;; INTERPRETATION: a sum-exp represents a sum and a diff-exp represents a difference
;; calculation.

;; TEMPLATE:
;; expr-fn : Expr -> ??
#|
(define (expr-fn expr)
  (cond
    [(sum-exp? expr) ...(sum-exp-exprs expr)]
    [(diff-exp? expr) ...(dif-exp-exprs expr)]
    [(integer? expr) ...]))
|#


;; A LOExpr is one of
;; -- empty
;; -- (cons Expr LOExpr)

;; TEMPLATE:
;; loe-fn : LOExpr -> ??
#|
(define (loe-fn loe)
  (cond
    [(empty? loe) ...]
    [else (... (expr-fn (first loe))
               (loe-fn (rest lor)))]))
|#


;; A NELOExpr is a (cons Expr LOExpr)
;; INTERPRETATION: A NELOExpr is a non-empty LOExpr.

;; TEMPLATE:
;; neloe-fn : NELOExpr -> ??
#|
(define (neloe-fn neloe)
  (... (expr-fn (first neloe)) ... (loe-fn (rest neloe)) ...)
|#

;; Second Data Defination for NELOExpr

;; A NELOExpr is one of
;; -- (cons Expr empty)
;; -- (cons Expr NELOExpr)

;; TEMPLATE:
;; neloe-fn : NELOExpr -> ??
#|
(define (nelope-fn ne-lst)
  (cond
    [(empty? (rest ne-lst)) (... (first ne-lst))]
    [else (...
           (expr-fn (first ne-lst))
           (nelope-fn (rest ne-lst)))]))
|#

;; example for testing
(define EXPRS
  (make-sum-exp (list (make-diff-exp (list 22 3333 44))
                      (make-diff-exp (list (make-sum-exp (list 66 67 68))
                                           (make-diff-exp (list 42 43))))
                      (make-diff-exp (list 77 88)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; expr-to-strings : Expr NonNegInt -> ListOfString
;; GIVEN: An expression and a width
;; RETURNS: A representation of the expression as a sequence of lines, with each line
;; represented as a string of length not greater than the width.
;; EXAMPLES:
#|
(expr-to-strings EXPRS 100)
   = (list "(+ (- 22 3333 44) (- (+ 66 67 68) (- 42 43)) (- 77 88))")
|#
;; DESIGN STRATEGY: call a more general functions
(define (expr-to-strings expr width)
  (expr-strings-after-width-fitting
   (string-to-list-of-strings
    (expr-to-string-with-spaces expr EMPTY-STR-LEN SPACE-LEN IS-LAST-EXPR))
   width))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; expr-to-string-with-spaces : Expr NonNegInt NonNegInt Boolean -> String
;; GIVEN: An expression, current no of spaces and next no of spaces, is last subexpression
;;        or not.
;; WHERE: current no of spaces are the no of spaces to append before the first sub
;;        expression of expr0 and next no of spaces are no of spaces to append before
;;        all remaining sub expressions of expr0, and last-expr? is true if expr is the
;;        last sub expression of some expr0.
;; RETURNS: A representation of the expression as a single string, with "|" represents
;;          the line breaker and spaces appended as per depth of the expr in expr0.
;; EXAMPLES:
#|
(expr-to-string-with-spaces (make-sum-exp (list 10 20)) 0 1 true)
   = "(+ 10|   20)"
|#
;; DESIGN STRATEGY: Recur on every sub expression of expr
;; HALTING MEASURE: the no. of sub expressions of expr
;; TERMINATION ARGUMENT: At the recursive call, each sub expression is converted to
;; string and method is recursively called by helper function with the next sub exp.
;; So no of expression is decreased at each call. 
(define (expr-to-string-with-spaces expr nos-cur nos-next last-expr?)
  (string-append (no-of-spaces-to-string nos-cur)
                 (expr-to-string expr nos-cur nos-next)
                 (if last-expr? EMPTY-STR LINE-BREAKER)))


;; expr-to-string : Expr NonNegInt NonNegInt -> String
;; GIVEN: An sub expression, current no of spaces and next no of spaces
;; WHERE: current no of spaces are the no of spaces to append before the first sub
;;        expression of expr0 and next no of spaces are no of spaces to append before
;;        all remaining sub expressions of expr0
;; RETURNS: A representation of the sub expression as a single string, with "|" represents
;;          the line breaker and spaces appended as per depth of the sub expr in expr0.
;; EXAMPLES:
#|
(expr-to-string (make-sum-exp (list 10 20)) 0 1 )
  = "(+ 10|   20)"
|#
;; DESIGN STRATEGY: Use template for Expr on expr
(define (expr-to-string expr nos-cur nos-next)
  (cond
    [(sum-exp? expr) (sum-and-diff-expr-to-string (sum-exp-exprs expr)
                                                  SUM-EXPR nos-cur nos-next)]
    [(diff-exp? expr) (sum-and-diff-expr-to-string (diff-exp-exprs expr)
                                                   DIFF-EXPR nos-cur nos-next)]
    [(integer? expr) (number->string expr)]))


;; sum-and-diff-expr-to-string : NELOExpr String NonNegInt NonNegInt -> String
;; GIVEN: An sub expression, expression string expr-str, current no of spaces and
;;        next no of spaces
;; WHERE: expr-str is SUM-EXPR if parent expr is a sum-expr else DIFF-EXPR, current no
;;        of spaces are the no of spaces to append before the first sub expression of
;;        expr0 and next no of spaces are no of spaces to append before all remaining
;;        sub expressions of expr0
;; RETURNS: A representation of the sub expression as a single string, with "|" represents
;;          the line breaker and spaces appended as per depth of the sub expr in expr0.
;; EXAMPLES:
#|
(sum-and-diff-expr-to-string (list 22 3333 44) "(+" 0 1) = "(+ 22|   3333|   44)"
|#
;; DESIGN STRATEGY: Call a more general function
(define (sum-and-diff-expr-to-string neloe expr-str nos-cur nos-next)
  (string-append expr-str
                 (sub-expr-to-string neloe
                                     (+ nos-cur nos-next (string-length expr-str)))
                 CLOSE-BRCKT))


;; sub-expr-to-string : NELOExpr NonNegInt -> String
;; GIVEN: a non empty list of sub expressions and no of spaces to be appended
;; before the sub expression
;; RETURNS: the string representation of the given list of sub expressions
;; EXAMPLES:
#|
(sub-expr-to-string (list 22 3333 44) 3) = " 22|   3333|   44"
|#
;; DESIGN STRATEGY: Use template for NELOExpr on neloe
(define (sub-expr-to-string neloe nos)
  (string-append
   (expr-to-string-with-spaces (first neloe) SPACE-LEN nos (empty? (rest neloe)))
   (sub-exprs-to-string (rest neloe) nos)))


;; sub-exprs-to-string : LOExpr NonNegInt -> String
;; GIVEN: a list of sub expressions and no of spaces to be appended before
;; the sub expression
;; RETURNS: the string representation of the given list of sub expressions
;; EXAMPLES:
#|
(sub-expr-to-string (list 3333 44) 3) = "   3333|   44"
|#
;; DESIGN STRATEGY: Use template for LOExpr on loe
(define (sub-exprs-to-string loe nos)
  (cond
    [(empty? loe) EMPTY-STR]
    [else (string-append
           (expr-to-string-with-spaces (first loe) nos SPACE-LEN (empty? (rest loe)))
           (sub-exprs-to-string (rest loe) nos))]))


;; no-of-spaces-to-string : NonNegInt -> String
;; GIVEN: a non negative integer nos
;; RETURNS: a string after appending SPACE in empty string nos times
;; EXAMPLES:
#|
(no-of-spaces-to-string 5) = "     "
|#
;; DESIGN STRATEGY: Recur on nos
;; HALTING MEASURE: nos
;; TERMINATION ARGUMENT: At the recursive call, nos is decresed by one and
;; once it reaches to zero the recursion terminates. 
(define (no-of-spaces-to-string nos)
  (if (positive? nos)
      (string-append SPACE
                     (no-of-spaces-to-string (sub1 nos)))
      EMPTY-STR))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; string-to-list-of-strings : String -> ListOfString
;; GIVEN: a string with line breakers which represents the expression
;; RETURNS: a list of sub strings after breaking the string at each line breaker 
;; EXAMPLES:
#|
(string-to-list-of-strings "(+ 10|   20") = (list "(+ 10" "   20")
|#
;; DESIGN STRATEGY: call a more general function
(define (string-to-list-of-strings expr-str)
  (list-of-chars-to-list-of-strings (explode expr-str) empty EMPTY-STR))


;; list-of-chars-to-list-of-strings : ListOfString ListOfString String -> ListOfString
;; GIVEN: a list of 1Strings loc, a list of string los and a string str
;; WHERE: str contains all 1Strings from last line breaker of loc0 to loc and los
;;        contains all this type of str before the last line breaker of loc0 before loc
;; RETURNS: a list of strings after merging the 1Strings of given los according to
;;          the line breaker
;; EXAMPLES:
#|
(list-of-chars-to-list-of-strings (explode "(+ 10|   20)") empty "")
   = (list "(+ 10" "   20)")
|#
;; DESIGN STRATEGY: Use template for ListOfString on loc
(define (list-of-chars-to-list-of-strings loc los str)
  (cond
    [(empty? loc) (append los (list str))]
    [else (if (string=? (first loc) LINE-BREAKER)
              (list-of-chars-to-list-of-strings (rest loc) (append los (list str))
                                                EMPTY-STR) 
              (list-of-chars-to-list-of-strings (rest loc) los
                                                (string-append str (first loc))))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; expr-strings-after-width-fitting : ListOfString NonNegInt -> ListOfString
;; GIVEN: a list of string which represents the expression and a width
;; RETURNS: Either a list of string after merging possible substrings of los
;; which fits into given width or an error if the given representation of the
;; expression cannot fit within the allotted space
;; EXAMPLES:
#|
(expr-strings-after-width-fitting (list "(+ 10" "   20)") 15) = (list "(+ 10   20)")
|#
;; DESIGN STRATEGY: Cases on weather the representation of expression fits into allotted
;;                  space or not
(define (expr-strings-after-width-fitting los width)
  (if (> (length-of-longest-string los) width)
      (error ERROR-MSG)
      (expr-strings-after-merge los
                                  (no-of-exprs-in-expr-strings los)
                                  INIT-VAL
                                  width)))


;; length-of-longest-string : ListOfString -> NonNegInt
;; GIVEN: a list of strings los
;; RETURNS: a length of the longest string in los
;; EXAMPLES: 
#|
(length-of-longest-string (list "(+ 10" "   200)")) = 6
|#
;; DESIGN STRATEGY: Use HOF folr on los
(define (length-of-longest-string los)
  (foldr
   ;; String NonNegInt -> NonNegInt
   ;; GIVEN: a string and maximum length
   ;; RETURNS: a maximum length from given string's length and given max-len
   (lambda (str max-len)
     (max (string-length str) max-len))
   INIT-VAL
   los))


;; no-of-exprs-in-expr-strings : ListOfString -> NonNegInt
;; GIVEN: a list of string which represents the expression
;; RETURNS: no of expressions present in los by counting starting bracket
;; EXAMPLES:
#|
(no-of-exprs-in-expr-strings (list "(+ 10")) = 1
|#
;; DESIGN STRATEGY: Use HOF foldr on los
(define (no-of-exprs-in-expr-strings los)
  (foldr
   ;; String NonNegInt -> NonNegInt
   ;; GIVEN: a string and count
   ;; RETURNS: no of times the OPEN-BRCKT occured in str
   (lambda (str count) (+ (count-of-substring str OPEN-BRCKT)
                          count))
   INIT-VAL
   los))


;; count-of-substring : String String -> NonNegInt
;; GIVEN: a string str and a string sub-str of length one
;; RETURNS: no of times the sub-str occured in str
;; EXAMPLES:
#|
(count-of-substring "(+ 10" "(") = 1
|#
;; DESIGN STRATEGY: Use HOF foldr on (explode str)
(define (count-of-substring str sub-str)
  (foldr
   ;; String NonNegInt -> NonNegInt
   ;; GIVEN: a string and a count
   ;; RETURNS: no of times the sub-str occured in str
   (lambda (str-in-list count)
     (if (string=? str-in-list sub-str)
         (add1 count)
         count))
   INIT-VAL
   (explode str)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; expr-strings-after-merge : ListOfString NonNegInt NonNegInt NonNegInt -> ListOfString
;; GIVEN: a list of string, no of total expressions represented as a string in los, no of
;; expression already merged and a width
;; WHERE: noe is greater or equal to noe-merged
;; RETURNS: a list of string after merging all possible expressions which fits in the
;; width from los
;; EXAMPLES:
#|
(expr-strings-after-merge (list "(+ 10" "   20)") 1 0 10) = (list "(+ 10 20)")
|#
;; DESIGN STRATEGY: Recur on non merged no of expressions in los (noe - noe-merged)
;; HALTING MEASURE: (noe - noe-merged)
;; TERMINATION ARGUMENT: At the recursive call, noe-merged is increased by one so
;; (noe - noe-merged) will decreases and it terminates once both become equal. 
(define (expr-strings-after-merge los noe noe-merged width)
  (cond
    [(= noe noe-merged) los]
    [else (expr-strings-after-merge
           (expr-strings-merge los los noe-merged width false false
                               INIT-VAL INIT-VAL INIT-VAL INIT-VAL INIT-VAL)
           noe
           (add1 noe-merged)
           width)]))


;; expr-strings-merge : ListOfString ListOfString NonNegInt NonNegInt Boolean Boolean
;;                      NonNegInt NonNegInt NonNegInt NonNegInt NonNegInt -> ListOfString
;; GIVEN: a list of string los, a sublist of los los-rec, no of expression already merged
;;        noep, width, first index found or not first?, last index found or not last?,
;;        first & last index, current index, no of expressions noe and no of open brackets
;; WHERE: los-rec is curr sublist of los, first and last index contains the index of los
;;        if first? and last? is true in the range of [0 (length los)), no of open bracket
;;        are the open brackets found in the string occured before los-rec in los. 
;; RETURNS: a list of string after merging the first non merged expression represented as
;;          a string in los
;; EXAMPLES:
#|
(expr-strings-merge (list "(+ 10" "   20)") (list "(+ 10" "   20)") 0 10 false false
                    INIT-VAL INIT-VAL INIT-VAL INIT-VAL INIT-VAL)
   = (list "(+ 10 20)")
|#
;; DESIGN STRATEGY: Cases on wether the first and last index found or not and then call
;;                  more general functions
(define (expr-strings-merge los los-rec noep width first? last? fst lst curr noe opbrckt)
  (local
    ((define ropbrckt (count-of-substring (first los-rec) OPEN-BRCKT))
     (define rclbrckt (count-of-substring (first los-rec) CLOSE-BRCKT)))
    (cond
      [(and first? last?) (expr-merge los fst lst width)]
      [(not first?) (first-expr-for-merging los los-rec noep width first? last?
                                            fst lst curr (+ ropbrckt noe) rclbrckt)]
      [else (last-expr-for-merging los los-rec noep width first? last?
                                   fst lst curr (+ opbrckt ropbrckt) rclbrckt)])))


;; first-expr-for-merging : ListOfString ListOfString NonNegInt NonNegInt Boolean Boolean
;;                      NonNegInt NonNegInt NonNegInt NonNegInt NonNegInt -> ListOfString
;; GIVEN: a list of string los, a sublist of los los-rec, no of expression already merged
;;        noep, width, first index found or not first?, last index found or not last?,
;;        first & last index, current index, no of total open brackets and no of close
;;        brackets
;; WHERE: los-rec is curr sublist of los, first and last index contains the index of los
;;        if first? and last? is true in the range of [0 (length los)), no of open bracket
;;        are the open brackets found in the string occured before los-rec in los. 
;; RETURNS: the list of string after merging first non merged expression in los
;; EXAMPLES:
#|
(first-expr-for-merging (list "(+ 10" "   20)") (list "(+ 10" "   20)") 0 10 false false
                        0 0 0 1 0)
   = (list "(+ 10 20)")
|#
;; DESIGN STRATEGY: Cases on wether the curr index contains the non merged expression
;;                  or not
(define (first-expr-for-merging los los-rec noep width first? last?
                                fst lst curr opbrckt rclbrckt)
  (if (> opbrckt noep)
      (if (<= (- opbrckt noep) rclbrckt)
          los
          (expr-strings-merge los (rest los-rec) noep width true last? curr lst
                              (add1 curr) INIT-VAL (- opbrckt noep rclbrckt)))
      (expr-strings-merge los (rest los-rec) noep width first? last? fst lst
                          (add1 curr) opbrckt INIT-VAL)))


;; last-expr-for-merging : ListOfString ListOfString NonNegInt NonNegInt Boolean Boolean
;;                      NonNegInt NonNegInt NonNegInt NonNegInt NonNegInt -> ListOfString
;; GIVEN: a list of string los, a sublist of los los-rec, no of expression already merged
;;        noep, width, first index found or not first?, last index found or not last?,
;;        first & last index, current index, no of total open brackets and no of close
;;        brackets
;; WHERE: los-rec is curr sublist of los, first and last index contains the index of los
;;        if first? and last? is true in the range of [0 (length los)), no of open bracket
;;        are the open brackets found in the string occured before los-rec in los. 
;; RETURNS: the list of string after merging first non merged expression in los
;; EXAMPLES:
#|
(last-expr-for-merging (list "(+ 10" "   20)") (list "   20)") 0 10 true false
                        0 0 1 1 1)
  = (list "(+ 10 20)")
|#
;; DESIGN STRATEGY: Cases on wether the first non merged expression ends at curr index
;;                  or not
(define (last-expr-for-merging los los-rec noep width first? last?
                               fst lst curr opbrckt rclbrckt)
  (if (<= opbrckt rclbrckt)
      (expr-strings-merge los los-rec noep width first? true fst curr
                          curr INIT-VAL INIT-VAL)
      (expr-strings-merge los (rest los-rec) noep width first? last? fst lst
                          (add1 curr) INIT-VAL (- opbrckt rclbrckt))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; expr-merge : ListOfString NonNegInt NonNegInt NonNegInt -> ListOfString
;; GIVEN: a list of string los, start and index for taking the sub list from los and
;;        a width
;; RETURNS: a list of string like given one but sublist of los from start to end index
;;          merged if it fits within given width and this combined string replaces the
;;          sublist in los.
;; EXAMPLES:
#|
(expr-merge (list "(+ 10" "   20)") 0 1 10) = (list "(+ 10 20)")
|#
;; DESIGN STRATEGY: call a more general functions
(define (expr-merge los fst lst width)
  (append (list-of-string-from-index INIT-VAL fst los empty INIT-VAL)
          (sub-expr-merge (list-of-string-from-index fst (add1 lst) los empty INIT-VAL)
                          width)
          (list-of-string-from-index (add1 lst) (length los) los empty INIT-VAL)))


;; list-of-string-from-index : NonNegInt NonNegInt ListOfString ListOfString
;;                             NonNegInt -> ListOfString
;; GIVEN: a start index and an end index, a list of string los, an output list of
;;        string oplos, and current position curr
;; WHERE: los is the curr sublist of some list los0 and oplos contains the elements
;;        of los0 from st-ind to curr position if st-ind is less than curr
;; AND: st-ind end-ind, curr is in the range [0, (length los))
;; RETURNS: the sublist of los0, starting from the element at st-ind to the element at
;;          end-ind in los0 
;; EXAMPLES:
#|
(list-of-string-from-index 1 2 (list "11" "22" "33" "44") empty 0) = (list "22" "33")
|#
;; DESIGN STRATEGY: Recur on (end-ind - curr)
;; HALTING MEASURE: (end-ind - curr)
;; TERMINATION ARGUMENT: At the recursive call, curr is increased by one so
;; (end-ind - curr) is decresed and it terminates once (end-ind - curr) reaches to 0.
(define (list-of-string-from-index st-ind end-ind los oplos curr)
  (cond
    [(= st-ind end-ind) empty]
    [(and (>= curr st-ind) (< curr end-ind))
     (list-of-string-from-index st-ind end-ind (rest los)
                                 (append oplos (list (first los))) (add1 curr))]
    [(< curr st-ind)
     (list-of-string-from-index st-ind end-ind (rest los) oplos (add1 curr))]
    [(>= curr end-ind) oplos]))


;; sub-expr-merge : ListOfString NonNegInt -> ListOfString
;; GIVEN: a list of string los and a width
;; RETURNS: a one string after appending all strings of los starting from first to last
;;          if it fits into width else returns los
;; EXAMPLES:
#|
(sub-expr-merge (list "(+ 10" "   20)") 10) = (list "(+ 10 20)")
|#
;; DESIGN STRATEGY: Cases on weather merged string fits into width or not
(define (sub-expr-merge los width)
  (local
    ((define str (string-implode (cons (first los)
                                       (strings-left-space-removal (rest los))))))
    (if (< (string-length str) width)
        (list str)
        los)))


;; string-implode : ListOfString -> String
;; GIVEN: a list of string los
;; RETURNS: a one string after appending all strings of los starting from first to last
;; EXAMPLES:
#|
(string-implode (list "abc" " def" "geh")) = "abc defgeh"
|#
;; DESIGN STRATEGY: Use HOF foldl on los
(define (string-implode los)
  (foldl
   ;; String String -> String
   ;; GIVEN: tow strings
   ;; RETURNS: a string after appending given strings 
   (lambda (str appended-str) (string-append appended-str str))
   EMPTY-STR
   los))


;; strings-left-space-removal : ListOfString -> ListOfString
;; GIVEN: a list of strings los
;; RETURNS: a list of string after removing the left spaces from each string of los 
;; EXAMPLES:
#|
(strings-left-space-removal (list "(+ 10" "   20)")) = (list " (+ 10" " 20)")
|#
;; DESIGN STRATEGY: Use HOF map on los
(define (strings-left-space-removal los)
  (map
   ;; String -> String
   ;; GIVEN: a string
   ;; RETURNS: a string after removing the left spaces from the given string
   (lambda (str) (string-left-space-removal (explode str) EMPTY-STR true))
   los))


;; string-left-space-removal : ListOfString String Boolean -> String
;; GIVEN: a list of 1strings los, string str and boolean value is-remove?
;; WHERE: str represents all non space 1strings occured in los0 and is-remove? represents
;;        the space value in los needs to remove or not.
;; RETURNS: the string after appending the 1strings of los starting from first to last 
;;          and 1strings before the first non space element in los are ignored.
;; EXAMPLES:
#|
(string-left-space-removal (list " " " " "d" " " "a") "" true) = " d a"
|#
;; DESIGN STRATEGY: Use template for ListofString on los
(define (string-left-space-removal los str is-remove?)
  (cond
    [(empty? los) str]
    [else (if (and (string=? (first los) SPACE) is-remove?)
              (string-left-space-removal (rest los) str is-remove?)
              (string-left-space-removal (rest los)
                                         (string-append str
                                                        (if is-remove? SPACE EMPTY-STR)
                                                        (first los))
                                         false))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; TESTS
(begin-for-test
  (check-equal?
   (expr-to-strings EXPRS 100)
   (list "(+ (- 22 3333 44) (- (+ 66 67 68) (- 42 43)) (- 77 88))")
   "Expression should be rendered in same line")

  (check-equal?
   (expr-to-strings EXPRS 15)
   (list "(+ (- 22" "      3333" "      44)" "   (- (+ 66" "         67" "         68)"
         "      (- 42" "         43))" "   (- 77 88))")
   "Expression should be rendered in different line")

  (check-error
   (expr-to-strings EXPRS 5)
   "Not enough room error generated."))