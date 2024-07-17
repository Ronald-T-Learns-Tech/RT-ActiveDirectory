(import music)
;; CSC-151-
;; Mini-Project 5: Musical Copyright
;; Ronald Taylor
;; 2022-10-10
;; ACKNOWLEDGEMENTS:
;;   


;PART 1

;1. (all-pairs v lst) that takes a value v and a list of values and creates all possible 
;pairs of values where the first element is v and the second element is a value from lst. For example:

; if list = null -> returns null
; if list is not null -> pair the first elements with every possible value in the list 


;;;(all-pairs v (l)) -> list?
;;; v- > all
;;; l -> list
;;; reutrns a list of pairs where v is paired up with the possible values of l


(define all-pairs
(lambda(v l)
(match l
[null null]
[(cons head tail)(cons(pair v head)(all-pairs v tail))])))

(test-case "null" equal? null (all-pairs "a" null))

(test-case "five" equal? (list (cons "q" 0) (cons "q" 1) (cons "q" 2) (cons "q" 3) (cons "q" 4))

(all-pairs "q" (range 5)))

(test-case "no letter" equal? (list (cons "" 0) (cons "" 1) (cons "" 2) (cons "" 3) (cons "" 4))

(all-pairs "" (range 5)))
(all-pairs "q" (range 5))



;2. 

; (cartesian-product l1 l2) that takes two lists and produces the Cartesian Product
; of the elements drawn from lists l1 and l2. The Cartesian Product of two lists
; is a list that contains all the possible pairs (pair x y) where x is drawn from
; l1 and y is drawn from l2.


;if both lists are null -> return null
;if both lists are not null -> combine each element of list1 and list2 together and put into one list
;if a list is not null and the other is -> return null




;;;(cartesian-product (l1) (l2)) -> list?
;;; l1 -> all
;;; l2 -> all
;;returns elements of l1 and l2 combined
(define cartesian-product 
(lambda(l1 l2)
(match l1
[null null]
[(cons head tail)(append (all-pairs head l2) (cartesian-product tail l2))])))

(test-case "null" equal? null (cartesian-product null null))
(test-case "numbers and letters" equal? (list (cons "a" 0) (cons "a" 1) (cons "a" 2) (cons "a" 3)
(cons "a" 4) (cons "b" 0) (cons "b" 1) (cons "b" 2) (cons "b" 3)
(cons "b" 4)) (cartesian-product (list "a" "b") (range 5)))
(test-case "only numbers" equal? (list (cons 0 0) (cons 0 1)
(cons 0 2) (cons 0 3) (cons 0 4) (cons 1 0) (cons 1 1) (cons 1 2) 
(cons 1 3) (cons 1 4) (cons 2 0) (cons 2 1) (cons 2 2) (cons 2 3) 
(cons 2 4) (cons 3 0) (cons 3 1) (cons 3 2) (cons 3 3) (cons 3 4) 
(cons 4 0) (cons 4 1) (cons 4 2) (cons 4 3) (cons 4 4))
(cartesian-product (range 5) (range 5)))
(test-case "null" equal? null (cartesian-product (range 5) null))
(cartesian-product (range 3) (list "a" "b"))


; 3. (all-two-note-songs notes)that takes a list of notes 
; (as MIDI note values) as input and produces all the possible 
; two note songs drawn from the list of provided notes. The duration 
; of the notes should be quarter notes qn. You may implement this 
; function, wither either recursion or a list transformation pipeline.


;if two lists are given -> pair each note in both lists 
;if both lists are null -> many pairs of null will be returned
;if one list contains notes and the other does not -> return each element of the
;list with notes and pair it up with "nulls"


;;(two-note-example) > list?
;;; note -> midi note?
;;; the elements of notes in the first list will pair up with each element
;;;of notes in the second list, because of the cartesian-product

(define all-two-note-songs
(lambda(note)
(cartesian-product note note)))

(all-two-note-songs (list (note 69 qn) null))
(all-two-note-songs (list null null))
(all-two-note-songs (list (note 60 qn) (note 69 qn)))


;PART 2

; (cons-all x lsts) that takes a single value x and a list of lists
; lsts, and returns lsts but with x added to the front of every list.

; if x and list are null -> return null
; if x is not null but list is null -> return null
; if x and list are not null -> return the value inputed for x in front of every list

;;(cons-all x (lsts)) -> list?
;;; x -> all
;;; lsts -> lists?
;;; this will the value inputed for x in front of every list

(define cons-all
(lambda(x lsts)
(match lsts
[null null]
[(cons head tail)(cons(cons x head)(cons-all x tail))])))

(test-case "null" equal? null (cons-all null null))
(test-case "numbers" equal? (list (list 9 1 3 4) (list 9 2 4 1) (list 9 9 0)) (cons-all 9 (list (list 1 3 4)(list 2 4 1)(list 9 0))))
(test-case "numbers and letters" equal? (list (list "Ron" 2) (list "Ron" 999 9) (list "Ron" "a" 7)) (cons-all "Ron" (list (list 2)(list 999 9)(list "a" 7))))
(test-case "one null string" equal? null (cons-all "a" null))
(cons-all 10 (list (list 1 3 3 4) (list 0 9 8 )(list 5 6 7 )))


;(combinations lsts) that returns a list of lists. For each 
;list returned in the result, the ith element of the list is drawn
;from ith list of lsts. The result, therefore, contains all the 
;ways we can combine the elements of the lists found in lsts.

;;if a list of lists is null -> return (list null)
;;if list of lists is not null -> combines all elements of the list in all possible ways 


;;(combinations (list (list) (list) (list))) -> list?
;;; lst -> list?
;;; this returns each element of the lists in the list, and combines them in all possible ways

(define combinations
(lambda(lsts)
(match lsts
[null (list null)]
[(cons head tail)
(match head 
[null null] 
[(cons a b) (append (cons-all a (combinations tail)) (combinations (cons b tail)))])])))



(test-case "null" equal? (list null) (combinations null))
(test-case "numbers" equal? (list (list 1 9 5) (list 1 8 5) (list 1 7 5) (list 3 9 5) (list 3 8 5) (list 3 7 5))
(combinations (list (list 1 3)(list 9 8 7)(list 5))))
(test-case "numbers and letters" equal? (list (list 1 58 5) (list 1 58 "Ron") (list 1 "a" 5) (list 1 "a" "Ron") (list 3 58 5) (list 3 58 "Ron") (list 3 "a" 5) (list 3 "a" "Ron"))
(combinations (list (list 1 3 )(list 58 "a")(list 5 "Ron"))))
(combinations (list (list 1 2)(list 3 4 5)(list 6 7)))
(test-case "1122" equal? (list (list 1 1 1 1) (list 1 1 1 2) (list 1 1 2 1) (list 1 1 2 2) (list 1 2 1 1) 
(list 1 2 1 2) (list 1 2 2 1) (list 1 2 2 2) (list 2 1 1 1) (list 2 1 1 2) (list 2 1 2 1) (list 2 1 2 2) 
(list 2 2 1 1) (list 2 2 1 2) (list 2 2 2 1) (list 2 2 2 2))(combinations (list (list 1 2)(list 1 2)(list 1 2)(list 1 2))))

;2. (all-songs n notes) that takes a non-negative number n and a list of notes
;(as MIDI note values) as input and produces all the possible songs of n notes drawn 
;from the list of provided notes. The duration of the notes should be quarter notes qn.
;You may implement this function, wither either recursion or a list transformation pipeline.



;if n is 0 -> return (list null)

;if n is a real number but the list is null -> return a list of lists of nulls

;if n is a real number and a list of notes is provided -> reutrn all possible songs of n notes 


;;(all-songs n (list (notes))) > list?
;;; n -> (> n 0)
;;;note -> list?
;;;(cons notes (- n 1) notes)
;;;this will output reutrn all possible songs of n notes 

(define help
(lambda(a notes)
(if (equal? a 0)
null
(cons notes (help (- a 1) notes))
)))


(define all-songs
(lambda(n notes)
(combinations (help n notes))))

(define five-note-example
(all-songs 5 (list (note 58 qn) (note 60 qn)(note 65 qn))))

five-note-example


;Part 3


; Do you think music should still be valued in light of modern-day computation’s 
; ability to “do it all?” If so, what do you personally value about music in spite 
; of this assignment? If not, why do you feel that music has lost its value?


; Music should be valued in light of modern-day computation’s 
; ability to “do it all? Because I do not believe computation can do it all.
;Computer Music lacks the stretching of tones in order to make phrases emotional.
;Computer's lack the ability to produce the loveley tone of the music. As a violinist,
;I believe no computer can replace the sound of a violin. Therefore, I feel there is no 
;value to computer music even in the teaching of students.
;
