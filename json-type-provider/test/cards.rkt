#lang typed/racket/base

(require racket/match
         typed/rackunit
         "../main.rkt")

;; Example imposing more precise types on raw JSONdata
(define-json-types
  [Card ([rank : Rank] [suit : Suit])]
  [Deck (Listof Card)] ; list of 0+ cards
  [Deck+ (List* Card (Listof Card))] ; list of 1+ cards
  [Hand (List Card Card Card Card Card)] ; exactly 5 cards
  [Rank ([x : (U JSNum String)]
         => (U 1 2 3 4 5 6 7 8 9 10 'J 'Q 'K 'A)
         #:by (case x
                [(1 "1") 1]
                [(2 "2") 2]
                [(3 "3") 3]
                [(4 "4") 4]
                [(5 "5") 5]
                [(6 "6") 6]
                [(7 "7") 7]
                [(8 "8") 8]
                [(9 "9") 9]
                [(10 "10") 10]
                [("j" "J") 'J]
                [("q" "Q") 'Q]
                [("k" "K") 'K]
                [("a" "A") 'A]
                [else (error 'Rank "got ~a" x)]))]
  [Suit ([x : String]
         => (U '♥ '♦ '♣ '♠)
         #:by (case x
                [("heart") '♥]
                [("diamond") '♦]
                [("club") '♣]
                [("spade") '♠]
                [else (error 'Suit "got ~a" x)]))])

(define res (list (Card 'J '♥) (Card 5 '♣) (Card 8 '♠) (Card 9 '♦) (Card 'A '♥)))
(check-equal?
 (read-Deck (open-input-file "cards.json"))
 res)

(check-equal?
 (read-Deck+ (open-input-file "cards.json"))
 res)

(check-equal?
 (read-Hand (open-input-file "cards.json"))
 res)

;; Example of folding without intermediate list
(define read-Card-sum
  ((inst read-fold Card Integer) 0
                                 (λ (card sum)
                                   (define r (match (Card-rank card)
                                               [(? integer? n) n]
                                               ['J 11]
                                               ['Q 12]
                                               ['K 13]
                                               ['A 14]))
                                   (+ r sum))
                                 read-Card))
(check-equal?
 (read-Card-sum (open-input-file "cards.json"))
 (+ 11 5 8 9 14))
