;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname change) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;Kashif Moattar


;; A ChangeList is a (listof (list Nat (listof Nat)))

;;(fewest-coins change coins) uses inputs change and coins to produce and output a list of hte fewest coins needed to create that change with the given coins
;;Examples:
(check-expect (fewest-coins 45 '(1 5 10 25 100 200)) '(25 10 10))
(check-expect (fewest-coins 2 '(1)) '(1 1))


;;fewest-coins: Nat (listOf Nat) -> (listOf Nat)
(define (fewest-coins change coins)
  (local [;;(ListCoinage num listvalue) finds a Nat within a changelist of anyform an outputs the value/coin combinations associated with that Nat.
          ;;ListCoinage: Nat ChangeList -> (listOf Nat)
          (define (ListCoinage nat listvalue)
            (cond [(= nat (first (first listvalue))) (second (first listvalue))] ;;If nat is found withint the list-value voice, output the coinage associated with that number
                  [else (ListCoinage nat (rest listvalue))]))] ;;Otherwise, recall this function with the rest of listvalue (no empty? case as the value MUST exist before this point)
    (local [;;(possiblities OGnum firstbound secondbound coins listvalue incheck) uses inputs OGnum, firstbound, secondbound, coins, listvalue, and incheck to find and output the possibilities of a num
            ;;searching all "math-combos" to get those possibilities between two numbers (Ex. (3+1) = 4, (2+2) = 4 .....) using a ChangeList (listvalue) of all numbers containing before OGnum as denoted by firstbound+secondbound
            ;;to actually output these values
            ;;possiblities: Nat Nat Nat (listOf Nat) ChangeList Nat -> (listOf (listOf Nat))
            (define (possiblities OGnum firstbound secondbound coins listvalue incheck)
              (cond [(member? OGnum coins) (cons (cons OGnum empty) empty)] ;;If OGnum, the value being checked, is within the coins denominations, then output (cons (cons OGnum empty) empty
                    [(and (not (member? OGnum coins)) (= incheck 0)) (possiblities OGnum (sub1 firstbound) (add1 secondbound) coins listvalue (add1 incheck))] ;;If OGnum, is not in the coins, recall this function with firstbound subbed by 1, second added by 1, and incheck added by 1
                    [(> (ceiling (/ OGnum 2)) firstbound) empty] ;;If firstbound ever hits below the ceiling of OGnum/2, then output empty
                    [else (cons (append (ListCoinage firstbound listvalue) (ListCoinage secondbound listvalue)) ;;Otherwise, cons a recall to ListCoinage of the firstbound value and listvalue, with ListCoinage of secondbound and listvalue, then append both these results.
                                (possiblities OGnum (sub1 firstbound) (add1 secondbound) coins listvalue incheck))]))] ;;And ending the cons with a recall of this function with firstbound subbed by 1, and second added by 1
      (local [;;(entirelist change countup coins currentlist) uses inputs change, countup, coins, and currentlist to output a changelist of numbers 0 to change, where each number is
              ;;found to be the most optimized (Aka fewest coins to get that number), by searching through the ChangeList (aka input currentlist) before it, and updating that value
              ;;entirelist: Nat Nat (listOf Nat) (AnyOf ChangeList Empty) -> ChangeList
              (define (entirelist change countup coins currentlist)
                (cond [(> countup change) currentlist] ;;If countup is higher than change, output the currentlist
                      [(= countup 0) (entirelist change (add1 countup) coins (list (list 0 (list ))))] ;;if countup = 0, recall this function with the add1 of countup, coins, and (list (list 0 (list )))
                      [else (entirelist change (add1 countup) coins ;;Otherwise, recall this function with the add1 of countup
                                        (append (list (list countup 
                                                            (first (quicksort (possiblities countup countup 0 coins currentlist 0)  ;;And the append of the newest, and smallest of combo combos of a value through the first of quicksort of the recall to possiblities with countup, countup, 0, coins, currentlist, and 0
                                                                              (lambda (x y)
                                                                                (> (length y) (length x)))))))
                                                currentlist))]))]
       (quicksort (second (first (entirelist change 0 coins empty))) >))))) ;;Recalls to entirelist with change, 0, coins, and empty that is then first, and secnd to get the "change" coin combo
                                                                            ;;Then it is quicksorted from largest to smallest to get the desired result

;;Tests:
(check-expect (fewest-coins 300 '(1 25 100 200)) (list 200 100))
(check-expect (fewest-coins 100 '(1 4 90 100)) (list 100))
(check-expect (fewest-coins 3 '(1 5)) (list 1 1 1))
(check-expect (fewest-coins 10 '(5 1)) (list 5 5))
(check-expect (fewest-coins 10 '(1 2)) (list 2 2 2 2 2))
(check-expect (fewest-coins 10 '(1 5 2)) (list 5 5))
(check-expect (fewest-coins 11 '(1 8 6 5)) (list 6 5))
(check-expect (fewest-coins 14 '(1 3 10 5 7 4)) (list 10 4)) ;;Can list (list 7 7), but choses the first, and smallest coinage, which in my code is (list 10 4)
(check-expect (fewest-coins 3 '(1)) (list 1 1 1))
(check-expect (fewest-coins 3 '(1 3)) (list 3))
(check-expect (fewest-coins 3 '(3 5 6 1 7 9)) (list 3))
(check-expect (fewest-coins 0 '(3 5 6 1 7 9)) (list ))


