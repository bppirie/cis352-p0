#lang racket

;;; Project 0 Tic-tac-toe with Racket
;;; 
;;; Please immediately read README.md

(provide board?
         next-player
          valid-move?
          make-move
          winner?
          calculate-next-move)

;; 
;; Useful utility functions
;;

; Returns the number of elements in l for which the predicate f
; evaluates to #t. For example:
;
;    (count (lambda (x) (> x 0)) '(-5 0 1 -3 3 4)) => 3
;    (count (lambda (x) (= x 0)) '(-5 0 1 -3 3 4)) => 1
(define (count f l)
  (cond [(empty? l) 0]
        [(f (car l)) (add1 (count f (cdr l)))]
        [else (count f (cdr l))]))

;; 
;; Your solution begins here
;; 

; Check whether a list is a valid board
(define (board? lst)
  (cond
        ;; Checks if the square root of the length of the list is an integer, if it is then skip if not then false
        [(not (integer? (sqrt (length lst)))) #f]
        ;; Compares the number of 'X, 'O, and 'E symbols in the list with its lemgth, if it is then skip, if not then false
        [(not (= (length lst) (count (lambda (x) (or [equal? x 'X] [equal? x 'O] [equal? x 'E])) lst))) #f]
        ;; If |'X - 'O| > 1 then false, if not then skip
        [(< 1 (abs (- (count (lambda (x) (equal? x 'X)) lst) (count (lambda (x) (equal? x 'O)) lst)))) #f]
        ;; If 'O > 'X then false, if not then skip
        [(> (count (lambda (x) (equal? x 'O)) lst) (count (lambda (x) (equal? x 'X)) lst)) #f]
        ;; If no conditions are flagged then we have a valid board
        [else #t]))
  

;;; From the board, calculate who is making a move this turn
(define (next-player board)
  (cond
    ;; If no moves or 'X = 0 = 'O then return 'X
    [(= 0 (count (lambda (x) (equal? x 'X)) board) (count (lambda (x) (equal? x 'O)) board)) 'X]
    ;; If 'X = 'O then return 'X
    [(= (count (lambda (x) (equal? x 'X)) board) (count (lambda (x) (equal? x 'O)) board)) 'X]
    [else 'O]))   ;; Otherwise return 'O

;;; If player ('X or 'O) want to make a move, check whether it's this
;;; player's turn and the position on the board is empty ('E)
(define (valid-move? board row col player)
  (cond
    ;; Ensure input is correct for the board size
    [(or (equal? board '()) (< row 0) (< col 0) (>= row (sqrt(length board))) (>= col (sqrt(length board)))) #f]
    ;; If player is not next-player then false, if they are then skip
    [(not (equal? player (next-player board))) #f]
    ;; If element at pos (row)*(sqrt board length) + (col) is not E then false otherwise skip
    [(not (equal? 'E (list-ref board (+ col (* row (sqrt (length board))))))) #f]
    [else #t]))   ;; Otherwise the move is valid so true

;;; To make a move, replace the position at row col to player ('X or 'O)
(define (make-move board row col player)
  (list-set board (+ col (* row (sqrt (length board)))) player))

;;; To determine whether there is a winner?
(define (winner? board)
  ;; Check all possible ways to win
        ;; Check if any rows have all 'X or all 'O
  (cond [(test-rows board 'X (sqrt (length board))) 'X] [(test-rows board 'O (sqrt (length board))) 'O]
        ;; Check if any columns have all 'X or all 'O
        [(test-cols board 'X (sqrt (length board))) 'X] [(test-cols board 'O (sqrt (length board))) 'O]
        ;; Check if the forwards diagonal has all 'X or 'O
        [(test-diagf board 'X (sqrt (length board))) 'X] [(test-diagf board 'O (sqrt (length board))) 'O]
        ;; Check if the backwards diagonal has all 'X or 'O
        [(test-diagb board 'X (sqrt (length board))) 'X] [(test-diagb board 'O (sqrt (length board))) 'O]
        ;; Otherwise there hasnt been a winner
        [else #f]))

;; Test each row starting with the last row, combine all row boolean values using or
(define (test-rows board player row)
  (if (= row 0) ;; If row is 0 we have tested all the rows so return false
      #f
      ;; If row is not zero then test if that row is true or if the rest of the rows are true
      (or (test-row board player row (sqrt (length board))) (test-rows board player (- row 1)))))

;; Test row starting with the last element in the row
(define (test-row board player row cnt)
  ;; Use recursion to check each element in the row
        ;; If the the cnt is 1 the first element is in position n(row-1) for nXn board
  (cond [(= cnt 1) (equal? player (list-ref board (* (sqrt (length board)) (- row 1))))]
        ;; If any other element in the row matches player then check the element before it
        [(equal? player (list-ref board (+ (* (sqrt (length board)) (- row 1)) (- cnt 1)))) (test-row board player row (- cnt 1))]
        ;; Otherwise the row is not all the same player
        [else #f]))

;; Test each column starting with the last column, combine all column values using or
(define (test-cols board player col)
  (if (= col 0) ;; If col is 0 we have tested all the columns so return false
      #f
      ;; If col is not zero then test if that column is true or if the rest of the columns are true
      (or (test-col board player col (sqrt (length board))) (test-cols board player (- col 1)))))

;; Test column starting with the last element in the column
(define (test-col board player col cnt)
  ;; Use recursion to check each element in the column
        ;; If cnt is 1 the first element in the column is in position col-1
  (cond [(= cnt 1) (equal? player (list-ref board (- col 1)))]
        ;; If any other element in the column matches player then check the elements before it
        [(equal? player (list-ref board (+ (* (sqrt (length board)) (- cnt 1)) (- col 1)))) (test-col board player col (- cnt 1))]
        ;; Otherwise the column is not all the same player
        [else #f]))

;; Test forward diagonal starting with the bottom right corner
(define (test-diagf board player size)
  (if (= size 0) ;; If size is 0 then we have tested all elements on front diagonal so return true
      #t
      ;; Test each diagonal entry and combine results with and
      (and (test-forward board player size) (test-diagf board player (- size 1)))))

;; Check if entry on front diagonal matches player
(define (test-forward board player size)
  ;; Position of front diagonal entries are in n(size-1)+(size-1) for nXn board
  (equal? player (list-ref board (+ (* (sqrt (length board)) (- size 1)) (- size 1)))))

;; Test backward diagonal starting with bottom left corner
(define (test-diagb board player size)
  (if (= size 0) ;; If size is 0 then we have tested all elements on back diagonal so return true
      #t
      ;; Test each diagonal entry and combine results with and
      (and (test-backward board player size) (test-diagb board player (- size 1)))))

;; Check if entry on back diagonal matches player
(define (test-backward board player size)
  ;; Position of back diagonal entries are in n(size)+size for nXn board
  (equal? player (list-ref board (- (* (sqrt (length board)) size) size))))

;;; The board is the list containing E O X 
;;; Player will always be 'O
;;; returns a pair of x and y
(define (calculate-next-move board player)
  ;; Create list of scores corresonding to possible moves
  (define scores (maxscores '() player board '() (length board) 0))
  ;; Find the maxscore in the list of scores
  (define maxscore (foldl max (first scores) scores))
  ;; Find the index of maxscore
  (define index (index-of scores maxscore))
  ;; Row is equal to the index/n for nXn board using integer div
  (define x (quotient index (sqrt (length board))))
  ;; Column is equal to index-(n*x) for nXn board
  (define y (- index (* (sqrt (length board)) x)))
  ;; Return the pair
  (define pair (cons x (cons y '())))
  ;; Board is automatically replacing 1st entry so check if board is full and return false
  (if (full? board)
      #f
      pair))

;; Find maxscores of possible moves by iterating through the list and calling calc-score on each 'E entry to test the score if that entry were player
(define (maxscores board player mut-board scorelist size round)
  ;; If mut-board is empty then we have iterated through the list so return reverse of score list since it was built in reverse order
  (cond [(equal? mut-board '()) (reverse scorelist)]
        ;; If the first element is 'E then find maxscores of the rest of mut-board but save the first element in board to be used to restore the board when calling calc score with player in place of 'E
        [(equal? (first mut-board) 'E) (maxscores (cons (first mut-board) board) player (rest mut-board) (cons (calc-score (append (reverse board) (cons player (rest mut-board))) player round) scorelist) size round)]
        ;; Else the first symbol is 'X or 'O so place a negative number smaller than any possible score to ensure the ai doesnt select an already chosen move
        [else (if (equal? player 'O)
                  ;; If the player is 'O then make already chosen entries super small
                  (maxscores (cons (first mut-board) board) player (rest mut-board) (cons (* -10 size size) scorelist) size round)
                  ;; If the player is 'X then make already chosen entries super big
                  (maxscores (cons (first mut-board) board) player (rest mut-board) (cons (* 10 size size) scorelist) size round))]))

;; Calculate the score of a board
(define (calc-score board player round)
  ;; If the board has winner 'O then score is 10
  (cond [(equal? (winner? board) 'O) (- 10 round)]
        ;; If the board has winner 'X then score is -10
        [(equal? (winner? board) 'X) (+ -10 round)]
        ;; If board is full score is 0
        [(full? board) 0]
        ;; Else there is no winner, so for each 'E entry find the score of the board if the opposing player were to select it
        [else (define scores (maxscores '() (if (equal? player 'O) 'X 'O) board '() (length board) (add1 round)))
              ;; Take the list of maxscores and check if the round is odd or even
              (if (equal? (modulo round 2) 0)
                  ;; If the round is even then the scores were created with possible 'X moves so select the minimizing score
                  (foldl min (first scores) scores)
                  ;; If the round is odd then the scores were created with possible 'O moves so select the maximizing score
                  (foldl max (first scores) scores))]))

;; Check if the board is full
(define (full? board)
  ;; If the board is full return #t otherwise return #f
  (not (ormap (lambda (x) (equal? x 'E)) board)))
<<<<<<< HEAD
=======
              
>>>>>>> f0e5bea7b01786c8ca8394140b6aea251ed9ec76

