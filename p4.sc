;;; This problem set is an exercise in game-tree (adversarial) search.
;;; You will implement the min-max procedure with alpha/beta pruning for playing two-person, multiple-move,
;;; zero-sum, deterministic, complete-information games.
;;; And you will instantiate this procedure for playing Tic Tac Toe.
;;; When solving this problem set, it may be useful to refer to the lecture slides for lectures 7 and 8.
;;; Tic Tac Toe is played on an n × n board. The two players are X and O.
;;; Players alternate turns with player X playing first. The board is initially empty.
;;; At each turn, a player marks an empty square with his or her identity (i.e. X or O).
;;; The first player to mark n squares in a row (horizontally, vertically, or diagonally) with his or her mark wins.
;;; We will represent player X as 1 and player O as −1. We will represent an n × n board as a list of n rows,
;;; from top to bottom. We will represent a row of an n × n board as a list of n positions, from left to right.
;;; Each position will be:
;;;                       1, indicating a mark by player X,
;;;                      −1, indicating a mark by player O,
;;;                       0, indicating an empty position.

;; prints out the board
(define (print-board board)
 (cond ((null? board) (newline) #t)
       (else
	(write (first board)) (newline)
	(print-board (rest board)))))

;;; initial-board n
;;; n is a positive integer. Returns b^0 , the initially empty n × n Tic Tac Toe board.
(define (initial-board n)
 (map (lambda (z)
       (zeros n))
      (zeros n)))

(define (zeros n)
 (cond ((zero? n) '())
       (else (cons 0 (zeros (- n 1))))))

(define (test-initial-board)
 (print-board (initial-board 3)))
  
;;; moves b
;;; b is a board. Returns m(b), the set (represented as a list) of legal moves for player p(b) in board b.
;;; You can choose whatever representation you wish for moves so long as it is accepted by make-move
;;; and produced by optimal-moves~.
;;; Each legal move is represented as a game board where ONE square is replaced with #t where a legal move is possible
;;; ( 0  1 -1)
;;; (#t -1  1)
;;; ( 0  1  0)
(define (moves b)
 (moves-recurse b (- (length b) 1) (- (length (first b)) 1)))

(define (test-moves)
 (map print-board (moves '(( 0  1 -1)
			   ( 0 -1  1)
			   ( 0  1  0)))))

(define (moves-recurse b row col)
 ;;(display "moves-recurse") (write b) (write row) (write col) (newline)
 (let ((new-board (replace-zero-row-col b row col)))
  (cond ((map-reduce (lambda (a b) (or a b))
		    #f
		    (lambda (row) (not (eq? (length row) 3)))
		    new-board)
	 (cond
	  ((and (zero? row) (zero? col)) '())
	  ((zero? col) (moves-recurse b (- row 1) (- (length b) 1)))
	  (else (moves-recurse b row (- col 1)))))
	((and (zero? row) (zero? col)) (list new-board))
	((zero? col) (cons new-board (moves-recurse b (- row 1) (- (length b) 1))))
	(else (cons new-board (moves-recurse b row (- col 1)))))))

;;; Tries to replace a zero at position with #f (row, col), otherwise return the empty list.
(define (replace-zero-row-col b row col)
 ;;(display "replace-zero-row-col") (write b) (write row) (write col) (newline)
 (let ((new-row (replace-zero-col (first b) col)))
  (if (null? new-row)
      '()
      (if (zero? row)
	  (cons new-row (rest b))
	  (cons (first b) (replace-zero-row-col (rest b) (- row 1) col))))))

(define (replace-zero-col r col)
 ;;(display "replace-zero-col") (write r) (write col) (newline)
 (cond ((null? r) '())
       ((zero? col) (if (zero? (first r)) (cons #t (rest r)) '()))
       (else (cons (first r) (replace-zero-col (rest r) (- col 1))))))
 
;;;(cond ((and (zero? row) (zero? col)) (cons (if (zero? (first (first b))) #t (first (first (b))))
;;;					    (replace-zero-at b (

;;; make-move m b
;;; m is a move. b is a board. Returns b 0 (m, b), the board that results when player p(b) takes move m in board b.
;;; You can choose whatever representation you wish for moves so long as it is what is produced by moves
;;; and optimal-moves~.
(define (make-moves m b)
 '())

;;; win b
;;; b is a board. Returns w 0 (b), i.e. 1 if player 1 has won in board b, −1 if player −1 has won in board b,
;;; and 0 if neither player has won.
(define (win b)
 '())

;;; optimal-moves~ k b
;;; k is either a nonnegative integer or ∞. b is a board. k is a search-depth bound. If k = ∞, returns m̂(b),
;;; the set (represented as a list) of optimal moves for player p(b) in board b. If k 6 = ∞, returns m̃ k (b),
;;; a set (represented as a list) of moves. You can choose whatever representation you wish for moves so
;;; long as it is accepted by make-move and produced by moves.
;;; Note that there is a bug in the definition of m̃ k (b) in the original version of the lecture slides
;;; that has been fixed in the new version on the Web site.
(define (optimal-moves~ k b)
 '())


;;; More notes

;;; If k = ∞, there is no search-depth bound. In this case, we want you to find the optimal moves
;;; by searching to the end of the game. No static board evaluator is necessary in this case.
;;; If k 6 = ∞, we want you to search k moves into the future and then employ a static evaluator w̃ 0 (b).
;;; You are free to choose your own heuristic static evaluation function, though it should return nonintegral
;;; values (see slide 16 of lecture 8) in some cases.

;;; We want your implementation of optimal-moves~ to perform alpha/beta pruning.

;;; While not strictly necessary, you will probably find it helpful to implement procedures for
;;; p(b), w ∗ (b), w l ∗ (b), and w̃ k (b) as defined in the lecture slides.
;;; You will probably also find it helpful to implement a procedure for w̃ l k (b)
;;; which is not defined in the lecture slides but is a simple and obvious extension of what is defined there.

;;; To help debug and test your implementation, we have provided the GUI (p4).
;;; Clicking on +N or −N increments or decrements the board size n respectively.
;;; Clicking on +K or −K increments or decrements the depth bound k respectively, if it is not ∞,
;;; and sets it to zero if it is ∞. Clicking on W* sets k to ∞. W* is highlighted if k = ∞.
;;; If k 6 = ∞, +K and −K indicate the current value of k. Clicking on New Game initializes b to b^0.
;;; Currently, the GUI is hardwired so that the human plays X and the computer plays O.
;;; The human plays by clicking on a square. The message pane indicates when the game ends in a win or draw,
;; the computer resigns, or the human has made an illegal move.
;;; Clicking on Quit exits the GUI.

;;; Good luck and have fun!
