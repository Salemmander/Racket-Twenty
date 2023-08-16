#lang racket
(require racket/format)

; Define Cards
(define FACES '(A 2 3 4 5 6 7 8 9 10 J Q K))
(define SUITS '(♠ ♥ ♦ ♣))
; A full deck of cards
(define CARDS (apply append (map (lambda (f) (map (lambda (s) (list 'card s f)) SUITS)) FACES)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define TRANSCRIPT #f) ; Print out moves at each step
(define DEMO #t) ; Print out moves at each step in scheme format

(define HAND_SIZE 7)
(define PLAY_TO 200)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (card? c) (eq? (car c) 'card))
(define (suit card) (cadr card)) ; Extract Suit
(define (face card) (caddr card)) ; Extract Face
(define (same-suit c1 c2) (eq? (suit c1) (suit c2)))
(define (same-face? c1 c2) (eq? (face c1) (face c2)))
(define (value card)  ; The value used to make the count
  (cond [(eq? (face card) 'A) 1]
        [(eq? (face card) 'J) 10]
        [(eq? (face card) 'Q) 10]
        [(eq? (face card) 'K) 'wild]
        [else (face card)]))
(define (pvalue card)  ; The points the card is worth when calculating the penalty
  (cond [(eq? (face card) 10) 10]
        [(eq? (face card) 'J) 10]
        [(eq? (face card) 'Q) 10]
        [(eq? (face card) 'K) 20]
        [else 5]))
(define (can-pair? card) ; Only 10,J, and Q can be played as a pair
  (cond [(eq? (face card) 10) #t]
        [(eq? (face card) 'J) #t]
        [(eq? (face card) 'Q) #t]
        [else #f]))
(define (mkwild card value) (list 'wild card value)) ; constructs a list representing playing a king as wild.
(define (play-value play) ; Computes the value of playing a card or pair. Returns false on an invalid play.
  (cond [(card? play) (value play)]
		[(or (not (list? play)) (null? play)) #f]
		[(eq? 'wild (car play)) (caddr play)]
		[(and (card? (car play)) (can-pair? (car play))) 10]
		[else #f]))
(define (play->cards play) (flatten-play '() play)) ; extract the cards being played as a list
(define (flatten-play acc play)
  (cond [(null? play) acc]
        [(card? (car play)) (flatten-play (cons (car play) acc) (cdr play))]
        [(eq? 'wild (caar play)) (flatten-play (cons (cadar play) acc) (cdr play))]
        [else (flatten-play (cons (caar play) (cons (cadar play) acc)) (cdr play))]))

; Player State
; A list containing a penalty score, hand, player function, and custom state
; The custom state can be used by the player function, and can be anything
(define (pstate-init hand func) (list 0 hand func '()))
(define (pstate->score pstate) (car pstate))
(define (pstate<-score pstate s) (cons s (cdr pstate)))
(define (pstate->hand pstate) (cadr pstate))
(define (pstate<-hand pstate h) (cons (car pstate) (cons h (cddr pstate))))
(define (pstate->function pstate) (caddr pstate))
(define (pstate->custom pstate) (cadddr pstate))
(define (pstate<-custom pstate c) (list-set pstate 3 c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Driver Code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; State
; A list containing the current count, deck, last hand's first player, previous player's play, player 1's state, player 2's state
(define (state-init deck hand1 func1 hand2 func2) (list 1 deck 1 '() (pstate-init hand1 func1) (pstate-init hand2 func2) ))
(define (state->count state) (car state))
(define (state<-count state c) (list-set state 0 c))
(define (state->deck state) (cadr state))
(define (state<-deck state deck) (list-set state 1 deck))
(define (state->lastfirst state) (caddr state))
(define (state<-lastfirst state p) (list-set state 2 p))
(define (state->lastplay state) (cadddr state))
(define (state<-lastplay state play) (list-set state 3 play))
(define (state->player n state) (if (= n 1) (car (cddddr state)) (cadr (cddddr state))))
(define (state<-player n state pstate) (list-set state (+ n 3) pstate))


; Game Code
(define (play-game player1 player2) 
  (let* [(deck (shuffle CARDS))
         (h1 (take deck HAND_SIZE))
         (h2 (take (drop deck HAND_SIZE) HAND_SIZE))
         (new-deck (drop deck (* 2 HAND_SIZE)))]
    (play-game-rec 1 (state-init new-deck h1 player1 h2 player2))))

(define (new-hand state)
  (let* [(p1 (pstate->function (state->player 1 state)))
         (p2 (pstate->function (state->player 2 state)))
         (deck (shuffle CARDS))
         (h1 (take deck HAND_SIZE))
         (h2 (take (drop deck HAND_SIZE) HAND_SIZE))
         (new-deck (drop deck (* 2 HAND_SIZE)))]
	(keep-custom (state-init new-deck h1 p1 h2 p2) state)))

(define (keep-custom new-state old-state)
  (let* [(np1 (state->player 1 new-state))
         (op1 (state->player 1 old-state))
         (np2 (state->player 2 new-state))
         (op2 (state->player 2 old-state))
		 (pstate1 (pstate<-custom np1 (pstate->custom op1)))
		 (pstate2 (pstate<-custom np2 (pstate->custom op2)))]
	(state<-player 1 (state<-player 2 new-state pstate2) pstate1)))

(define (++ n) (- 3 n))

(define (play-game-rec start state)
  (cond [(game-over state) (end-of-game state)]
		[else (begin
				(transcribeln "New Hand")
				(demo 'newhand)
				(play-game-rec (++ start) (play-hand start state)))]))

(define (game-over state)
  (if state
    (let [(score1 (pstate->score (state->player 1 state)))
          (score2 (pstate->score (state->player 2 state)))]
      (and (not (= score1 score2)) (or (>= score1 PLAY_TO) (>= score2 PLAY_TO))))
	#t))

(define (end-of-game state)
  (if state
    (let [(score1 (pstate->score (state->player 1 state)))
          (score2 (pstate->score (state->player 2 state)))]
      (begin 
		(demo 'done)
        (displayln "")
        (displayln "Game Over!")
        (displayln "")
        (display "Player 1 Score: ")
        (displayln score1)
        (display "Player 2 Score: ")
        (displayln score2)
        (displayln "")
        (if (> score1 score2) 
    	    (displayln "Player 2 Has Won!")
    	    (displayln "Player 1 Has Won!"))))
	#f))

(define (init-hand count state)
  (let* [(deck (shuffle CARDS))
         (h1 (take deck 7))
         (h2 (take (drop deck 7) 7))
         (new-deck (drop deck 14))
         (p1 (pstate<-hand (state->player 1 state) h1))
         (p2 (pstate<-hand (state->player 2 state) h2))]
    (list count new-deck (++ (state->lastfirst)) '() p1 p2)))

(define (play-hand turn state)
  (cond [(not state) #f]
	    [(hand-over state) (calc-penalty state (new-hand state))]
        [else (play-hand (++ turn) (play-turn #f turn state))]))

(define (calc-penalty old-state new-state)
  (let* [(p1 (penalty (pstate->hand (state->player 1 old-state))))
         (p2 (penalty (pstate->hand (state->player 2 old-state))))
		 (pstate1 (pstate<-score (state->player 1 new-state) (+ p1 (pstate->score (state->player 1 old-state)))))
		 (pstate2 (pstate<-score (state->player 2 new-state) (+ p2 (pstate->score (state->player 2 old-state)))))]
	(state<-player 2 (state<-player 1 new-state pstate1) pstate2)))

(define (penalty hand) (foldl (lambda (x y) (+ (pvalue x) y)) 0 hand))

(define (hand-over state)
  (or (null? (pstate->hand (state->player 1 state))) (null? (pstate->hand (state->player 2 state))) (= 21 (state->count state))))

(define (play-turn err turn state)
  (let* [(pstate (state->player turn state))
         (take-turn (pstate->function pstate))
         (play (take-turn err (state->count state) (state->lastplay state) (pstate->score (state->player (++ turn) state)) pstate))]
    (cond [(and (list? play) (not (null? play))) 
           (let [(new-state (make-change turn play state))]
             (if new-state 
			   (begin
				 (transcribe "[")
				 (transcribe (state->count state))
				 (transcribe "] ")
				 (transcribe "Player ")
				 (transcribe turn)
				 (transcribe ": ")
				 (transcribe-play (car play))
				 (transcribe " (Current Hand: ")
				 (transcribe-cards (pstate->hand (state->player turn new-state)))
				 (transcribeln ")")
				 (demo (list turn (state->count state) (car play) (pstate->hand (state->player turn new-state))))
			     (state<-lastplay new-state (car play)))
			   (begin
				 (transcribe "Invalid Move: ")
				 (transcribe-play (car play))
				 (transcribeln "")
				 (demo (list turn 'invalid (state->count state) (car play)))
			     (play-turn #t turn state))))]
          [else (displayerr (string-append "Player " (number->string turn) " will not make legal move!")) #f])))

(define (reshuffle-cards state)
  (let [(cards1 (pstate->hand (state->player 1 state)))
        (cards2 (pstate->hand (state->player 2 state)))]
	(shuffle (filter-not (lambda (x) (or (member x cards1) (member x cards2))) CARDS))))


(define (make-change turn play state)
  (cond [(eq? (car play) 'draw-card)
         (let [(pstate (state->player turn state))
               (deck (state->deck state))]
           (if (null? deck) 
			 (let [(new-deck (reshuffle-cards state))]
			   (if (null? new-deck) #f (make-change turn play (state<-deck state new-deck))))
             (state<-deck (state<-player turn state (update-pstate (pstate<-hand pstate (cons (car deck) (pstate->hand pstate))) play)) (cdr deck))))]
        [else
          (let [(score (count-play 0 (car play)))
				(pstate (state->player turn state))]
            (if (= score (state->count state)) (state<-count (state<-player turn state (update-pstate pstate play)) (+ 1 (state->count state))) #f))]))

(define (update-pstate pstate play)
  (let [(npstate (update-hand pstate (car play)))]
	(pstate<-custom npstate (cadr play))))

(define (update-hand pstate play)
  (cond [(eq? play 'draw-card) pstate]
		[else 
		  (let [(cards (flatten-play '() play))]
			(pstate<-hand pstate (filter (lambda (x) (not (member x cards))) (pstate->hand pstate))))]))

(define (count-play acc play)
  (cond [(null? play) acc]
        [(card? (car play)) (count-play (+ acc (value (car play))) (cdr play))]
        [(eq? 'wild (caar play)) (count-play (+ acc (caddr (car play))) (cdr play))]
        [else (count-play (+ acc 10) (cdr play))]))

; User Interface Player
(define (user-interface err count last_play other-score pstate-base)
  (begin 
    (if err (displayerr "Invalid move.") '()) ;display error message if previous move was invalid
    (let [(pstate (if (null? (pstate->custom pstate-base)) (pstate<-custom pstate-base (if (null? last_play) 1 2)) pstate-base))]
	  (begin
        (displayln "")
        (displayln "================================================================================")
        (display "Player ")
        (display (pstate->custom pstate))
        (displayln " it is your turn.")
        (displayln "================================================================================")
        (do-user-interface '() count last_play other-score pstate)))))
(define (do-user-interface acc count last_play other-score  pstate)
  (begin
    (displayln "")
    (display "Your Score: ")
    (displayln (pstate->score pstate))
    (display "Opponent's Score: ")
    (displayln other-score)
    (display "count: ")
    (displayln count)
    (display "Opponent's move: ")
    (display-opp-move last_play)
    (display "Hand: ")
    (display-cards (pstate->hand pstate))
    (displayln "")
    (display "Current Play: ")
    (display-cards acc)
    (displayln "")
    (displayln "")
    (let [(option (menu '(("Draw Card and End Turn" draw-card)
                          ("Play Card" play-card)
                          ("Play Pair" play-pair)
                          ("Clear Play" clear)
                          ("Done" done))))]
      (cond [(eq? option 'draw-card) (list 'draw-card (pstate->custom pstate))]
            [(eq? option 'play-card) 
             (let [(play (play-card pstate))]
               (if play
                 (let [(ap (if (eq? (face (car play)) 'K) (get-wild-val (car play)) (car play)))]
                   (if ap
                     (do-user-interface (cons ap acc) count last_play other-score (cadr play))
                     (do-user-interface acc count last_play other-score pstate)))
                 (do-user-interface acc count last_play other-score pstate)))]
            [(eq? option 'play-pair) 
             (let [(p (filter (lambda (pair) (can-pair? (car pair))) (get-pairs pstate)))]
               (if (null? p)
                 (begin
                   (displayerr "You have no pairs.")
                   (do-user-interface acc count last_play other-score pstate))
                 (let ([play (play-pair pstate p)])
                   (if play
                    (do-user-interface (cons (car play) acc) count last_play other-score (cadr play))
                    (do-user-interface acc count last_play other-score pstate)))))]
            [(eq? option 'clear) (do-user-interface '() count last_play other-score (clear-changes acc pstate) )]
            [(eq? option 'done) (list acc (pstate->custom pstate))]
            [else (writeln option)]))))

(define (clear-changes acc pstate) (pstate<-hand pstate (append (pstate->hand pstate) (flatten-play '() acc))))

(define (get-wild-val card)
  (begin 
    (display "Enter Wild Card Value (1-20): ")
    (let [(input (string->number (read-line (current-input-port) 'any) 10 'number-or-false))]
      (cond [(not input) #f]
            [(and (>= input 1) (<= input 20)) (list 'wild card input)]
            [else #f]))))

(define (display-opp-move play)
  (cond [(null? play) (displayln "You are first.")]
		[(eq? 'draw-card play) (displayln "Draw a card.")]
		[else (begin
                 (display-cards play)
	             (displayln ""))]))

(define (display-cards cards) (show-cards display cards))
(define (display-card card) (show-card display card))

(define (play-card pstate)
  (let [(h (pstate->hand pstate))]
    (begin
      (displayln "Select a card.")
      (let [(select (menu (append (map (lambda (c) (list (string-append (~a (suit c)) (~a (face c))) c)) h) '(("Back." back)))))]
        (if (eq? select 'back) #f
          (list select (pstate<-hand pstate (filter (lambda (c) (not (equal? c select))) h))))))))

(define (get-pairs pstate)
  (let [(h (pstate->hand pstate))]
    (get-pairs-rec '() h)))

(define (find-match c cards)
  (cond 
    [(null? cards) '()]
    [(same-face? c (car cards)) cards]
    [else (find-match c (cdr cards))]))

(define (get-pairs-rec acc cards)
  (if (null? cards) acc
    (let [(m (find-match (car cards) (cdr cards)))]
      (if (null? m) 
        (get-pairs-rec acc (cdr cards))
        (get-pairs-rec (cons (list (car cards) (car m)) acc) (cdr cards))))))

(define (play-pair pstate pairs)
  (let [(h (pstate->hand pstate))]
    (begin
      (displayln "Select a pair.")
      (let [(select (menu (append (map mkpairopt pairs) '(("Back." back)))))]
        (if (eq? select 'back) #f
            (list select (pstate<-hand pstate (filter (lambda (c) (not (member c select))) h))))))))

(define (mkpairopt pair)
  (list (string-append "(" (~a (suit (car pair))) (~a (face (car pair))) "," (~a (suit (cadr pair))) (~a (face (cadr pair))) ")") pair))

(define (menu options)
  (begin
    (display-options 1 options)
    (displayln "")
    (display "Select Option: ")
    (let [(input (string->number (read-line (current-input-port) 'any) 10 'number-or-false))]
      (if input 
        (let [(option (get-option input options))]
          (if option option 
            (begin
              (displayerr "Invalid Option")
              (menu options))))
        (begin
          (displayerr "Invalid Option")
          (menu options))))))

(define (display-options n options)
  (if (null? options) '()
    (begin
      (display n)
      (display ". ")
      (displayln (caar options))
      (display-options (+ n 1) (cdr options)))))

(define (get-option i options)
  (cond [(null? options) #f]
        [(= 1 i) (cadar options)]
        [else (get-option (- i 1) (cdr options))]))

(define (displayerr msg) 
  (begin
    (displayln "")
    (display "!! ")
    (display msg)
    (displayln " !!")
    (displayln "")))

(define (transcribe msg) (do-transcribe display msg))
(define (transcribeln msg) (do-transcribe displayln msg))

(define (do-transcribe f msg) (if TRANSCRIPT (f msg) '()))

(define (demo msg) (if DEMO (writeln msg) '()))

(define (transcribe-play play) 
  (if (eq? 'draw-card play) (transcribe "Draw Card") (transcribe-cards play)))

(define (transcribe-cards cards) (show-cards transcribe cards))

(define (show-cards f cards)
  (cond [(null? cards) '()]
        [(null? (cdr cards)) (show-card f (car cards))]
        [else (show-card f (car cards)) (f ", ") (show-cards f (cdr cards))]))

(define (show-card f card)
  (cond [(card? card)
         (begin
           (f (suit card))
           (f (face card)))]
        [(eq? 'wild (car card))
         (begin
           (f "[")
           (show-card f (cadr card))
           (f " as ")
           (f (caddr card))
           (f "]"))]
        [else
          (begin
           (f "(")
           (show-card f (car card))
           (f ", ")
           (show-card f (cadr card))
           (f ")"))]))

(include "project2.rkt")
