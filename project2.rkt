; #lang racket is not needed because this file will be included from base.rkt



;; Example AI that always draw's cards.
(define (example-ai-1 err count last_play other_score pstate) (if err '() '(draw-card, ())))

;; Example AI that will only play a card if the count can be made with 1 card.
(define (example-ai-2 err count last_play other_score pstate) 
  (if err '() (example-ai-2-rec count (pstate->hand pstate))))
(define (example-ai-2-rec count hand)
  (cond [(null? hand) '(draw-card, ())]
		[(eq? (face (car hand)) 'K) (list (list (mkwild (car hand) count)) '())]
		[(= (value (car hand)) count) (list (list (car hand)) '())]
		[else (example-ai-2-rec count (cdr hand))]))




;---------------------PROJECT-AI-1---------------------------- 
;AI 1

;Get combinations of cards DONE
;Check which ones equal to or less than the count without kings DONE
  ;If play is empty return 0 DONE
  ;If element is a King just keep going   DONE
  ;If element is not a king add the value   DONE
;Check the ones that are less than the count and if they do not have a king remove them DONE
;Check the ones that are equal to the count and if they have a king remove them because the kings are not set at this point and they cannot be zero DONE
     ;Now all possible plays can make the count
;Now check for which ones have the most cards in the play DONE
;get list of plays with the fewest kings DONE
;check if there is one with a king DONE
    ; if there is a king/kings set the values
    ; else play the first option



(define (project-ai-1 err count last_play other_score pstate)
  (if err '() (project-ai-1-rec count (pstate->hand pstate))))

(define (project-ai-1-rec count hand)
  (cond
        [(null?(fewest-kings(longest-sublists (playable count(less-than-count count (combos hand)))))) '(draw-card, ())]
        [(has-king? (car (fewest-kings(longest-sublists (playable count(less-than-count count (combos hand)))))))
              (list (set-kings count (car (fewest-kings(longest-sublists (playable count(less-than-count count (combos hand)))))))'())]; 
        [(not (has-king? (car (fewest-kings(longest-sublists (playable count(less-than-count count (combos hand))))))))
         (list(car (fewest-kings(longest-sublists (playable count(less-than-count count (combos hand))))))'()) ] 
        )
)

;--------------------PROJECT-AI-2-----------------------------
; AI 2

; Get combinations of cards DONE
; Check which ones equal to or less than the count without kings DONE
; Check the ones that are less than the count and if they do not have a king remove them DONE
; Check the ones that are equal to the count and if they have a king remove them because the kings are not set at this point and they cannot be zero DONE
; Check for the ones that have the highest penalty value DONE
; Check if there are any kings left DONE
;    If there are kings set them equal to whatever makes them equal to the count
;    Else play the hand
  

(define (project-ai-2 err count last_play other_score pstate)
  (if err '() (project-ai-2-rec count (pstate->hand pstate))))

(define (project-ai-2-rec count hand)
  (cond
        [(null?(high-penalty (playable count(less-than-count count (combos hand))))) '(draw-card, ())]
        [(has-king?(car(high-penalty(playable count(less-than-count count (combos hand))))))
              (list(set-kings count(car(high-penalty(playable count(less-than-count count (combos hand))))))'())]; 
        [(not (has-king?(car(high-penalty(playable count(less-than-count count (combos hand)))))))
         (list(car(high-penalty(playable count(less-than-count count (combos hand)))))'()) ] 
        )
)


;--------------------Helpers----------------------------------
(define (combos hand)
  (cdr (combinations hand)))

(define (sum elemList)
 (cond [(null? elemList) 0]
       [(eq? (face (car elemList)) 'K) (sum (cdr elemList))]
       [(+ (value (car elemList)) (sum (cdr elemList)))]
       ))

(define (penalty-sum lst)
   (cond [(null? lst) 0]
         [(+ (pvalue (car lst)) (penalty-sum (cdr lst)))]
       ))
  

(define (high-penalty lst)
  (cond ((null? lst) '())  ; return empty list if input list is empty
        (else
         (define (lst-pen lst) (penalty-sum lst))
         (define max-penalty (apply max (map lst-pen lst)))
         (filter (lambda (sublist) (= (penalty-sum sublist) max-penalty)) lst))
  ))


(define (longest-sublists lst)
  (cond ((null? lst) '())  ; return empty list if input list is empty
        (else
         (define (length-of-list lst) (length lst))
         (define max-length (apply max (map length-of-list lst)))
         (filter (lambda (sublist) (= (length sublist) max-length)) lst))))

(define (less-than-count count lst)
  (filter (lambda (sublst) (<= (sum sublst) count)) lst))

(define (playable count lst)
  (filter (lambda (sublst) (or
                            (and
                             (< (sum sublst) count) (has-king? sublst))
                            (and
                             (= (sum sublst) count) (not(has-king? sublst))))) lst))

(define (fewest-kings lst)
  (if (null? lst)
      '()
      (let ((min-count (apply min (map (lambda (sublst) (count (lambda (x) (eq? (face x) 'K)) sublst)) lst))))
        (filter (lambda (sublst) (= (count (lambda (x) (eq? (face x) 'K)) sublst) min-count)) lst))))


(define (has-king? lst)
  (cond ((null? lst) #f)
        ((eq? (face(car lst)) 'K) #t)
        (else (has-king? (cdr lst)))))

(define (num-kings lst)
  (count (lambda (x) (equal? (face x) 'K)) lst))


(define (last-index-king lst)
  (let loop ((lst lst) (index (- (length lst) 1)))
    (cond ((null? lst) #f)
          ((equal? (face (car (reverse lst))) 'K) index)
          (else (loop (reverse (cdr (reverse lst))) (- index 1)))))) 


(define (set-kings count lst)
         (map (lambda (x i) (if (equal? (face x) 'K)
                             (if (eq? i (last-index-king lst))
                                 (mkwild x (- (- count (sum lst)) (- (num-kings lst) 1)))
                                 (mkwild x 1))
                             x))
                       lst (range 0 (length lst))))








  
; Testing your first AI
; (play-game user-interface project-ai-1)

; Testing your second AI
 (play-game user-interface project-ai-2)

; Make them Play each other
;(play-game project-ai-1 project-ai-2)



