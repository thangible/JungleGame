#lang racket
(require 2htdp/universe)

;;Exports
(provide start-server)

;;Max number of players
(define NUM_PLAYERS 2)

;;Board-Representation
(define empty_board  (list "red" "rat" "red" 0  2  "cat" "red" 5 1 "dog" "red"  1  1  "wolf"  "red"  4  2
                           "leopard"  "red"    2   2 "tiger" "red" 6  0  "lion"   "red" 0    0
                           "elephant"  "red"  6 2 "rat"   "black" 6  6 "cat"  "black" 1 7 "dog" "black"
                           5 7  "wolf" "black"  2  6 "leopard"  "black"  4  6
                           "tiger"  "black"  0  8  "lion"  "black"  6  8  "elephant"  "black"  0  6
                           "den"  "red"  3  0 "den" "black" 3 8))

(define UNIVERSE0 
  (list '() "wait" empty_board))

;;Quick accessors for the universe
(define (current_worlds univ)
  (first univ))
(define (world1 univ)
  (first (current_worlds univ)))
(define (world2 univ)
  (second (current_worlds univ)))

(define (current_state univ)
  (second univ))

(define (current_board univ)
  (third univ))

;;Represent a universe
;; '((iworld_active iworld_inactive) status (("new active player" "piece1" "color"
;;                                            "x-coordinate" "y-coordonate" "piece2" ...)) 
;; 
;;Where status: 'wait
;;Message to the universe
;; (universe world "status" ("new active player" "piece" "color" "x-coordinate" "y-coordonate" ....))
;;                                 ->  create a new board from the message
;;                                 ->  inform all other worlds
;;                                 ->  switch active/inactive worlds
;; (universe world 'reset)   -->  only possible, if (third universe) == 'finished, 
;;                                start a new game
;;                                switch active/inactive worlds

;List -> String
;Check if a player won and return the won player
(define (check_win? board)
  (cond
       [(and(list? (member "den" board)) (list? (member "den" (rest(member "den" board))))) #f]
       [else #t]))

;;add a new world
(define (add-world univ wrld)
  (cond 
         ;;If Maximal Number of players is reached
         ;; --> decline the world
         [(= (length (current_worlds univ)) NUM_PLAYERS)
          (make-bundle univ
                       (list (make-mail wrld (list "rejected" empty_board)))
                       (list wrld))]
         
         ;;If Maximal Number of players with this world is reached
         ;; --> Add new world
         ;; --> Start the game
         [(= (length (current_worlds univ)) (- NUM_PLAYERS 1))
          (make-bundle (list
                        (append (current_worlds univ) (list wrld))
                        'play
                        empty_board)
                       (list (make-mail (world1 univ) (list "turn" empty_board))
                             (make-mail wrld   (list "wait" empty_board)))
                       '())]
         
         ;;If Maximal Number of players with is not reached
         ;; --> Add new world
         [else 
          (make-bundle (list 
                        (append (current_worlds univ) (list wrld))
                        "wait" 
                        empty_board)
                       (list (make-mail wrld (list "wait" empty_board)))
                       '())]))
 
;;Communication between worlds
(define (handle-messages univ wrld message)
  (display "got message")
  (display message)
  (cond[(and(and(list? message) (= (length message) 2)) (= (remainder(-(length (second message)) 1) 4) 0))
         (define new_board  (second message))
         (cond[(and(check_win? new_board) (not(equal?(first message) "restart")))
               (make-bundle (list 
                        (current_worlds univ) 
                         "finished" 
                         new_board)
                        (list (make-mail wrld          (list "won" new_board))
                              (make-mail (world2 univ) (list "lost" new_board)))
                                '())]
              [(equal?(first message) "restart")
               (make-bundle (list 
                        (current_worlds univ) 
                         "play" 
                         new_board)
                        (list (make-mail (world1 univ)         (list "turn" empty_board))
                              (make-mail (world2 univ) (list "wait" empty_board)))
                                '())]
              [else (make-bundle (list 
                             (reverse (current_worlds univ))
                             "play" 
                             new_board)
                            (list (make-mail (world1 univ) (list "wait" new_board))
                                  (make-mail (world2 univ) (list "turn" new_board)))
                            '())])]
       ;other request will not change the universe
       [else (make-bundle univ '() '())]))

;;create a universe
(define (start-server)
  (universe UNIVERSE0
            (on-new add-world)
            (on-msg handle-messages)
            (port 9092)))

;(start-server)
