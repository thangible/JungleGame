#lang racket
(require lang/posn)
(require 2htdp/universe)

(require "Constants.rkt")
(require "Structs.rkt")
(require "helper-functions.rkt")


;'##::: ##:'########:'########:'##:::::'##::'#######::'########::'##:::'##:
; ###:: ##: ##.....::... ##..:: ##:'##: ##:'##.... ##: ##.... ##: ##::'##::
; ####: ##: ##:::::::::: ##:::: ##: ##: ##: ##:::: ##: ##:::: ##: ##:'##:::
; ## ## ##: ######:::::: ##:::: ##: ##: ##: ##:::: ##: ########:: #####::::
; ##. ####: ##...::::::: ##:::: ##: ##: ##: ##:::: ##: ##.. ##::: ##. ##:::
; ##:. ###: ##:::::::::: ##:::: ##: ##: ##: ##:::: ##: ##::. ##:: ##:. ##::
; ##::. ##: ########:::: ##::::. ###. ###::. #######:: ##:::. ##: ##::. ##:
;..::::..::........:::::..::::::...::...::::.......:::..:::::..::..::::..::

;Network function
;Data will be formated as follows and sent:
; A message consisting of a list, whose first element is the color of the player in turn
; Then every piece will be an element coded as follows:
;  Piece = (name(String) color(String) x-position(Int) y-position(Int)
; Hence the message will look like this:
;  list(color(String) name(String) color(String) x-position(Int) y-position(Int) name(String)
;  color(String) x-position(Int) y-position(Int) etc...


(provide (all-defined-out))

;Worldstate -> List
;converts the worldstate to a message
(define (worldstate-into-message state)
  (define turn (list (worldstate-turncolor (second state))))
  (list (first state)(append turn (list-of-pieces-into-readable-list (worldstate-pieces (second state))))))

;List(Piece) -> List
;converts a list of pieces to a list coded in name, color, x-cord, y-cord
(define (list-of-pieces-into-readable-list listofPieces)
  (define Message '())
  (cond [(empty? listofPieces) Message]
        [else 
         (append(append-element (posn-y(piece-position(first listofPieces)))
                                (append-element  (posn-x(piece-position (first listofPieces)))
                                                 (append-element  (piece-color (first listofPieces))
                                                                  (append-element  (piece-name (first listofPieces))Message))))
                (list-of-pieces-into-readable-list (rest listofPieces)))]))

;List -> list(string worldstate)
;Create a message from a worldstate
(define (create-worldstate-from-message message)
  (list (first message) (worldstate (create-alive-pieces-from-message(cdr (second message))) null
                                    (first (second message)))))

;List -> List
;Create a list of pieces from a message
(define (create-alive-pieces-from-message message)
  (cond [(empty? message) '()]
        [else 
         (append(list(piece(first message) (second message)
                           (make-posn (third message)
                                      (fourth message)) (decide-the-rank (first message)) (decide-with-picture
                                                                                           (first message) (second message))
                                                        (decide-with-selected-picture (first message) (second message))))
                (create-alive-pieces-from-message (list-tail message 4)))]))

; String String -> image
;Function to provide the right image for a given piece
(define (decide-with-picture name color)
  (cond[(and(equal? name "rat") (equal? PLAYER1 color)) rat-PLAYER1-image]
       [(and(equal? name "cat") (equal? PLAYER1 color)) cat-PLAYER1-image]
       [(and(equal? name "dog") (equal? PLAYER1 color)) dog-PLAYER1-image]
       [(and(equal? name "wolf") (equal? PLAYER1 color)) wolf-PLAYER1-image]
       [(and(equal? name "leopard") (equal? PLAYER1 color)) leopard-PLAYER1-image]
       [(and(equal? name "tiger") (equal? PLAYER1 color)) tiger-PLAYER1-image]
       [(and(equal? name "lion") (equal? PLAYER1 color)) lion-PLAYER1-image]
       [(and(equal? name "elephant") (equal? PLAYER1 color)) elephant-PLAYER1-image]
       [(and(equal? name "den") (equal? PLAYER1 color)) den-PLAYER1-image]
       [(and(equal? name "trap") (equal? PLAYER1 color)) trap-PLAYER1-image]
       
       [(and(equal? name "rat") (equal? PLAYER2 color)) rat-PLAYER2-image]
       [(and(equal? name "cat") (equal? PLAYER2 color)) cat-PLAYER2-image]
       [(and(equal? name "dog") (equal? PLAYER2 color)) dog-PLAYER2-image]
       [(and(equal? name "wolf") (equal? PLAYER2 color)) wolf-PLAYER2-image]
       [(and(equal? name "leopard") (equal? PLAYER2 color)) leopard-PLAYER2-image]
       [(and(equal? name "tiger") (equal? PLAYER2 color)) tiger-PLAYER2-image]
       [(and(equal? name "lion") (equal? PLAYER2 color)) lion-PLAYER2-image]
       [(and(equal? name "elephant") (equal? PLAYER2 color)) elephant-PLAYER2-image]
       [(and(equal? name "den") (equal? PLAYER2 color)) den-PLAYER2-image]
       [(and(equal? name "trap") (equal? PLAYER2 color)) trap-PLAYER2-image]
       [else "unknown"]))

; String String -> image
;Function to provide the right selected-image for a given piece
(define (decide-with-selected-picture name color)
  (cond[(and(equal? name "rat") (equal? PLAYER1 color)) rat-PLAYER1-image-prev]
       [(and(equal? name "cat") (equal? PLAYER1 color)) cat-PLAYER1-image-prev]
       [(and(equal? name "dog") (equal? PLAYER1 color)) dog-PLAYER1-image-prev]
       [(and(equal? name "wolf") (equal? PLAYER1 color)) wolf-PLAYER1-image-prev]
       [(and(equal? name "leopard") (equal? PLAYER1 color)) leopard-PLAYER1-image-prev]
       [(and(equal? name "tiger") (equal? PLAYER1 color)) tiger-PLAYER1-image-prev]
       [(and(equal? name "lion") (equal? PLAYER1 color)) lion-PLAYER1-image-prev]
       [(and(equal? name "elephant") (equal? PLAYER1 color)) elephant-PLAYER1-image-prev]
       [(and(equal? name "den") (equal? PLAYER1 color)) den-PLAYER1-image]
       [(and(equal? name "trap") (equal? PLAYER1 color)) trap-PLAYER1-image]
       
       [(and(equal? name "rat") (equal? PLAYER2 color)) rat-PLAYER2-image-prev]
       [(and(equal? name "cat") (equal? PLAYER2 color)) cat-PLAYER2-image-prev]
       [(and(equal? name "dog") (equal? PLAYER2 color)) dog-PLAYER2-image-prev]
       [(and(equal? name "wolf") (equal? PLAYER2 color)) wolf-PLAYER2-image-prev]
       [(and(equal? name "leopard") (equal? PLAYER2 color)) leopard-PLAYER2-image-prev]
       [(and(equal? name "tiger") (equal? PLAYER2 color)) tiger-PLAYER2-image-prev]
       [(and(equal? name "lion") (equal? PLAYER2 color)) lion-PLAYER2-image-prev]
       [(and(equal? name "elephant") (equal? PLAYER2 color)) elephant-PLAYER2-image-prev]
       [(and(equal? name "den") (equal? PLAYER2 color)) den-PLAYER2-image]
       [(and(equal? name "trap") (equal? PLAYER2 color)) trap-PLAYER2-image]
       [else "unknown"]))

;String -> Int
;Function to return the rank of a piece
(define (decide-the-rank name)
(cond[(equal? name "rat") 1]
       [(equal? name "cat") 2 ]
       [(equal? name "dog") 3]
       [(equal? name "wolf") 4]
       [(equal? name "leopard") 5 ]
       [(equal? name "tiger") 6 ]
       [(equal? name "lion") 7 ]
       [(equal? name "elephant") 8 ]
       [else 0]))

;worldstate list -> state
;check if the received message is legal (by length)
(define (receive-message currentState message)
  (cond[(and(and(list? message) (= (length message) 2)) (= (remainder(-(length (second message)) 1) 4) 0))
        (create-worldstate-from-message message)]
       [else currentState]))

;Sendet den Server eine Message die den current worldstate beinhaltet, sowie eine weitere Nachricht
;Functions send the server a message, which contains the current state as well as a token 
(define (send-mesage-to-server state)
  (display "send message to server")
  (display (worldstate-into-message state))
  (make-package  state (worldstate-into-message state)))
