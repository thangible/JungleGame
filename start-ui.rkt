#lang racket
(require racket/include)
(require racket/gui/base)

(require "client.rkt")
(require "server.rkt")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define IP-ADDRESS "")
(define PORT "")
(define PLAYER-NAME "")
(define exist (instantiate dialog% ("Internet Protocol")))
(define background "Images/jungle-start.jpg")

;Create a new Frame
(define new-frame (new frame%
                       [label "Jungle"]
                       [width 846]
                       [height 566]))
(send new-frame show #t)


;Function to start the game in a computer
(define (single)
  (send new-frame show #f)
  (start-jungle "Jungle-Alternately"))

;Function to start a server
(define (start-server-ui)
  (display "start server")
  (start-server))

;Debug function for activating the network communication
(define (multiplayer-test)
  (send new-frame show #f)
  (start-two-worlds))

;2.te Debug function for activating the network communication
(define (multiplayer n ip port)
  (send new-frame show #f)
  (if(equal? ip "DEBUG")
  (start-two-worlds)
  (start-multiplayer-jungle n ip port)))

;New canva to check the mouse event and enable new functions without building more buttons
(define my-canvas%
  (class canvas%
    (define/override (on-event event)
      (let ((x (if (eq? 'left-down (send event get-event-type)) (send event get-x) 0))
            (y (if (eq? 'left-down (send event get-event-type)) (send event get-y) 0)))
        (cond [(and (< x 750) (> x 430) (< y 270) (> y 220)) (single)]
              [(and (< x 750) (> x 430) (< y 335) (> y 275)) (send lan show #t)]
              [(and (< x 750) (> x 430) (< y 385) (> y 335)) (start-server-ui)]
              [(and (< x 750) (> x 430) (< y 435) (> y 385)) (help)]
              )))
    (super-new)))

;draw an image
(define mycanvas (new my-canvas% 
                      [parent new-frame]
                      [paint-callback (lambda (c d)
                                        (send d draw-bitmap (read-bitmap background) 0 0) )]))


;Dialog to enter the ip adress and name
(define lan (instantiate dialog% ("Enter IP-Address")))
(define name (new text-field% [parent lan] [label "Name"]))
(define field-ip-address (new text-field% [parent lan] [label "IP-Address"]))
(define field-port (new text-field% [parent lan] [label "Port"]))


(define join-button (new button% [parent lan] [label "join"] [callback
                                          (lambda (b e)
                                            (begin (set! IP-ADDRESS (send field-ip-address get-value))
                                                   (set! PORT (send field-port get-value))
                                                   (set! PLAYER-NAME (send name get-value))
                                                    (send lan show #f)
                                                    (multiplayer PLAYER-NAME IP-ADDRESS PORT)
                                                    ))]))

;Mesage box for the rules of the game
(define (help)
  (message-box "JUNGLE-RULES"
"Movement:
Players alternate moves with Red moving first.
During their turn, a player must move. All pieces
can move one square horizontally or vertically
(not diagonally). A piece may not move into its
own den. Animals of either side can move into
and out of any trap square.

There are special rules related to the water
squares:
   - The rat is the only animal that may go onto
     a water square.
   - The lion and tiger can jump over a river
     horizontally or vertically. They jump from a
     square on one edge of the river to the next
     non-water square on the other side. If that
     square contains an enemy piece of equal or
     lower rank, the lion or tiger capture it as
     part of their jump. A jumping move is blocked
    (not permitted) if a rat of either color currently
     occupies any of the intervening water squares.

Capturing:
Animals capture opponent pieces by killing
them (the attacking piece replaces the captured piece
on its square; the captured piece is removed from the game).
 A piece can capture any enemy piece that has the same or
lower rank, with the following exceptions:

    - The rat can kill (capture) an elephant, but only
      from a land square, not from a water square.
    - Many published versions of the game say the rat kills
      the elephant by running into its ear and gnawing into its brain.
    - A rat in the water is invulnerable to capture by any piece on land.
     (Therefore a rat in the water can only be killed by another rat in the water.)
    - A piece that enters one of the opponent's trap squares is reduced in rank to 0.
      Thus the trapped piece may be captured by the defending side with any piece,
      regardless of rank. A trapped piece has its normal rank restored when it
      exits an opponent's trap square."))
