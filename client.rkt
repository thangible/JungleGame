#lang racket

;IMPORT---------------------------------------------------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------------------------------------------------------
(require racket/block)
(require 2htdp/image)
(require racket/include)
(require 2htdp/universe)
(require racket/mpair)
(require racket/vector)
(require data/gvector)
(require lang/posn)
(require 2htdp/planetcute)
(require 2htdp/universe)
(require racket/trace)
(require "helper-functions.rkt")
(require "Constants.rkt")
(require "Structs.rkt")
(require "Network.rkt")

;EXPORT---------------------------------------------------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------------------------------------------------------
(provide start-jungle start-multiplayer-jungle start-two-worlds)

;
;'##::::'##::::'###::::'####:'##::: ##::::'########:'##::::'##:'##::: ##::'######::'########:'####::'#######::'##::: ##::'######::
; ###::'###:::'## ##:::. ##:: ###:: ##:::: ##.....:: ##:::: ##: ###:: ##:'##... ##:... ##..::. ##::'##.... ##: ###:: ##:'##... ##:
; ####'####::'##:. ##::: ##:: ####: ##:::: ##::::::: ##:::: ##: ####: ##: ##:::..::::: ##::::: ##:: ##:::: ##: ####: ##: ##:::..::
; ## ### ##:'##:::. ##:: ##:: ## ## ##:::: ######::: ##:::: ##: ## ## ##: ##:::::::::: ##::::: ##:: ##:::: ##: ## ## ##:. ######::
; ##. #: ##: #########:: ##:: ##. ####:::: ##...:::: ##:::: ##: ##. ####: ##:::::::::: ##::::: ##:: ##:::: ##: ##. ####::..... ##:
; ##:.:: ##: ##.... ##:: ##:: ##:. ###:::: ##::::::: ##:::: ##: ##:. ###: ##::: ##:::: ##::::: ##:: ##:::: ##: ##:. ###:'##::: ##:
; ##:::: ##: ##:::: ##:'####: ##::. ##:::: ##:::::::. #######:: ##::. ##:. ######::::: ##::::'####:. #######:: ##::. ##:. ######::
;..:::::..::..:::::..::....::..::::..:::::..:::::::::.......:::..::::..:::......::::::..:::::....:::.......:::..::::..:::......:::

;; Start the Game
;; Start the Game in multiplayer mode (Using network)
(define (start-multiplayer-jungle n ip p)
  (big-bang (list "wait" (worldstate alive null PLAYER1)) 
    (to-draw render)
    (on-receive receive-message)
    (state #t)
    (name n)
    (on-mouse handle-mouse)
    (register ip)
    (port p)))

;Start the game in local mode
(define (start-jungle n)
  (big-bang (list "local" (worldstate alive null PLAYER1)) 
    (to-draw render)
    (state #t)
    (name n)
    (on-mouse handle-mouse)
    (stop-when last-world? last-scene)))

;Function to launch two worlds in the local computer
(define (start-two-worlds)
(launch-many-worlds 
  (start-multiplayer-jungle "Player1" LOCALHOST 9092)
  (start-multiplayer-jungle "Player2" LOCALHOST 9092)
))

;worldstate int int mouse-event -> worldstate
;react to the mouse event, it has 2 cases of local player and multiplayer mode.
(define (handle-mouse w x y me)
  (define message (first w))
  (define state (second w))
  (cond
;-----------------------------------------------------------------------------------------------------
;------------------------Multiplayer-----------------------------------------------------------------
;-----------------------------------------------------------------------------------------------------
    ;-----------------MESSAGE: TURN-------------------------------------------------------------------
    [(and(mouse=? me "button-down") (equal? message "turn"))
     ; If one figure was already chosen, the legality of the new position will be checked.
     ; If the test passes, a message encoding a new board configuration will be sent to server 
     (cond
       [(and (check-if-figure-is-selected? (worldstate-selected state))
             (is-legal-move? (worldstate-selected state)
                             (mouse-position-to-posn x y TILE-SIZE)
                             (worldstate-turncolor state)
                             (worldstate-pieces state)))
        (send-mesage-to-server (list "move-done" (move-figure-to-position (worldstate-selected state)
                                                                          (mouse-position-to-posn x y TILE-SIZE)
                                                                          (worldstate-pieces state)
                                                                          (worldstate-turncolor state))))]
       ;In case user re-click the selected figure, it is counted as unselect the figure
       [(and (check-if-figure-is-selected? (worldstate-selected state))
             (equal? (piece-position(worldstate-selected state))
                     (mouse-position-to-posn x y TILE-SIZE)))
        (list message (worldstate (append (worldstate-pieces state)
                                          (list (worldstate-selected state)))
                                  null
                                  (worldstate-turncolor state)))]
            
       ;If no figure was selected before, no message will be send to the server.
       ;Locally the worldstate change its state in term of "selected"
       [(not(check-if-figure-is-selected? (worldstate-selected state)))
        (list "turn" (pick-up-a-figure (worldstate-pieces state) (worldstate-pieces state)
                                       (worldstate-selected state) (worldstate-turncolor state) x y))]
       ;If a figure was selected, but the new position is not legal,
       ;then no action is done - worldstate remains the same
       [else w]
       )]
    ;-----------------MESSAGE: LOST/WON---------------------------------------------------------
    ;In case one player wins, a message will be sent to server saying that a restart is needed
    [(and(mouse=? me "button-down")(equal? message "lost"))
     (send-mesage-to-server(list "restart" (second w)))
     ]
    [(and(mouse=? me "button-down")(equal? message "won"))
     (send-mesage-to-server(list "restart" (second w)))
     ]
     
;-----------------------------------------------------------------------------------------------------
;------------------------local-play-----------------------------------------------------------------
;-----------------------------------------------------------------------------------------------------
    ;-----------------MESSAGE: LOCAL---------------------------------------------------------
    ;When playing locally, the same procedure applies, but just without communication with server
    [(and(mouse=? me "button-down") (local? message))   
     (cond [(and (check-if-figure-is-selected? (worldstate-selected state))
                 (not (equal? (piece-position(worldstate-selected state))
                              (mouse-position-to-posn x y TILE-SIZE))))
            (list message (move-figure-to-position (worldstate-selected state)
                                                   (mouse-position-to-posn x y TILE-SIZE)
                                                   (worldstate-pieces state)
                                                   (worldstate-turncolor state)))]
           ;if the user wishes to unselect
           [(and (check-if-figure-is-selected? (worldstate-selected state))
                 (equal? (piece-position(worldstate-selected state))
                         (mouse-position-to-posn x y TILE-SIZE)))
            (list message (worldstate (append (worldstate-pieces state)
                                              (list (worldstate-selected state)))
                                      null
                                      (worldstate-turncolor state)))]
           ;Else no figure is selected, we pick up the figure
           [else (list message (pick-up-a-figure (worldstate-pieces state) (full-copy (worldstate-pieces state))
                                                 (worldstate-selected state) (worldstate-turncolor state) x y))])]
    ;If a player wins, the renderer will be informed
    [(and(check-player-one-win (worldstate-pieces state)) (local? message))
     (list "local-P1-win" (second w))]    
    [(and(check-player-two-win (worldstate-pieces state)) (local? message))
     (list "local-P2-win" (second w))]    
    [else w]))

;String -> Boolean
;Function tests if the game is played locally
(define (local? message)
  (equal? message "local"))

;String -> Boolean
;Function test if the message is about the winning scenerios
(define (last-world? w)
  (if (or (equal? (first w) "local-P1-win")
          (equal? (first w) "local-P2-win"))
      #t
      #f))

;piece int int color -> Boolean
;check if the figure has x y position and the color plus is not a trap
(define (contains-figure figure x y color)
  (and (equal? (mouse-position-to-posn x y) (piece-position figure))
       (equal? (piece-color figure) color))
       )

;piece int int color -> Boolean
;check if the figure has x y position and the color; and is not a trap
(define (contains-playable-figure? figure x y color)
  (and (equal? (mouse-position-to-posn x y TILE-SIZE)
               (piece-position figure))
       (equal? (piece-color figure) color)
       (not (equal? (piece-name figure) "trap"))
       (not (equal? (piece-name figure) "den"))))

;list list piece x y -> worldstate
;Recursive function to return a changed worldstate whose ListOfPieces does not have selected figure
(define (pick-up-a-figure ListOfPIeces OriginalList SelectedPiece color x y)
  (cond
    ;Base case: if list is empty, return then the original list
    [(empty? ListOfPIeces) (worldstate OriginalList null color)]
    ;if the first piece has pos x y with given color, then we pick it up (return a worldstate with list of piece without it)
    [(contains-playable-figure? (first ListOfPIeces) x y color)
     (choose-figure (first ListOfPIeces) x y OriginalList color)]
    ;else we continue to check the rest of the list
    [else (pick-up-a-figure (rest ListOfPIeces) OriginalList SelectedPiece color x y)]))

;piece int int list color -> worldstate
;return a world state whose ListOfPieces doesnt contain the given figure.
(define (choose-figure figure x y newW color)
  (define newW2 (full-copy newW))
  (worldstate (remove (first(member figure newW)) newW2) figure color))

;piece -> Boolean
;Checks is a figure is already selected
(define (check-if-figure-is-selected? SelectedPiece)
  (piece? SelectedPiece))

;piece pos list color -> worldstate
;Function move the selected piece to the new position, give its accoriding worldstate
(define (move-figure-to-position  SelectedPiece newPosition ListOfPieces Player-Color)
  (define newPlayer (changePlayer Player-Color))
  (if
   ;if the move is legal
   (is-legal-move? SelectedPiece newPosition Player-Color ListOfPieces)
   ;and if there is other piece in new position (must be enemy piece, bc legal move check friend alr)
   (if (check-Piece-on-field ListOfPieces newPosition)
       ;then fight
       (fight SelectedPiece (get-Piece-on-Position ListOfPieces newPosition) ListOfPieces newPosition)
       ;else just move. we add the selected piece again to the list of piece and selected become null, change color
       (worldstate
        (append ListOfPieces
                (list(piece
                      (piece-name SelectedPiece)
                      (piece-color SelectedPiece)
                      newPosition
                      (piece-rank SelectedPiece)
                      (piece-image SelectedPiece)
                      (piece-selected-image SelectedPiece))
                     ))
        null
        newPlayer))
   ;if the move is illegal, return the same worldstate
   (worldstate ListOfPieces SelectedPiece Player-Color)))

;'##:::::::'########::'######::::::'###::::'##::::::::::'##::::'##::'#######::'##::::'##:'########::::'########::'##::::'##:'##:::::::'########::'######::
; ##::::::: ##.....::'##... ##::::'## ##::: ##:::::::::: ###::'###:'##.... ##: ##:::: ##: ##.....::::: ##.... ##: ##:::: ##: ##::::::: ##.....::'##... ##:
; ##::::::: ##::::::: ##:::..::::'##:. ##:: ##:::::::::: ####'####: ##:::: ##: ##:::: ##: ##:::::::::: ##:::: ##: ##:::: ##: ##::::::: ##::::::: ##:::..::
; ##::::::: ######::: ##::'####:'##:::. ##: ##:::::::::: ## ### ##: ##:::: ##: ##:::: ##: ######:::::: ########:: ##:::: ##: ##::::::: ######:::. ######::
; ##::::::: ##...:::: ##::: ##:: #########: ##:::::::::: ##. #: ##: ##:::: ##:. ##:: ##:: ##...::::::: ##.. ##::: ##:::: ##: ##::::::: ##...:::::..... ##:
; ##::::::: ##::::::: ##::: ##:: ##.... ##: ##:::::::::: ##:.:: ##: ##:::: ##::. ## ##::: ##:::::::::: ##::. ##:: ##:::: ##: ##::::::: ##:::::::'##::: ##:
; ########: ########:. ######::: ##:::: ##: ########:::: ##:::: ##:. #######::::. ###:::: ########:::: ##:::. ##:. #######:: ########: ########:. ######::
;........::........:::......::::..:::::..::........:::::..:::::..:::.......::::::...:::::........:::::..:::::..:::.......:::........::........:::......:::
;Function decides if a piece can be in a certain tile
;piece pos color list -> Boolean
(define (is-legal-move? SelectedPiece newPosition color ListOfPieces)
  (define pieceRank (piece-rank SelectedPiece))
  (define pieceName (piece-name SelectedPiece))
  (define piecePosition (piece-position SelectedPiece))
  (cond
    ;cant go to your own den, bro
    [(is-home-den? newPosition color) #f]
    ;an animal in the team is already there
    [(is-a-friend-there? newPosition color ListOfPieces SelectedPiece) #f]
    ;only mouse can go into water
    [(and (is-pos-water? newPosition)(not (equal? pieceRank 1))) #f]
    ;mouse can not attack elephant from water
    [(is-rat-attacking-elephant-from-water? SelectedPiece newPosition color ListOfPieces) #f]
    ;spring is legal for lion and tiger
    [(and (not(is-a-normal-move? piecePosition newPosition))
          (not(is-a-legal-spring? piecePosition newPosition pieceName ListOfPieces))) #f] 
    [else #t]
    ))

;Function decides if a pos is water;
;as default, water-list will be given as second argument
;pos water-list -> boolean 
(define (is-pos-water? pos [waters water-list])
  (if (empty? waters) #f
      (or (equal? pos (first waters))
          (is-pos-water? pos (rest waters))))
  )


;Function decides if a animal of a certain color is in a position
;list pos color -> boolean
(define (is-a-friend-there? pos color ListOfPieces SelectedPiece)
  (cond [( equal?(check-Piece-on-field ListOfPieces pos) #t) 
      (cond [(equal? (piece-color (get-Piece-on-Position ListOfPieces pos)) (piece-color SelectedPiece)) #t]
           [else #f])]
      [else #f]))
 
;pos color -> Boolean
;Function decides if a position is a den of the same team
(define (is-home-den? pos color)
  (cond[ (and (equal? pos (make-posn 3 0)) (equal? color PLAYER1))
         #t]
       [(and (equal? pos (make-posn 3 8)) (equal? color PLAYER2))
             #t]
      [else #f]
      )
  )

; piece pos color list -> Boolean
;Funktion decides if it is a case of rat attacking elephant from water
(define (is-rat-attacking-elephant-from-water? SelectedPiece newPosition color ListOfPieces)
  (define elephant (get-piece "elephant" (changePlayer color) ListOfPieces))
  (cond
    ;the piece is not in water
    [(not (is-pos-water? (piece-position SelectedPiece))) #f]
    ;the piece is not  rat
    [(not (equal? (piece-name SelectedPiece) "rat")) #f]
    ;elephant is not  alive
    [(null? elephant) #f]
    ;all the above condition and the elephant is in the newPosition
    [(equal? (piece-position elephant) newPosition) #t]
    ;else it is not that case
    [else #f]))

; pos pos String list -> Boolean
;Function decides if a spring is legal 
(define (is-a-legal-spring? oldPosition newPosition pieceName ListOfPieces)
  (define possibleVerticalSprings   (list
                                     (list (make-posn 2 2)  (make-posn 2 6))
                                     (list (make-posn 1 2)  (make-posn 1 6))
                                     (list (make-posn 4 2)  (make-posn 4 6))
                                     (list (make-posn 5 2)  (make-posn 5 6))
                                     ))
  (define possibleHorizontalSprings (list 
                                     (list (make-posn 0 3)  (make-posn 3 3))
                                     (list (make-posn 3 3)  (make-posn 6 6))
                                     (list (make-posn 0 4)  (make-posn 3 4))
                                     (list (make-posn 3 4)  (make-posn 6 4))
                                     (list (make-posn 0 5)  (make-posn 3 5))
                                     (list (make-posn 3 5)  (make-posn 6 5))))
  (if (member pieceName '("lion" "tiger"))
      (or
       ;loops throught the possible vertical list and check if oldpos, newpos match; and also check if there is a rat between
       (not(empty? (filter (lambda (spring)
                             (and (member oldPosition spring)
                                 (member newPosition spring)
                                 (no-rat-between-jump? oldPosition newPosition #t ListOfPieces)))
                           possibleVerticalSprings)))
       ;loops throught the possible horizontal list and check if oldpos, newpos match; and also check if there is a rat between
       (not (empty? (filter(lambda (spring)
                             (and (member oldPosition spring)
                                  (member newPosition spring)
                                  (no-rat-between-jump? oldPosition newPosition #f ListOfPieces)))
                           possibleHorizontalSprings)))
       ;if piece is not lion and tiger then it is not a legal spring
       ) #f))

;pos pos Boolean list -> Boolean
;Function check if there is no rat between 2 jump points
(define (no-rat-between-jump? oldPosition newPosition isVerticalSpring ListOfPieces)
  (define rat-1 (get-piece "rat" PLAYER1 ListOfPieces))
  (define rat-2 (get-piece "rat" PLAYER2 ListOfPieces))
  (cond
    ;if all rats are ded
    [(and (null? rat-1) (null? rat-2)) #t ]
    ;if only rat 2 alive
    [(and (null? rat-1) (not (null? rat-2)))
     (this-rat-not-between-jump? oldPosition newPosition rat-2 ListOfPieces isVerticalSpring)]
    ;if only rat 1 alive
    [(and (not (null? rat-1)) (null? rat-2))
     (this-rat-not-between-jump? oldPosition newPosition rat-1 ListOfPieces isVerticalSpring)]
    ;all rats are alive!!
    [else
     (and (this-rat-not-between-jump? oldPosition newPosition rat-1 ListOfPieces isVerticalSpring)
          (this-rat-not-between-jump? oldPosition newPosition rat-2 ListOfPieces isVerticalSpring))]))

;pos pos color list Boolean -> Boolean
;Funktion check if a certain rat is in the way
(define (this-rat-not-between-jump? oldPosition newPosition rat ListOfPieces isVerticalSpring)
   (define y-rat (posn-y (piece-position rat)))
   (define x-rat (posn-x (piece-position rat)))
   (if isVerticalSpring
      (let ([x (posn-x oldPosition)]
            [min-y (min (posn-y oldPosition) (posn-y newPosition))]                 
            [max-y (max (posn-y oldPosition) (posn-y newPosition))])
        ;rat is in the same column and between the 2 position
        (if (and (apply < (list min-y y-rat max-y)) (equal? x-rat x)) #f #t))
      (let ([y (posn-y oldPosition)]
            [min-x (min (posn-x oldPosition) (posn-x newPosition))]
            [max-x (max (posn-x oldPosition) (posn-x newPosition))])
        ;rat is in the same row and between the 2 position
        (if (and (apply < (list min-x x-rat max-x)) (equal? y-rat y)) #f #t))))

;pos pos -> Boolean
;Function check if the movement is normal (only one step)
(define (is-a-normal-move? oldPosition newPosition)
  (let ([x-diff (abs (- (posn-x oldPosition) (posn-x newPosition)))]
        [y-diff (abs (- (posn-y oldPosition) (posn-y newPosition)))])
    (if (or (and (= x-diff 1) (= y-diff 0))
            (and (= x-diff 0) (= y-diff 1)))
        #t
        #f)))

;-----------HELPING FUNCTION FOR LEGAL MOVE RULES-------------------------------------------------------------------
;string color list -> null/piece
;Function get a piece out of a list of piece
(define (get-piece name color ListOfPieces)
  (let ([piece (filter (lambda (a-piece)
                         (and (equal? (piece-name a-piece) name )
                              (equal? (piece-color a-piece) color)))
                       ListOfPieces)])
    (if (empty? piece) null
        (first piece))))

;list pos -> boolean
; true if piece is on field todo implement
(define (check-Piece-on-field ListOfPieces pos)
   (cond [(empty? ListOfPieces) #f]
        [(equal? (piece-position (first ListOfPieces)) pos) #t]
        [else
         (check-Piece-on-field (rest ListOfPieces) pos)]))

;list position -> piece
;returns a piece if the position contains a piece, else null
(define (get-Piece-on-Position ListOfPieces pos)
  (cond [(empty? ListOfPieces) null]
        [(equal? (piece-position (first ListOfPieces)) pos) (first ListOfPieces)]
        [else
         (get-Piece-on-Position (rest ListOfPieces) pos)]))

;PLAYER1/PLAYER2 -> PLAYER1/PLAYER2
;returns the next player
(define (changePlayer currentColor)
   (cond[(equal? currentColor PLAYER1)
         PLAYER2]
        [else PLAYER1]))

;---------------------------------------------------------------------------------------------------
;piece piece list pos -> worldstate
;This function strictly assumes that the selectedPiece and targetPiece have to be of different colors
;fight function let two animal fight and decides the outcome (new worldstate)
(define (fight selectedPiece targetPiece ListOfPieces pos)
  (define newPlayer (changePlayer (piece-color selectedPiece)))
  (if (wonFight? selectedPiece targetPiece)
      ;if the fight is won, the targetPiece will be removed
      (worldstate(append (deleteItem ListOfPieces targetPiece)
                           (list (piece (piece-name selectedPiece) (piece-color selectedPiece)  pos                             
                                        (piece-rank selectedPiece)
                                        (piece-image selectedPiece) (piece-selected-image selectedPiece)))) null newPlayer)  
       (worldstate ListOfPieces null newPlayer)))
     

;piece piece -> Boolean
;checks if selectedPiece won the fight
(define (wonFight? selectedPiece targetPiece)
  (cond[(onEnemyTrap? targetPiece TRAPS) #t]
       [else
        (cond[(and (isRat? selectedPiece)
                   (equal? (piece-rank targetPiece) 8)) #t]
             ; Rule question Rat can always kill the rat
             ;[(and(isRat? targetPiece)(equal? (piece-rank selectedPiece) 8))
             ; #f]
             [(or(> (piece-rank selectedPiece) (piece-rank targetPiece)) (= (piece-rank selectedPiece) (piece-rank targetPiece)))
              #t]
             [else #f])]))

;piece -> Boolean
;Checks if Piece is a rat
(define (isRat? selectedPiece)
  (equal? (piece-rank selectedPiece) 1))

;piece -> Boolean
;Checks if a Piece is on a enemy trap
(define (onEnemyTrap? targetPiece TRAPS)
 (cond [(empty? TRAPS) #f]
        [(and(equal? (piece-position targetPiece) (piece-position (first TRAPS)))
              (not(equal?(piece-color targetPiece) (piece-color (first TRAPS)))))
                      #t]
        [else
         (onEnemyTrap? targetPiece (rest TRAPS))]))

;list -> Boolean
;Check if Player One won the game
(define (check-player-one-win ListOfPieces)
  (not (member den-black ListOfPieces)))

;list -> Boolean
;Check if Player TWO won the game
(define (check-player-two-win ListOfPieces)
  (not(member den-red ListOfPieces)))

;
;:'######:::'########:::::'###::::'########::'##::::'##:'####::'######:::::'########:'##::::'##:'##::: ##::'######::'########:'####::'#######::'##::: ##::'######::
;'##... ##:: ##.... ##:::'## ##::: ##.... ##: ##:::: ##:. ##::'##... ##:::: ##.....:: ##:::: ##: ###:: ##:'##... ##:... ##..::. ##::'##.... ##: ###:: ##:'##... ##:
; ##:::..::: ##:::: ##::'##:. ##:: ##:::: ##: ##:::: ##:: ##:: ##:::..::::: ##::::::: ##:::: ##: ####: ##: ##:::..::::: ##::::: ##:: ##:::: ##: ####: ##: ##:::..::
; ##::'####: ########::'##:::. ##: ########:: #########:: ##:: ##:::::::::: ######::: ##:::: ##: ## ## ##: ##:::::::::: ##::::: ##:: ##:::: ##: ## ## ##:. ######::
; ##::: ##:: ##.. ##::: #########: ##.....::: ##.... ##:: ##:: ##:::::::::: ##...:::: ##:::: ##: ##. ####: ##:::::::::: ##::::: ##:: ##:::: ##: ##. ####::..... ##:
; ##::: ##:: ##::. ##:: ##.... ##: ##:::::::: ##:::: ##:: ##:: ##::: ##:::: ##::::::: ##:::: ##: ##:. ###: ##::: ##:::: ##::::: ##:: ##:::: ##: ##:. ###:'##::: ##:
;. ######::: ##:::. ##: ##:::: ##: ##:::::::: ##:::: ##:'####:. ######::::: ##:::::::. #######:: ##::. ##:. ######::::: ##::::'####:. #######:: ##::. ##:. ######::
;:......::::..:::::..::..:::::..::..:::::::::..:::::..::....:::......::::::..:::::::::.......:::..::::..:::......::::::..:::::....:::.......:::..::::..:::......:::
;worldstate -> void
(define (render wstate)
  (define message (first wstate))
  (define state (second wstate))
  ;Locally, the color of the player in turn will be shown
  (cond[(and (equal? (worldstate-turncolor state) PLAYER1)
             (equal? message "local")) 
         (above (text "Player 1" 16 "red")
               (tiles+traps+figures+highlight (worldstate-pieces state)
                                              (worldstate-selected state)
                                              MT-SCENE TRAPS))]
       [(and (equal? (worldstate-turncolor state) PLAYER2)
             (equal? message "local")) 
        (above (text "Player 2" 16 "black")
               (tiles+traps+figures+highlight (worldstate-pieces state)
                                              (worldstate-selected state)
                                              MT-SCENE TRAPS))]
       ;Checks if one of the players has won
       [(equal? message "lost")(endscreen-player "You lost." "black" (bitmap "Images/lose.png"))]
       [(equal? message "won") (endscreen-player "You won." "black" (bitmap "Images/win.png"))]
       ;Else it is in multiplayer mode, it will show if the players should wait or play
       [else (above
              (text (string-append message (string-append " "(worldstate-turncolor state)))
                    16
                    (worldstate-turncolor state))
              (tiles+traps+figures+highlight (worldstate-pieces state)
                                             (worldstate-selected state)
                                              MT-SCENE TRAPS))]))

;. -> image
;Function to draw the rules and the overview for a selected piece
(define (overview-and-help)
  (text HELP 16 "black"))


;worldstate -> image
;Function to draw the last worldstate
(define (last-scene wstate)
  (define message (first wstate))
  (if (equal? message "local-P2-win")
      (above (endscreen-player "Player 2 wins" PLAYER2 (bitmap "Images/win.png")) (endscreen-player "Player 1 loses" PLAYER2 (bitmap "Images/lose.png")))
      (above (endscreen-player "Player 1 wins" PLAYER2 (bitmap "Images/win.png")) (endscreen-player "Player 2 loses" PLAYER2 (bitmap "Images/lose.png")))))

;list color image -> image
;Function to show Win/Lose
(define (endscreen-player message player-color image)
  (beside image (text/font message 28 player-color #f "modern" "normal" "normal" #f)))

;list piece image list -> image
;Function to draw the board, figures and hightlight
(define (tiles+traps+figures+highlight pieces SelectedPiece scene traps )
  (draw-pieces pieces (tiles+traps+highlight SelectedPiece traps pieces scene)))

;piece list list image -> image
;Funcion to draw the board and figures
(define (tiles+traps+highlight SelectedPiece traps pieces scene)
  (draw-traps traps(tiles+highlight  SelectedPiece pieces scene)))

;piece list image -> image
;Function to highlight the pieces
(define (tiles+highlight SelectedPiece pieces scene)
  (highlight SelectedPiece pieces (draw-tiles scene)))

;image -> image
;Function to draw the board
(define (draw-tiles scene)
(place-image EMPTY-BOARD (/ SCREEN-WIDTH 2) (/ SCREEN-HEIGHT 2) scene))

;list image -> image
;Funktion to draw the figures
(define (draw-pieces pieces scene)
  (cond [(empty? pieces) scene]
        [else (draw-one-piece (first pieces)                          
                       (draw-pieces (rest pieces) scene))]))

;piece image -> image
;Funktion to place an image of piece depending on the given position of piece
(define (draw-one-piece piece scene)
  (place-image (scale IMAGE-SCALE (scale (/ TILE-SIZE (image-width (piece-image piece))) (piece-image piece)))               
               (posn-x (position-to-center-cords (piece-position piece) TILE-SIZE))
               (posn-y (position-to-center-cords (piece-position piece) TILE-SIZE))
               scene))

;lists piece -> image
;Function to draw the traps
(define (draw-traps traps scene)
  (cond [(empty? traps) scene]
        [else (draw-one-trap (first traps)                          
                             (draw-traps (rest traps) scene))]))

;piece image -> image
;Function to draw one trap, depending on its given position
(define (draw-one-trap trap scene)
  (place-image (scale IMAGE-SCALE (scale (/ TILE-SIZE (image-width (piece-image trap))) (piece-image trap)))               
               (posn-x (position-to-center-cords (piece-position trap) TILE-SIZE))
               (posn-y (position-to-center-cords (piece-position trap) TILE-SIZE))
               scene))


;piece list image -> image
;Function highlights all posible move of a piece
(define (highlight SelectedPiece pieces scene)
  (if(piece? SelectedPiece)
     (draw-highlights(recursive-highlighted-tiles ALL-POSITIONS SelectedPiece pieces)
                     (place-image (scale IMAGE-SCALE (scale
                                                      (/ TILE-SIZE (image-width (piece-selected-image SelectedPiece)))
                                                      (piece-selected-image SelectedPiece)))               
                                  (posn-x (position-to-center-cords (piece-position SelectedPiece) TILE-SIZE))
                                  (posn-y (position-to-center-cords (piece-position SelectedPiece) TILE-SIZE))
                                  scene))
     scene))
  
  
;list image -> image  
;Helping Function for hightlight
;Recursively highlights all the possible move of the selected piece
(define (draw-highlights list-of-posn scene)
 (cond [(empty? list-of-posn) scene]
        [else (hightlight-a-tile (first list-of-posn)                          
                       (draw-highlights (rest list-of-posn) scene))]))

;list piece list -> list
;Function gives all the posible positions of a piece
(define (recursive-highlighted-tiles list-of-posn SelectedPiece ListOfPieces)
  (cond [(empty? list-of-posn) '()]
        [(is-legal-move? SelectedPiece
                         (first list-of-posn)
                         (piece-color SelectedPiece)
                         ListOfPieces)
         (append (list(first list-of-posn))
                 (recursive-highlighted-tiles (rest list-of-posn) SelectedPiece  ListOfPieces))]
        [else                          
         (append (recursive-highlighted-tiles (rest list-of-posn) SelectedPiece  ListOfPieces))]))
  
     
;pos image -> image     
;Function to highlight a single tile
(define (hightlight-a-tile pos scene)
  (define x (posn-x(position-to-center-cords pos TILE-SIZE)))
  (define y (posn-y(position-to-center-cords pos TILE-SIZE)))
  (place-image HIGHLIGHTED-TILE x y scene))

  

;'########::'##::::'##:'##::: ##::::::'####::::'########:'########::'######::'########:
; ##.... ##: ##:::: ##: ###:: ##:::::'##. ##:::... ##..:: ##.....::'##... ##:... ##..::
; ##:::: ##: ##:::: ##: ####: ##:::::. ####::::::: ##:::: ##::::::: ##:::..::::: ##::::
; ########:: ##:::: ##: ## ## ##:::::'####:::::::: ##:::: ######:::. ######::::: ##::::
; ##.. ##::: ##:::: ##: ##. ####::::'##. ##'##:::: ##:::: ##...:::::..... ##:::: ##::::
; ##::. ##:: ##:::: ##: ##:. ###:::: ##:. ##:::::: ##:::: ##:::::::'##::: ##:::: ##::::
; ##:::. ##:. #######:: ##::. ##::::. ####. ##:::: ##:::: ########:. ######::::: ##::::
;..:::::..:::.......:::..::::..::::::....::..:::::..:::::........:::......::::::..:::::
;RUN--FOREST--RUN-----------------------------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------------------------------------------









