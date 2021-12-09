#lang racket

(require lang/posn)
(require 2htdp/image)
(require "helper-functions.rkt")
(require "Structs.rkt")
;DATA DEFINITION--------------------------------------------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------------------------------------------------------
(provide (all-defined-out))

;:'######:::'#######::'##::: ##::'######::'########::::'###::::'##::: ##:'########::'######::
;'##... ##:'##.... ##: ###:: ##:'##... ##:... ##..::::'## ##::: ###:: ##:... ##..::'##... ##:
; ##:::..:: ##:::: ##: ####: ##: ##:::..::::: ##:::::'##:. ##:: ####: ##:::: ##:::: ##:::..::
; ##::::::: ##:::: ##: ## ## ##:. ######::::: ##::::'##:::. ##: ## ## ##:::: ##::::. ######::
; ##::::::: ##:::: ##: ##. ####::..... ##:::: ##:::: #########: ##. ####:::: ##:::::..... ##:
; ##::: ##: ##:::: ##: ##:. ###:'##::: ##:::: ##:::: ##.... ##: ##:. ###:::: ##::::'##::: ##:
;. ######::. #######:: ##::. ##:. ######::::: ##:::: ##:::: ##: ##::. ##:::: ##::::. ######::
;:......::::.......:::..::::..:::......::::::..:::::..:::::..::..::::..:::::..::::::......:::

;Tile-size // Basics metric for all sizes
(define TILE-SIZE 100)
(define BOARD-GAP  5)

;IMAGE-SCALE adjust how big the animal image is. E.g 0.8 => 80% as big as the orignal image
(define IMAGE-SCALE 0.8)

;TILE COLOR and TILE Background color
(define TILE-COLOR "Khaki")
(define TILE-BACKGROUND "Dark Khaki")

;Screen width and height
;(define CURRENT-WINDOW-WIDTH (send a-window get-width))
(define SCREEN-WIDTH  (* TILE-SIZE 7))
(define SCREEN-HEIGHT (* TILE-SIZE 9))

;The dear positions to win
(define DEAR-POS-PLAYER1 (make-posn 3 0))
(define DEAR-POS-PLAYER2 (make-posn 3 8))

;player colors
(define PLAYER1 "red")
(define PLAYER2 "black")

;VIsual constants
(define MT-SCENE (empty-scene SCREEN-WIDTH SCREEN-HEIGHT))

(define LAND (tile TILE-COLOR TILE-BACKGROUND TILE-SIZE BOARD-GAP))
(define WATER (water-tile "white" "darkblue" TILE-SIZE BOARD-GAP))

(define HIGHLIGHTED-TILE (tile "lightgrey" TILE-BACKGROUND TILE-SIZE BOARD-GAP))
(define HIGHLIGHTED-SELECTED (square (- TILE-SIZE 2) "outline" "green"))
(define ALL-LAND (beside LAND LAND LAND LAND LAND LAND LAND))
(define LAND-WATER (beside LAND WATER WATER LAND WATER WATER LAND))

(define EMPTY-BOARD (above ALL-LAND ALL-LAND ALL-LAND LAND-WATER LAND-WATER LAND-WATER ALL-LAND ALL-LAND ALL-LAND))

;Help Text//Rules

(define HELP "1. Players have to move.
2. All pieces can move one square.
3. Lion and tiger can jump over the river.
4 The rat can go into the river
5. A rat can block the jump")
  

;implement images (assuming each image is 100x100 px)
(define rat-PLAYER1-image     (bitmap "Images/rat_red.png"))
(define cat-PLAYER1-image     (bitmap "Images/cat_red.png"))
(define dog-PLAYER1-image     (bitmap "Images/dog_red.png"))
(define wolf-PLAYER1-image    (bitmap "Images/wolf_red.png"))
(define leopard-PLAYER1-image (bitmap "Images/leopard_red.png"))
(define tiger-PLAYER1-image   (bitmap "Images/tiger_red.png"))
(define lion-PLAYER1-image    (bitmap "Images/lion_red.png"))
(define elephant-PLAYER1-image(bitmap "Images/elephant_red.png"))

(define rat-PLAYER2-image     (bitmap "Images/rat_black.png"))
(define cat-PLAYER2-image     (bitmap "Images/cat_black.png"))
(define dog-PLAYER2-image     (bitmap "Images/dog_black.png"))
(define wolf-PLAYER2-image    (bitmap "Images/wolf_black.png"))
(define leopard-PLAYER2-image (bitmap "Images/leopard_black.png"))
(define tiger-PLAYER2-image   (bitmap "Images/tiger_black.png"))
(define lion-PLAYER2-image    (bitmap "Images/lion_black.png"))
(define elephant-PLAYER2-image(bitmap "Images/elephant_black.png"))

(define trap-PLAYER1-image (bitmap "Images/trap_red.png"))
(define trap-PLAYER2-image (bitmap "Images/trap_black.png"))
(define den-PLAYER1-image (bitmap "Images/den_red.png"))
(define den-PLAYER2-image (bitmap "Images/den_black.png"))

;implement the preview image
(define rat-PLAYER1-image-prev     (bitmap "Images/rat_red_50.png"))
(define cat-PLAYER1-image-prev     (bitmap "Images/cat_red_50.png"))
(define dog-PLAYER1-image-prev     (bitmap "Images/dog_red_50.png"))
(define wolf-PLAYER1-image-prev    (bitmap "Images/wolf_red_50.png"))
(define leopard-PLAYER1-image-prev (bitmap "Images/leopard_red_50.png"))
(define tiger-PLAYER1-image-prev   (bitmap "Images/tiger_red_50.png"))
(define lion-PLAYER1-image-prev    (bitmap "Images/lion_red_50.png"))
(define elephant-PLAYER1-image-prev(bitmap "Images/elephant_red_50.png"))

(define rat-PLAYER2-image-prev     (bitmap "Images/rat_black_50.png"))
(define cat-PLAYER2-image-prev     (bitmap "Images/cat_black_50.png"))
(define dog-PLAYER2-image-prev     (bitmap "Images/dog_black_50.png"))
(define wolf-PLAYER2-image-prev    (bitmap "Images/wolf_black_50.png"))
(define leopard-PLAYER2-image-prev (bitmap "Images/leopard_black_50.png"))
(define tiger-PLAYER2-image-prev   (bitmap "Images/tiger_black_50.png"))
(define lion-PLAYER2-image-prev    (bitmap "Images/lion_black_50.png"))
(define elephant-PLAYER2-image-prev(bitmap "Images/elephant_black_50.png"))


;'####:'##::: ##:'####:'########:'####::::'###::::'##:::::::'####:'########::::'###::::'########:'####::'#######::'##::: ##:
;. ##:: ###:: ##:. ##::... ##..::. ##::::'## ##::: ##:::::::. ##::..... ##::::'## ##:::... ##..::. ##::'##.... ##: ###:: ##:
;: ##:: ####: ##:: ##::::: ##::::: ##:::'##:. ##:: ##:::::::: ##:::::: ##::::'##:. ##::::: ##::::: ##:: ##:::: ##: ####: ##:
;: ##:: ## ## ##:: ##::::: ##::::: ##::'##:::. ##: ##:::::::: ##::::: ##::::'##:::. ##:::: ##::::: ##:: ##:::: ##: ## ## ##:
;: ##:: ##. ####:: ##::::: ##::::: ##:: #########: ##:::::::: ##:::: ##::::: #########:::: ##::::: ##:: ##:::: ##: ##. ####:
;: ##:: ##:. ###:: ##::::: ##::::: ##:: ##.... ##: ##:::::::: ##::: ##:::::: ##.... ##:::: ##::::: ##:: ##:::: ##: ##:. ###:
;'####: ##::. ##:'####:::: ##::::'####: ##:::: ##: ########:'####: ########: ##:::: ##:::: ##::::'####:. #######:: ##::. ##:
;....::..::::..::....:::::..:::::....::..:::::..::........::....::........::..:::::..:::::..:::::....:::.......:::..::::..::

;all posible positions in the board 7x9
(define ALL-POSITIONS (list (make-posn 0 0) (make-posn 1 0) (make-posn 2 0) (make-posn 3 0) 
                            (make-posn 4 0) (make-posn 5 0) (make-posn 6 0) (make-posn 7 0) 
                            (make-posn 0 1) (make-posn 1 1) (make-posn 2 1) (make-posn 3 1) 
                            (make-posn 4 1) (make-posn 5 1) (make-posn 6 1) (make-posn 7 1) 
                            (make-posn 0 2) (make-posn 1 2) (make-posn 2 2) (make-posn 3 2) 
                            (make-posn 4 2) (make-posn 5 2) (make-posn 6 2) (make-posn 7 2) 
                            (make-posn 0 3) (make-posn 1 3) (make-posn 2 3) (make-posn 3 3)
                            (make-posn 4 3) (make-posn 5 3) (make-posn 6 3) (make-posn 7 3) 
                            (make-posn 0 4) (make-posn 1 4) (make-posn 2 4) (make-posn 3 4) 
                            (make-posn 4 4) (make-posn 5 4) (make-posn 6 4) (make-posn 7 4) 
                            (make-posn 0 5) (make-posn 1 5) (make-posn 2 5) (make-posn 3 5) 
                            (make-posn 4 5) (make-posn 5 5) (make-posn 6 5) (make-posn 7 5)
                            (make-posn 0 6) (make-posn 1 6) (make-posn 2 6) (make-posn 3 6) 
                            (make-posn 4 6) (make-posn 5 6) (make-posn 6 6) (make-posn 7 6) 
                            (make-posn 0 7) (make-posn 1 7) (make-posn 2 7) (make-posn 3 7) 
                            (make-posn 4 7) (make-posn 5 7) (make-posn 6 7) (make-posn 7 7)
                            (make-posn 0 8) (make-posn 1 8) (make-posn 2 8) (make-posn 3 8) 
                            (make-posn 4 8) (make-posn 5 8) (make-posn 6 8) (make-posn 7 8)))


;initialize all the figures
(define rat-w     (piece "rat"     PLAYER1 (make-posn  0 2) 1 rat-PLAYER1-image rat-PLAYER1-image-prev))
(define cat-w     (piece "cat"     PLAYER1 (make-posn  5 1) 2 cat-PLAYER1-image cat-PLAYER1-image-prev))
(define dog-w     (piece "dog"     PLAYER1 (make-posn  1 1) 3 dog-PLAYER1-image dog-PLAYER1-image-prev))
(define wolf-w    (piece "wolf"    PLAYER1 (make-posn  4 2) 4 wolf-PLAYER1-image wolf-PLAYER1-image-prev))
(define leopard-w (piece "leopard" PLAYER1 (make-posn  2 2) 5 leopard-PLAYER1-image leopard-PLAYER1-image-prev))
(define tiger-w   (piece "tiger"   PLAYER1 (make-posn  6 0) 6 tiger-PLAYER1-image tiger-PLAYER1-image-prev))
(define lion-w    (piece "lion"    PLAYER1 (make-posn  0 0) 7 lion-PLAYER1-image lion-PLAYER1-image-prev))
(define elephant-w(piece "elephant"PLAYER1 (make-posn  6 2) 8 elephant-PLAYER1-image elephant-PLAYER1-image-prev))

(define rat-b     (piece "rat"     PLAYER2 (make-posn  6 6) 1 rat-PLAYER2-image rat-PLAYER2-image-prev))
(define cat-b     (piece "cat"     PLAYER2 (make-posn  1 7) 2 cat-PLAYER2-image cat-PLAYER2-image-prev))
(define dog-b     (piece "dog"     PLAYER2 (make-posn  5 7) 3 dog-PLAYER2-image dog-PLAYER2-image-prev))
(define wolf-b    (piece "wolf"    PLAYER2 (make-posn  2 6) 4 wolf-PLAYER2-image wolf-PLAYER2-image-prev))
(define leopard-b (piece "leopard" PLAYER2 (make-posn  4 6) 5 leopard-PLAYER2-image leopard-PLAYER2-image-prev))
(define tiger-b   (piece "tiger"   PLAYER2 (make-posn  0 8) 6 tiger-PLAYER2-image tiger-PLAYER2-image-prev))
(define lion-b    (piece "lion"    PLAYER2 (make-posn  6 8) 7 lion-PLAYER2-image lion-PLAYER2-image-prev))
(define elephant-b(piece "elephant"PLAYER2 (make-posn  0 6) 8 elephant-PLAYER2-image elephant-PLAYER2-image-prev))

(define trap1 (piece "trap" PLAYER1 (make-posn 2 0) 0 trap-PLAYER1-image trap-PLAYER1-image))
(define trap2 (piece "trap" PLAYER1 (make-posn 4 0) 0 trap-PLAYER1-image trap-PLAYER1-image))
(define trap3 (piece "trap" PLAYER1 (make-posn 3 1) 0 trap-PLAYER1-image trap-PLAYER1-image))
(define trap4 (piece "trap" PLAYER2 (make-posn 2 8) 0 trap-PLAYER2-image trap-PLAYER2-image))
(define trap5 (piece "trap" PLAYER2 (make-posn 4 8) 0 trap-PLAYER2-image trap-PLAYER2-image))
(define trap6 (piece "trap" PLAYER2 (make-posn 3 7) 0 trap-PLAYER2-image trap-PLAYER2-image))

(define den-red   (piece "den" PLAYER1 (make-posn 3 0) 0 den-PLAYER1-image den-PLAYER1-image))
(define den-black (piece "den" PLAYER2 (make-posn 3 8) 0 den-PLAYER2-image den-PLAYER2-image))

;List of all alive figures
(define alive (list rat-w cat-w dog-w wolf-w leopard-w tiger-w lion-w elephant-w
                    rat-b cat-b dog-b wolf-b leopard-b tiger-b lion-b elephant-b
                     den-red den-black))

;List of all traps
(define TRAPS (list trap1 trap2 trap3 trap4 trap5 trap6))

;List of all water tiles
(define water-list (list (make-posn  1 3) (make-posn  1 4) (make-posn  1 5)
                     (make-posn  2 3) (make-posn  2 4) (make-posn  2 5)
                     (make-posn  4 3) (make-posn  4 4) (make-posn  4 5)
                     (make-posn  5 3) (make-posn  5 4) (make-posn  5 5)))
