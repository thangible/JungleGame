#lang racket
(require lang/posn)
(require 2htdp/image)

;EXPORT functions for other modules ---------------------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------------------------------------------------------
(provide position-to-center-cords mouse-position-to-posn full-copy deleteItem posn-add append-element water-tile tile)

;
;'##::::'##:'########:'##:::::::'########:::::'########:'##::::'##:'##::: ##::'######::'########:'####::'#######::'##::: ##::'######::
; ##:::: ##: ##.....:: ##::::::: ##.... ##:::: ##.....:: ##:::: ##: ###:: ##:'##... ##:... ##..::. ##::'##.... ##: ###:: ##:'##... ##:
; ##:::: ##: ##::::::: ##::::::: ##:::: ##:::: ##::::::: ##:::: ##: ####: ##: ##:::..::::: ##::::: ##:: ##:::: ##: ####: ##: ##:::..::
; #########: ######::: ##::::::: ########::::: ######::: ##:::: ##: ## ## ##: ##:::::::::: ##::::: ##:: ##:::: ##: ## ## ##:. ######::
; ##.... ##: ##...:::: ##::::::: ##.....:::::: ##...:::: ##:::: ##: ##. ####: ##:::::::::: ##::::: ##:: ##:::: ##: ##. ####::..... ##:
; ##:::: ##: ##::::::: ##::::::: ##::::::::::: ##::::::: ##:::: ##: ##:. ###: ##::: ##:::: ##::::: ##:: ##:::: ##: ##:. ###:'##::: ##:
; ##:::: ##: ########: ########: ##::::::::::: ##:::::::. #######:: ##::. ##:. ######::::: ##::::'####:. #######:: ##::. ##:. ######::
;..:::::..::........::........::..::::::::::::..:::::::::.......:::..::::..:::......::::::..:::::....:::.......:::..::::..:::......:::

;pos int -> pos
;Function converts the tile position to the coordination of its center in the board
;This is for the sake of placing image
(define (position-to-center-cords position tile-size)
  (make-posn (+ (* tile-size (posn-x position)) (/ tile-size 2 ))
             (+ (* tile-size (posn-y position)) (/ tile-size 2 ))))

;int int int -> pos
;Function converts the mouse position (in Coordination) to the tile position
(define (mouse-position-to-posn x y tile-size)
  (make-posn (quotient x tile-size) (quotient y tile-size)))

;list -> list
;Funcion makes a copy of a list
(define (full-copy list)
  (if (null? list) 
      '() 
      (if (list? list) 
          (cons (full-copy (car list)) (full-copy (cdr list)))
          list)))

;list item -> list
;Function deletes an item from a list
(define (deleteItem list item)
  (cond ((null? list)
         '())
        ((equal? item (car list))
         (cdr list))
        (else
         (cons (car list) 
               (deleteItem (cdr list) item)))))

;pos pos -> pos
;Function adds two position together
(define (posn-add posn1 posn2)
  (make-posn (+ (posn-x posn1) (posn-x posn2)) (+ (posn-y posn1) (posn-y posn2))))

;Element List -> List
;Function appends an elment at the end of the list
(define (append-element elem lst)
  (foldr cons (list elem) lst))

;String String int int -> image
;Functions adjust the visuality of a tile
(define (tile color backgroundcolor tile-size board-gap)
  (overlay 
   (overlay
    (overlay(rectangle (* (- tile-size board-gap) 0.8) (- tile-size board-gap)  "solid" color)
            (rectangle (- tile-size board-gap) (* (- tile-size board-gap) 0.8)  "solid" color))
    (underlay/align/offset
     "right" "bottom"
     (underlay/align/offset
      "left" "bottom"
      (underlay/align/offset
       "right" "top"
       (underlay/align/offset
        "left" "top"
        (rectangle (- tile-size board-gap) (- tile-size board-gap) "solid" backgroundcolor)
        0 0
        (circle   (/(- tile-size (* tile-size 0.8))2)  "solid" color))
       0 0
       (circle (/(- tile-size (* tile-size 0.8))2) "solid" color))
      0 0
      (circle (/(- tile-size (* tile-size 0.8))2) "solid" color))
     0 0
     (circle (/(- tile-size (* tile-size 0.8))2) "solid" color)))
   (rectangle tile-size tile-size "solid" backgroundcolor)))


;String String int int -> image
;Functions adjust the visuality of a water tile
(define (water-tile color backgroundcolor tile-size board-gap)
  (overlay/align/offset "center" "center"
                        (above/align "left"
                                     ;(for ([i (in-range 1 (/ tile-size (* board-gap 2)))])
                                     ;(row-of-waves color backgroundcolor tile-size board-gap)))
                                     ;(row-of-waves color backgroundcolor (* tile-size 0.8) board-gap)
                                     (row-of-waves color backgroundcolor (* tile-size 0.8) (* board-gap 1.2))
                                     (row-of-waves color backgroundcolor (* tile-size 0.8) (* board-gap 1.2))
                                     (row-of-waves color backgroundcolor (* tile-size 0.8) (* board-gap 1.2)))
                0 0 (rectangle tile-size tile-size "solid" backgroundcolor)))

;String String int int -> image
;Functions creates the row of waves
(define (row-of-waves color backgroundcolor tile-size board-gap)
  (beside/align "top"
                (calc-waves color backgroundcolor tile-size board-gap)
                (calc-waves color backgroundcolor tile-size board-gap)
                (calc-waves color backgroundcolor tile-size board-gap)))

;String String int int -> image
;Helping function for row-of-waves
(define (calc-waves color backgroundcolor tile-size board-gap)
  (overlay/offset (circle (/ tile-size  board-gap) "solid" backgroundcolor)
                   0 -3
                   (circle (/ tile-size board-gap) "solid" color)))
