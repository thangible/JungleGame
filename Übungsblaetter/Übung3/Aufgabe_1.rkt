#lang racket
(require 2htdp/image)
(require racket/include)
(require 2htdp/universe)

;Spiel des Lebens

(define GRID 10)
(define TILE-SIZE 10)


(define DEAD (square TILE-SIZE "outline" "black"))
(define ALIVE (square TILE-SIZE "solid" "black"))


;function creates a matrix with size (size x size)
(define (make-matrix size)
  (make-list size (make-list size 0)))

;function determines if a cell x y is dead
(define (alive? matrix x y)
  (cond [(equal? (list-ref (list-ref matrix y) x) 1)
         #t]
        [else  #f]))

;function returns value at cell x,y
;in case x or y undertermined (out or frame), return 0
(define (get-value-in-matrix matrix x y)
  (cond [(< x 0) 0]
        [(< y 0) 0]
        [(>= x GRID) 0]
        [(>= y GRID) 0]
        [else (list-ref (list-ref matrix y) x)]))


;Draw matrix
(define (draw matrix)
  (apply above(map (lambda (x) (vertical x)) matrix)))


;helping function for draw matrix
(define (vertical list)
  (apply beside (map (lambda (x) (if (= 0 x) DEAD
                                     ALIVE))
                   list)))


;Funktion, die für einen beliebigen Index des Spielzustands,
;z.B. (x = 20, y = 10), die Werte der 8er-Nachbarschaft ermittel

;Esfunktioniert nicht!!!!!!!!!!!!!!! Ich möchte gerne wissen warum! (versuche mal mit start-matrix mit (x y) = (1 1) = 1.
;count-neighbours matrix 1 0 return 0!
; (define (count-neigbours matrix x y)
;   (define sum 0)
;   (for ([i '(0 -1 1)][j '(0 -1 1)])
;     (set! sum (+ sum (get-value-in-matrix matrix (+ x i) (+ y j)))))
;   (display sum)
;   sum
;   )


(define (count-neigbours matrix x y)
  (apply +
         (map (lambda (pos) (get-value-in-matrix matrix (first pos) (last pos)))
              (get-pos-neigbours matrix x y))))

(define (get-pos-neigbours matrix x y)
  (define possible-neighbours (list
                               (list -1  1) (list 0  1) (list 1  1)
                                (list -1  0)             (list 1  0)
                                (list -1 -1) (list 0 -1) (list 1  -1)))
  (map (lambda (pos) (list  (+ x (first pos))
                            (+ y (last  pos))))
       possible-neighbours))


;Funktion, die die Werte dieser 8er-Nachbarschaft erhält, und gemäß den Spiel-
;regeln den Folgezustand des Automaten am  übergebenen Index bestimmt
(define (react-to-neighbours matrix x y)
  (define currentState (get-value-in-matrix matrix x y))
  (define live-neighbors (count-neigbours matrix x y))
  (cond [(and (= currentState 0) (= live-neighbors 3)) 1]
        [(and (= currentState 1) (< live-neighbors 2)) 0]
        [(and (= currentState 1) (> live-neighbors 3)) 0]
        [(and (= currentState 1) (or (= live-neighbors 2)
                                     (= live-neighbors 3))) 1]
        [else currentState]))

;eine Funktion, die einen kompletten Spielzustand gemäß der Regeln in einen neuen
;Spielzustand  überführen
(define (update-state matrix)
  (define size (length matrix))
  (append (map (lambda (y) (update-row y matrix))
               (sequence->list (in-range size)))))

;Hilfsfunktion für update
(define (update-row y matrix)
  (define size (length matrix))
  (map (lambda (x) (react-to-neighbours matrix x y))
       (sequence->list (in-range size))))


;----------MAIN---------------
(define start-matrix
  (list '(0 0 0 0 0 0 0 0 0 0)
        '(0 0 0 0 0 0 0 0 0 0)
        '(0 0 0 0 0 0 0 0 0 0)
        '(0 0 0 0 0 0 0 0 0 0)
        '(0 0 0 0 1 0 0 0 0 0)
        '(0 0 0 0 1 0 0 0 0 0)
        '(0 0 0 0 1 0 0 0 0 0)
        '(0 0 0 0 0 0 0 0 0 0)
        '(0 0 0 0 0 0 0 0 0 0)
        '(0 0 0 0 0 0 0 0 0 0)
        '(0 0 0 0 0 0 0 0 0 0)
        ))


(big-bang start-matrix
  (to-draw draw)
  (on-tick update-state 4))    

(count-neigbours (make-matrix GRID) 20 20)

