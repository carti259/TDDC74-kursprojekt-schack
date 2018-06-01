#|
Names: Carl Tidén, Tom Tersén
Last edited: 2018-05-31
Purpose of file:
 To define various help-functions used in different parts of the code.
|#

#lang racket
(provide (all-defined-out))

; This list is used to detect draws.
(define draw-combinations
  (list '("BK" "WK")
        '("BK" "WB" "WK")
        '("BB" "BK" "WK")
        '("BK" "WH" "WK")
        '("BH" "BK" "WK")))

; Returns the coordinates of the tile with id in a pair,
; first row is indexed as 1, same goes for columns.
(define (get-tile-coordinates tile-id)
  (cons (get-tile-row tile-id) (get-tile-col tile-id)))

(define (get-tile-col tile-id)
  (if (zero? (remainder tile-id 8))
      8
      (remainder tile-id 8)))

(define (get-tile-row tile-id)
  (if (zero? (remainder tile-id 8))
      (quotient tile-id 8)
      (+ (quotient tile-id 8) 1)))

(define (same-row? from to)
  (= (get-tile-row from) (get-tile-row to)))

(define (same-col? from to)
  (= (get-tile-col from) (get-tile-col to)))

; The below method returns a pair containing info
; about the direction of the suggested move.
; Possibilities are:
;  (0 0)   - same position
;  (-1 0)  - up
;  (-1 1)  - diagonal up right
;  (0 1)   - right
;  (1 1)   - diagonal down right
;  (1 0)   - down
;  (1 -1)  - diagonal down left
;  (0 -1)  - left
;  (-1 -1) - diagonal up left
(define (direction from to)
  (cond
    [(and (same-row? from to)
          (same-col? from to))
     ; We're standing still.
     (cons 0 0)]
    [(same-row? from to)
     ; We're moving sideways.
     (cons 0 (/ (- (get-tile-col to) (get-tile-col from))
                (abs (- (get-tile-col to) (get-tile-col from)))))]
    [(same-col? from to)
     ; We're moving up or down.
     (cons (/ (- (get-tile-row to) (get-tile-row from))
              (abs (- (get-tile-row to) (get-tile-row from)))) 0)]
    [else
     ; We're moving diagonally.
     (cons (/ (- (get-tile-row to) (get-tile-row from))
              (abs (- (get-tile-row to) (get-tile-row from))))
           (/ (- (get-tile-col to) (get-tile-col from))
              (abs (- (get-tile-col to) (get-tile-col from)))))]))

; This function returns the distance of a straight or diagonal move.
(define (distance from to)
  (cond
    [(is-diagonal-move? from to)
     ; The move is diagonal!
     (abs (- (get-tile-row from) (get-tile-row to)))]
    [(is-straight-move? from to)
     ; The move is straight!
     (cond
       [(same-row? from to)
        (abs (- (get-tile-col from) (get-tile-col to)))]
       [(same-col? from to)
        (abs (- (get-tile-row from) (get-tile-row to)))])]))

; This function takes two coordinates as numbers,
; and returns the corresponding tile-id (also a number).
(define (tile-id-from-coordinates row col)
  (+ (* (- row 1) 8) col))

(define (is-diagonal-move? from to)
  ; Return true if the move is diagonal!
  (= (abs (- (get-tile-row from) (get-tile-row to)))
     (abs (- (get-tile-col from) (get-tile-col to)))))

(define (is-straight-move? from to)
  ; Return true if the move is straight!
  (or (same-row? from to)
      (same-col? from to)))

; This function takes a tile-id and returns the x-pixel-coordinate
; at where a piece icon is to be placed at.
(define (tile-to-bitmap-x tile-id)
  (+ 14 (* 60 (- (get-tile-col tile-id) 1))))

(define (tile-to-bitmap-y tile-id)
  (+ 14 (* 60 (- (get-tile-row tile-id) 1))))

; This function takes a tile-id and returns the x-pixel-coordinate of
; the tile.
(define (highlight-x tile-id)
  (* 60 (- (get-tile-col tile-id) 1)))

(define (highlight-y tile-id)
  (* 60 (- (get-tile-row tile-id) 1)))

; This function takes x- and y-pixel-coordinates and returns the
; tile-id.
(define (pixel-pos-to-tile-id x-pos y-pos)
  (tile-id-from-coordinates
   (+ (quotient y-pos 60) 1)
   (+ (quotient x-pos 60) 1)))

; Returns #t for light, #f for dark.
(define (get-tile-color tile-id)
  (cond
    [(even? (get-tile-row tile-id))
     (even? (get-tile-col tile-id))]
    [else
     (odd? (get-tile-col tile-id))]))

; Returns #t if both tiles are of the same color.
(define (same-colored-tiles? tile-id-1 tile-id-2)
  (eqv? (get-tile-color tile-id-1) (get-tile-color tile-id-2)))