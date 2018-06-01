#|
Names: Carl Tidén, Tom Tersén
Last edited: 2018-05-31
Purpose of file:
 To define a base class for pieces and subclasses for different piece types.
 The piece types have individual behaviour, e.g. ways they can be moved.
|#

#lang racket/gui
(require "utils.rkt")
(provide pawn% bishop% knight% rook% queen% king%)

; The base class for all pieces. Common properties and methods for multiple
; piece types are defined within.
(define piece%
  (class object%
    (init-field name color parent-board pos number-of-moves)

    (define/public (get-name)
      name)

    (define/public (get-color)
      color)

    (define/public (get-pos)
      pos)

    (define/public (moves)
      number-of-moves)

    ; This method sets the 'pos' property and increases number of moves by 1.
    (define/public (move to-tile-id)
      (set! pos to-tile-id)
      (set! number-of-moves (+ 1 number-of-moves)))

    ; This method works just like the above, but it decreases 'number-of-moves'
    ; by 1.
    (define/public (move-back to-tile-id)
      (set! pos to-tile-id)
      (set! number-of-moves (- number-of-moves 1)))

    ; The below method is used by all piece types (except knight).
    ; It checks whether or not there is a piece in the way, preventing the
    ; suggested move to be made. 'endpoint-add' is used by pawns (they can not
    ; strike in forward direction).
    ; IN: number, #t/#f.
    ; OUT: #t/#f.
    (define/public (no-piece-in-the-way to-tile-id [endpoint-add 0])
      ; Get the direction pair of the desired move.
      (define move-direction (direction pos to-tile-id))

      (let iterate-tiles ([i 1])
        (cond
          [(= i (+ (distance pos to-tile-id) endpoint-add))
           ; No pieces were in the way.
           #t]
          [(not (equal? (send parent-board tile-occupied-by-color
                              (tile-id-from-coordinates
                               (+ (get-tile-row pos)
                                  (* i (car move-direction)))
                               (+ (get-tile-col pos)
                                  (* i (cdr move-direction)))))
                        "E"))
           ; The looped-over tile is not empty, we can not move
           ; over it.
           #f]
          [else
           ; Check the next tile!
           (iterate-tiles (+ i 1))])))

    (super-new)))

; Pawn class.
(define white-pawn-bitmap (read-bitmap "../img/pawn-white.png"))
(define black-pawn-bitmap (read-bitmap "../img/pawn-black.png"))
(define pawn%
  (class piece%
    (inherit-field color number-of-moves pos parent-board)
    (field
     [value 1])

    (define/public (get-bitmap)
      (if (equal? color "W")
          white-pawn-bitmap
          black-pawn-bitmap))

    (define/public (get-value) value)

    (define/private (other-color)
      (if (equal? color "W")
          "B"
          "W"))

    (define/private (way)
      (if (equal? color "W")
          1
          -1))

    ; The below method exists for every piece type. It is being called before
    ; the board executes the 'execute-move-piece' method, in order to check
    ; if the requested move can be made in the current situation with the type
    ; of piece selected.
    ; We can always presume the to-tile is "on the board".
    ; IN: number.
    ; OUT: #t/#f.
    (define/public (can-move to-tile-id)
      (cond
        [(or (and (= number-of-moves 0)
                  (= (get-tile-col pos) (get-tile-col to-tile-id))
                  (< 0 (* (-
                           (get-tile-row pos)
                           (get-tile-row to-tile-id)) (way)) 3)
                  (send this no-piece-in-the-way to-tile-id 1))
             ; (Above.) It's the first move, we're going straight ahead,
             ; it's 1 or 2 steps and the path is clear.
             (and (= (get-tile-col pos) (get-tile-col to-tile-id))
                  (= 1 (* (-
                           (get-tile-row pos)
                           (get-tile-row to-tile-id)) (way)))
                  (send this no-piece-in-the-way to-tile-id 1))
             ; It's not the first move, and we're going one step straight
             ; ahead on a clear path.
             (and (= (* (- (get-tile-row pos)
                           (get-tile-row to-tile-id)) (way)) 1)
                  (is-diagonal-move? pos to-tile-id)
                  (equal? (send parent-board tile-occupied-by-color to-tile-id)
                          (other-color))))
         ; We're moving one step diagonally forward to where there is a
         ; piece of the other color.
         #t]
        [else #f]))

    (define/public (get-symbol)
      (if (equal? color "W")
          "♙"
          "♟"))

    (super-new)))

; Bishop class.
(define white-bishop-bitmap (read-bitmap "../img/bishop-white.png"))
(define black-bishop-bitmap (read-bitmap "../img/bishop-black.png"))
(define bishop%
  (class piece%
    (inherit-field color number-of-moves pos parent-board)
    (field
     [value 3])

    (define/public (get-bitmap)
      (if (equal? color "W")
          white-bishop-bitmap
          black-bishop-bitmap))

    (define/public (get-value) value)

    (define/public (can-move to-tile-id)
      (cond
        [(and
          (is-diagonal-move? pos to-tile-id)
          (send this no-piece-in-the-way to-tile-id)
          (not (equal? (send parent-board tile-occupied-by-color to-tile-id)
                       color)))
         ; The move is diagonal, there's no piece in the way
         ; and no piece of our color is on the tile we're trying to
         ; move to.
         #t]
        [else #f]))

    (define/public (get-symbol)
      (if (equal? color "W")
          "♗"
          "♝"))

    (super-new)))

; Knight class.
(define white-knight-bitmap (read-bitmap "../img/knight-white.png"))
(define black-knight-bitmap (read-bitmap "../img/knight-black.png"))
(define knight%
  (class piece%
    (inherit-field color number-of-moves pos parent-board)
    (field
     [value 3])

    (define/public (get-bitmap)
      (if (equal? color "W")
          white-knight-bitmap
          black-knight-bitmap))

    (define/public (get-value) value)

    (define/public (can-move to-tile-id)
      (cond
        [(and (or (and (= (abs (- (get-tile-col pos)
                                  (get-tile-col to-tile-id))) 2)
                       (= (abs (- (get-tile-row pos)
                                  (get-tile-row to-tile-id))) 1))
                  ; Above code checks if row distance is 2 and column
                  ; distance is 1, below checks the opposite.
                  (and (= (abs (- (get-tile-col pos)
                                  (get-tile-col to-tile-id))) 1)
                       (= (abs (- (get-tile-row pos)
                                  (get-tile-row to-tile-id))) 2)))
              (not (equal? (send parent-board tile-occupied-by-color to-tile-id)
                           color)))
         ; It's a valid horse move!
         #t]
        [else #f]))

    (define/public (get-symbol)
      (if (equal? color "W")
          "♘"
          "♞"))

    (super-new)))

; Rook class.
(define white-rook-bitmap (read-bitmap "../img/rook-white.png"))
(define black-rook-bitmap (read-bitmap "../img/rook-black.png"))
(define rook%
  (class piece%
    (inherit-field color number-of-moves pos parent-board)
    (field
     [value 6])

    (define/public (get-bitmap)
      (if (equal? color "W")
          white-rook-bitmap
          black-rook-bitmap))

    (define/public (get-value) value)

    (define/public (can-move to-tile-id)
      (cond
        [(and
          (is-straight-move? pos to-tile-id)
          (send this no-piece-in-the-way to-tile-id)
          (not (equal? (send parent-board tile-occupied-by-color to-tile-id)
                       color)))
         ; The move is straight, there's no piece in the way
         ; and no piece of our color is on the tile we're trying to
         ; move to.
         #t]
        [else #f]))

    (define/public (get-symbol)
      (if (equal? color "W")
          "♖"
          "♜"))

    (super-new)))

; Queen class.
(define white-queen-bitmap (read-bitmap "../img/queen-white.png"))
(define black-queen-bitmap (read-bitmap "../img/queen-black.png"))
(define queen%
  (class piece%
    (inherit-field color number-of-moves pos parent-board)
    (field
     [value 9])

    (define/public (get-bitmap)
      (if (equal? color "W")
          white-queen-bitmap
          black-queen-bitmap))

    (define/public (get-value) value)

    (define/public (can-move to-tile-id)
      (cond
        [(and (or (is-diagonal-move? pos to-tile-id)
                  (is-straight-move? pos to-tile-id))
              (send this no-piece-in-the-way to-tile-id)
              (not (equal? (send parent-board tile-occupied-by-color to-tile-id)
                           color)))
         ; The move is diagonal or straight, no piece is in the way
         ; and no piece of the same color is on the tile we're moving to.
         #t]
        [else #f]))

    (define/public (get-symbol)
      (if (equal? color "W")
          "♕"
          "♛"))

    (super-new)))

; King class.
(define white-king-bitmap (read-bitmap "../img/king-white.png"))
(define black-king-bitmap (read-bitmap "../img/king-black.png"))
(define king%
  (class piece%
    (inherit-field color number-of-moves pos parent-board)
    (field
     [value 12])

    (define/public (get-bitmap)
      (if (equal? color "W")
          white-king-bitmap
          black-king-bitmap))

    (define/public (get-value) value)

    (define/public (can-move to-tile-id)
      (cond
        [(and (or (is-diagonal-move? pos to-tile-id)
                  (is-straight-move? pos to-tile-id))
              (not (equal? (send parent-board tile-occupied-by-color to-tile-id)
                           color))
              (= (distance pos to-tile-id) 1))
         ; The move is diagonal or straight, no piece of the same color is
         ; on the tile we're moving to and the move distance is 1.
         #t]
        [else #f]))

    (define/public (get-symbol)
      (if (equal? color "W")
          "♔"
          "♚"))

    (super-new)))