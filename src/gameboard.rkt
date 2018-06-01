#|
Names: Carl Tidén, Tom Tersén
Last edited: 2018-05-31
Purpose of file:
 To define the class 'board%', that has the main responsibility
 of controlling piece movement and maintain the rules of the game.
|#

#lang racket
(require "pieces.rkt" "utils.rkt")
(require 2htdp/batch-io)

(provide board%)

; The board class.
(define board%
  (class object%
    ; A hash table is used for keeping track of piece positions
    ; and empty tiles.
    (field [board-hash-table (make-hash)]
           [turn "W"]
           [white-score 0]
           [black-score 0]
           [white-in-check #f]
           [black-in-check #f]
           [in-checkmate #f]
           [in-draw #f])

    ; Returns who's turn it is. Either "W" or "B".
    (define/public (get-turn)
      (if (equal? turn "W")
          "W"
          "B"))

    ; Returns a pair with white and black score.
    (define/public (get-score)
      (cons white-score black-score))

    ; The three below methods does not check whether a specific
    ; situations is playing out on the board, but simply returns
    ; the properties of the board object.
    (define/public (get-in-check)
      (or white-in-check black-in-check))

    (define/public (get-in-checkmate)
      in-checkmate)

    (define/public (get-in-draw)
      in-draw)

    ; This method saves the current board to a file.
    ; The file is structured as:
    ;  turn (W/B),white-score,...,BR1,BK1,... (the piece names for each tile)
    (define/public (save-board-to-file file-path)
      (define board-string
        (string-append turn
                       ","
                       (number->string white-score)
                       ","
                       (number->string black-score)))

      (for ([i (in-range 1 65)])
        ; Loop through every tile.
        (if (equal? (get-tile-content i) "E")
            (set! board-string (string-append board-string ",E"))
            ; The tile is not empty!
            (set! board-string (string-append
                                board-string
                                ","
                                (string-append
                                 (send (get-tile-content i) get-name)
                                 (number->string
                                  (send (get-tile-content i) moves)))))))
      ; The method ends with saving the created string to a file.
      (write-file file-path board-string))

    ; This method is in a sense similar to the 'prepare-board' method.
    ; It puts all the pieces in their places according to the file
    ; with the name 'file-name'.
    (define/public (restore-board file-path)
      (cond
        [(file-exists? file-path)
         ; The file exists.
         ; The function call on 'read-lines' returns a list of strings
         ; (one for each line). The saved-board-file contains only one
         ; line, though.
         (define saved-board-string (car (read-lines file-path)))
         (define saved-board-list (string-split saved-board-string ","))

         ; Set some state properites.
         (set! turn (first saved-board-list))
         (set! white-score (string->number (second saved-board-list)))
         (set! black-score (string->number (third saved-board-list)))
         (prepare-board saved-board-string)
         ; Return true.
         #t]
        [else #f]))

    ; When a new board is created, run this method in order to
    ; instantiate all pieces and put them in the board hash table.
    ; Hash table keys are encoded by the strings "1", "2", ..., "64",
    ; top left tile is "1".
    (define/public (prepare-board [saved-board-string
                                   (car (read-lines
                                         "../saved-boards/new-board.txt"))])
      ; Define a list with the tile contents as elements.
      ; Chop off the first three elements (which is the turn and scores).
      (define board-list
        (cdr (cdr (cdr (string-split saved-board-string ",")))))

      ; Loop to lay out all pieces on the board.
      (for ([i (in-range 1 65)])
        (cond
          [(equal? (car board-list) "E")
           (hash-set! board-hash-table (number->string i) "E")]
          [(equal? (substring (car board-list) 1 2) "P")
           ; A pawn is supposed to be placed here!
           (hash-set! board-hash-table (number->string i)
                      (new pawn%
                           [name (substring (car board-list) 0 3)]
                           [color (substring (car board-list) 0 1)]
                           [parent-board this]
                           [pos i]
                           [number-of-moves (string->number
                                             (substring (car board-list)
                                                        3 4))]))]
          [(equal? (substring (car board-list) 1 2) "R")
           ; A rook is supposed to be placed here!
           (hash-set! board-hash-table (number->string i)
                      (new rook%
                           [name (substring (car board-list) 0 3)]
                           [color (substring (car board-list) 0 1)]
                           [parent-board this]
                           [pos i]
                           [number-of-moves (string->number
                                             (substring (car board-list) 3))]))]
          [(equal? (substring (car board-list) 1 2) "H")
           ; A knight is supposed to be placed here!
           (hash-set! board-hash-table (number->string i)
                      (new knight%
                           [name (substring (car board-list) 0 3)]
                           [color (substring (car board-list) 0 1)]
                           [parent-board this]
                           [pos i]
                           [number-of-moves (string->number
                                             (substring (car board-list) 3))]))]
          [(equal? (substring (car board-list) 1 2) "B")
           ; A bishop is supposed to be placed here!
           (hash-set! board-hash-table (number->string i)
                      (new bishop%
                           [name (substring (car board-list) 0 3)]
                           [color (substring (car board-list) 0 1)]
                           [parent-board this]
                           [pos i]
                           [number-of-moves (string->number
                                             (substring (car board-list) 3))]))]
          [(equal? (substring (car board-list) 1 2) "Q")
           ; A queen is supposed to be placed here!
           (hash-set! board-hash-table (number->string i)
                      (new queen%
                           [name (substring (car board-list) 0 3)]
                           [color (substring (car board-list) 0 1)]
                           [parent-board this]
                           [pos i]
                           [number-of-moves (string->number
                                             (substring (car board-list) 3))]))]
          [(equal? (substring (car board-list) 1 2) "K")
           ; A king is supposed to be placed here!
           (hash-set! board-hash-table (number->string i)
                      (new king%
                           [name (substring (car board-list) 0 3)]
                           [color (substring (car board-list) 0 1)]
                           [parent-board this]
                           [pos i]
                           [number-of-moves
                            (string->number
                             (substring (car board-list) 3))]))])
        (set! board-list (cdr board-list)))

      ; Set the state properties. Not really necessary when new board
      ; is "loaded".
      (set-check-checkmate-draw)

      (draw-board))

    ; Switch turns.
    (define/private (switch-turns)
      (if (equal? turn "W")
          (set! turn "B")
          (set! turn "W")))

    ; Get what's on a tile ("E" if empty). Send with the number
    ; (not string!) that identifies the tile.
    (define/public (get-tile-content tile-id)
      (hash-ref board-hash-table (number->string tile-id)))

    ; When a piece is being captured, this method is run to update
    ; the score. Argument is the captured piece.
    (define/private (update-score piece)
      (if (equal? (send piece get-color) "W")
          ; A white piece was captured.
          (set! black-score (+ black-score (send piece get-value)))
          ; A black piece was captured.
          (set! white-score (+ white-score (send piece get-value)))))

    ; This method returns true if the combination of pieces
    ; on the board means draw.
    ; OUT: #f or #t (or a list which is interpreted as true where this
    ; method is used).
    (define/private (in-combination-draw?)
      ; Define an empty list to place piece names in.
      (define piece-list '())

      ; Define an empty list to place bishop positions in.
      (define bishop-pos-list '())

      (for ([i (in-range 1 65)])
        (cond
          [(not (equal? (get-tile-content i) "E"))
           ; The tile is not empty, add the piece name to the list.
           (set! piece-list
                 (cons
                  (substring (send (get-tile-content i) get-name) 0 2)
                  piece-list))
           (cond
             [(equal? (substring (send (get-tile-content i) get-name) 1 2) "B")
              ; It's a bishop! Place its position in the list...
              (set! bishop-pos-list
                    (cons (send (get-tile-content i) get-pos)
                          bishop-pos-list))])]))

      ; Sort the piece list.
      (set! piece-list (sort piece-list string<?))

      ; Test for two kinds of combination draw.
      (cond
        [(or (and
              (equal? piece-list '("BB" "BK" "WB" "WK"))
              (same-colored-tiles?
               (first bishop-pos-list)
               (second bishop-pos-list)))
             (member piece-list draw-combinations))
         ; First alt.:
         ;  We have two bishops of each color (and two kings) on the board.
         ;  The bishops are standing on same-colored tiles.
         ; Second alt.:
         ;  If the current combination is a member of the 'draw-combinations'
         ;  list, we're returning #t.
         #t]
        [else #f]))

    ; A method to return the name of the current turn's (default case) king.
    (define/private (king-name [which turn])
      (if (equal? which "W")
          "WK0"
          "BK0"))

    ; This method is used to see if there are any available moves
    ; that will get the current turn out of check. In the case where
    ; the current turn is not in check, we're looking to see if there
    ; are any moves that will not place current turn in check
    ; (which means no stalemate).
    ; OUT: #t/#f.
    (define/private (can-not-escape-check?)
      (let iterate-tiles ([i 1])
        ; Loop to find all pieces of 'turn' color.
        (cond
          [(= i 65)
           ; We've gotten to the 65th tile without finding a way
           ; to eliminate check: return true!
           #t]
          [(and (not (equal? (get-tile-content i) "E"))
                (equal? (send (get-tile-content i) get-color) turn)
                ; There's a 'turn'-colored piece here!
                (let iterate-moves ([j 1])
                  ; See if the piece can go to 'j'-tile and eliminate check.
                  (cond
                    [(= j 65)
                     ; The piece could not eliminate check!
                     #f]
                    [(and (not (= i j)) ; Do not test for move to same tile.
                          (not (= j (get-piece-pos (king-name))))
                          (send (get-tile-content i) can-move j)
                          (no-check-after-move i
                                               j
                                               turn))
                     ; The pice can eliminate check by moving here!
                     #t]
                    [else
                     ; Try go to another tile!
                     (iterate-moves (+ j 1))])))
           ; The piece on the 'i'-tile could eliminate check!
           #f]
          [else
           ; Try the content of the next tile...
           (iterate-tiles (+ i 1))])))

    ; The below method finds out if the game is in check, checkmate or
    ; draw. It sets the properties 'is-check'... accordingly but commits
    ; no further action. The GUI then handles a checkmate or draw.
    (define/private (set-check-checkmate-draw)
      (set! white-in-check (check-now "W"))
      (set! black-in-check (check-now "B"))

      ; We only have to consider current turns's situation for
      ; check/checkmate/stalemate (current turn's since 'switch-turns'
      ; is run before this method in 'execute-move-piece').
      (cond
        [(in-combination-draw?)
         ; We're in a draw based on the combination of pieces
         ; on the board.
         (set! in-draw #t)]
        ; Only need to consider checkmate if check.
        [(check-now turn)
         ; Set the 'in-checkmate' property...
         (set! in-checkmate (can-not-escape-check?))]
        [else
         ; The current turn is not checked. But we can be
         ; in a stalemate situation. Set the 'in-draw' property...
         (set! in-draw (can-not-escape-check?))]))

    ; Get a specific piece position (as tile id). If the piece is not found,
    ; we're returning false.
    ; OUT: number or #f.
    (define/public (get-piece-pos piece-name)
      (let iterate-tiles ([i 1])
        (cond
          [(= i 65)
           ; The piece was not on the board!
           #f]
          [(and (not (equal? (get-tile-content i) "E"))
                (equal? (send (get-tile-content i) get-name) piece-name))
           i]
          [else
           ; Next recursion step!
           (iterate-tiles (+ i 1))])))

    ; This method is executed when it is known that a piece can move
    ; to a specific tile. This method not only changes the references in the
    ; hash table, but also runs the 'move' method on the piece!
    (define/private (execute-move-piece from-tile-id to-tile-id)
      ; Run the 'move' method on the piece.
      (send (get-tile-content from-tile-id) move to-tile-id)

      ; Check if a piece is on the to-tile-id tile.
      (if (not (equal? (get-tile-content to-tile-id) "E"))
          ; A piece is being captured.
          (begin
            (printf "A piece is being captured!\n")
            (update-score (get-tile-content to-tile-id)))
          (printf "No piece is being captured.\n"))

      (hash-set! board-hash-table (number->string to-tile-id)
                 (hash-ref board-hash-table (number->string from-tile-id)))
      (hash-set! board-hash-table (number->string from-tile-id) "E")

      ; Switch turns.
      (switch-turns)

      ; Now is the time to see if we're in check, checkmate or draw.
      (set-check-checkmate-draw)

      ; Redraw the board.
      (draw-board))

    ; This method takes numbers as from/to-tile-id as usual.
    ; It has to be made sure the from-tile-id tile contains a piece
    ; before this method is run. (And that it's that color's turn...).
    ; If the move can't be made, #f is returned, otherwise #t.
    ; IN: number, number.
    ; OUT: #t/#f.
    (define/public (move-from-to from-tile-id to-tile-id)
      ; Check whether or not the piece can move supplied way.
      (cond
        [(and (send (get-tile-content from-tile-id)
                    can-move to-tile-id)
              ; Make sure the suggested move does not place/keep us check.
              ; If no move like that can be made, we're in checkmate/stalemate,
              ; but such scenario is already "found" in the last run of
              ; 'execute-move-piece', and the game is stopped by the GUI
              ; (i.e. when we run 'move-from-to', we're sure there's a possible
              ; move to be made).
              (no-check-after-move
               from-tile-id
               to-tile-id
               (send (get-tile-content from-tile-id) get-color)))
         ; It can move like that! The consequences of the move is handled
         ; in 'execute-move-piece'.
         (execute-move-piece from-tile-id to-tile-id) #t]
        [else #f]))

    ; This method returns whether or not the suggested move places
    ; the color of the move in check.
    ; IN: number, number, string.
    ; OUT: #t/#f.
    (define/private (no-check-after-move from to color-to-be-moved)
      (define check-after-move #f)

      ; We know the move can be made by the piece itself.
      ; Save how the pieces stood before.
      (define tile-content-from (get-tile-content from))
      (define tile-content-to (get-tile-content to))

      ; Move the piece on the actual board.
      (send (get-tile-content from) move to)
      (hash-set! board-hash-table (number->string to)
                 (hash-ref board-hash-table (number->string from)))
      (hash-set! board-hash-table (number->string from) "E")

      ; Loop over every tile and see if it's possible piece
      ; is of the other color and can move to the king's position.
      (set! check-after-move (check-now color-to-be-moved))

      ; Reset the board.
      (send (get-tile-content to) move-back from)
      (hash-set! board-hash-table (number->string from) tile-content-from)
      (hash-set! board-hash-table (number->string to) tile-content-to)

      (not check-after-move))

    ; The below method returns the color of the tile with some tile
    ; number. If no piece there, it returns "E".
    (define/public (tile-occupied-by-color tile-id)
      (if (not (equal? (get-tile-content tile-id) "E"))
          (send (get-tile-content tile-id) get-color)
          "E"))

    ; Below is a function that takes a board and checks for check
    ; (checks if 'color' is in check).
    (define/private (check-now color)
      (let iterate-tiles ([i 1])
        (cond
          [(= i 65)
           ; No check!
           #f]
          [(and (not (equal? (get-tile-content i) "E"))
                (not (equal? (send (get-tile-content i) get-color)
                             color))
                (send (get-tile-content i) can-move
                      (get-piece-pos (king-name color))))
           ; A piece of the other color can move onto the 'color' king.
           #t]
          [else
           ; Next recursion step.
           (iterate-tiles (+ i 1))])))

    ; The below method draws the board. It is public for
    ; testing purposes.
    (define/public (draw-board)
      (printf "   a b c d e f g h\n")
      (for ([i (in-range 1 9)])
        (define this-row (string-join (list (number->string i) "|")))
        (for ([j (in-range 1 9)])
          (define tile-content
            (get-tile-content (tile-id-from-coordinates i j)))
          (cond
            [(equal? tile-content "E")
             (set! this-row (string-append this-row "E|"))]
            [else
             (set! this-row
                   (string-append
                    this-row
                    (string-join (list
                                  (send tile-content get-symbol) "|") "")))]))
        (set! this-row (string-append this-row "\n"))
        (printf this-row))
      (printf (string-join (list "White score:"
                                 (number->string white-score) "\n")))
      (printf (string-join (list "Black score:"
                                 (number->string black-score) "\n")))
      (cond [white-in-check (printf "White in check!\n")])
      (cond [black-in-check (printf "Black in check!\n")])
      (cond [in-checkmate (printf "Checkmate!\n")]))

    (super-new)))