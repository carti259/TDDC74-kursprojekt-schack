#|
Names: Carl Tidén, Tom Tersén
Last edited: 2018-05-31
Purpose of file:
 To define components and their behaviours to build a simple
 graphical user interface.
|#

#lang racket/gui
(require "menu.rkt" "utils.rkt")
(require racket/date)
(require 2htdp/image
         (only-in racket/gui/base play-sound))

; Define a new menu object to control through the GUI.
(define menu (new game-menu%))
; Make the current board prepare itself.
(send (send menu get-board) prepare-board)

; Define the main window of the game.
(define *window* (new frame%
                      [width 880]
                      [height 480]
                      [label "Schack"]))
; Show the main window.
(send *window* show #t)

; Define a panel to place board and menu horizontally.
(define *horizontal-panel* (new horizontal-panel% [parent *window*]))

; Define a panel to contain the board.
(define *board-panel* (new panel%
                           [parent *horizontal-panel*]
                           [stretchable-width #f]
                           [stretchable-height #f]))

; Define a panel to contain the menu.
(define *menu-panel* (new vertical-panel%
                          [parent *horizontal-panel*]))

; Define a panel to contain the canvases of the menu.
(define *menu-canvas-panel* (new vertical-panel%
                                 [parent *menu-panel*]
                                 [alignment '(left top)]
                                 [min-height 160]))

; Define a panel to contain the menu's buttons.
(define *menu-button-panel* (new horizontal-panel%
                                 [parent *menu-panel*]
                                 [alignment '(center bottom)]))

; Subclass the canvas class to handle mouse events.
(define board-canvas%
  (class canvas%
    (init-field mouse-handler)
    (define/override (on-event mouse-event)
      (mouse-handler mouse-event))
    (super-new)))

; Load the bitmaps required to draw the checkers and the
; highlight.
(define checkers-bitmap (read-bitmap "../img/checkers.png"))
(define tile-bitmap (read-bitmap "../img/highlight.png"))

; A function to draw the board on the board canvas.
(define (board-canvas-drawing-proc
         [canvas *board-canvas*]
         [dc (send *board-canvas* get-dc)])
  ; Clear the existing drawing.
  (send dc clear)
  ; Draw the checkers on the bottom.
  (send dc draw-bitmap checkers-bitmap 0 0)
  ; If we're to highlight a tile, place the highlight
  ; bitmap at that tile's position.
  (cond [piece-selected (send dc draw-bitmap
                              tile-bitmap
                              (highlight-x tile-id-of-selected-piece)
                              (highlight-y tile-id-of-selected-piece))])
  ; Loop through the board's tiles and place a piece bitmap
  ; there if the tile is not empty.
  (for ([i (in-range 1 65)])
    (define tc (send (send menu get-board) get-tile-content i))
    (cond
      [(not (equal? tc "E"))
       (send dc draw-bitmap
             (send tc get-bitmap)
             (tile-to-bitmap-x i)
             (tile-to-bitmap-y i))])))

; A function to play a sound effect when a piece is moving.
(define (play-sound-effect)
  (if (equal? (send (send menu get-board) get-turn) "W")
      ; If the turn is now white, it means black just moved.
      (play-sound "../sounds/black-move.mp3" #t)
      (play-sound "../sounds/white-move.mp3" #t)))

; Variables to know if we have a piece selected
; and, if so, the tile-id of the piece.
(define piece-selected #f)
(define tile-id-of-selected-piece #f)

; This method updates the message canvas to the current
; game state.
(define (display-game-state)
  (cond
    [(send (send menu get-board) get-in-checkmate)
     (display-message "Checkmate!")]
    [(send (send menu get-board) get-in-check)
     (display-message "Check!")]
    [(send (send menu get-board) get-in-draw)
     (display-message "Draw!")]))

; The mouse procedure is fired when the mouse commits
; an action on the board canvas.
(define (mouse-proc event)
  (cond
    [(and (eqv? (send event get-event-type) 'left-down)
          (not (send (send menu get-board) get-in-checkmate))
          (not (send (send menu get-board) get-in-draw)))
     ; It's a click, and we're not in checkmate or draw.
     ; Clear the message field.
     (send (send *message-canvas* get-dc) clear)
     ; Define a variable for the tile-id of the click for better
     ; readability later in this method.
     (define click-tile-id
       (pixel-pos-to-tile-id (send event get-x) (send event get-y)))

     (cond
       [piece-selected
        ; A piece is already selected.
        (cond
          [(send (send menu get-board) move-from-to
                 tile-id-of-selected-piece
                 click-tile-id)
           ; 'move-from-to' returned true; the piece could move here!
           ; Deselect the piece.
           (set! piece-selected #f)
           ; Play a sound.
           (play-sound-effect)
           ; Redraw.
           (board-canvas-drawing-proc)
           ; Update score canvas.
           (display-score)
           ; Run the 'display-game-state' function to update the message canvas.
           (display-game-state)]
          [else
           ; The piece could not move here!
           (display-message "Piece can not move like that!")])]
       [else
        ; No piece is selected!
        ; Define a variable for the tile content of clicked tile,
        ; this is to improve readability below.
        (define tc (send (send menu get-board)
                         get-tile-content click-tile-id))

        (if (and
             (not (equal? tc "E"))
             (equal? (send tc get-color) (send (send menu get-board) get-turn)))
            ; The tile clicked is not empty and contains a piece
            ; of the current turn's color.
            (begin
              (set! piece-selected #t)
              (set! tile-id-of-selected-piece click-tile-id)
              ; Redraw the board canvas.
              (board-canvas-drawing-proc))
            ; The user clicked a tile that is of the other color or empty.
            (display-message "You can't select that!"))])]
    [(eqv? (send event get-event-type) 'right-down)
     ; Right click! Clear message, deselect the piece, and redraw the canvas.
     (send (send *message-canvas* get-dc) clear)
     (set! piece-selected #f)
     (board-canvas-drawing-proc)]))

; Define a canvas to draw the board on.
(define *board-canvas* (new board-canvas%
                            [parent *board-panel*]
                            [paint-callback board-canvas-drawing-proc]
                            [min-width 480]
                            [min-height 480]
                            [mouse-handler mouse-proc]))

; Define a panel to contain the message canvas.
(define *message-panel* (new panel%
                             [parent *menu-canvas-panel*]
                             [min-height 80]
                             [stretchable-height #f]))

(define *score-panel* (new panel%
                           [parent *menu-canvas-panel*]
                           [min-height 80]))

; This function displays a new message on the message canvas.
(define (display-message text)
  (send (send *message-canvas* get-dc) clear)
  (send (send *message-canvas* get-dc) draw-text text 10 10))

; Define a canvas to write messages on.
(define *message-canvas* (new canvas%
                              [parent *message-panel*]
                              [min-width 400]
                              [min-height 80]))
; Set the font properties of the message canvas.
(send (send *message-canvas* get-dc) set-font (make-object font% 12 'modern))

; This function displays the current score.
(define (display-score [canvas #f] [dc #f])
  (send (send *score-canvas* get-dc) clear)
  (send (send *score-canvas* get-dc) draw-text
        (string-append
         "White score: "
         (number->string (car (send (send menu get-board) get-score)))
         ", black score: "
         (number->string (cdr (send (send menu get-board) get-score))))
        10 10))

; Define a canvas to draw the current score on.
(define *score-canvas* (new canvas%
                            [parent *score-panel*]
                            [min-width 400]
                            [min-height 80]
                            [paint-callback display-score]))
; Set the font properties of the message canvas.
(send (send *score-canvas* get-dc) set-font (make-object font% 12 'modern))

; This function orders a new board.
(define (new-game-proc button event)
  ; Deselect.
  (set! piece-selected #f)
  ; Get a new board.
  (send menu new-board)
  ; Redraw.
  (board-canvas-drawing-proc)
  ; Update score canvas.
  (display-score)
  ; Clear the message canvas.
  (send (send *message-canvas* get-dc) clear))

; The new board button runs the 'new-game-proc' function.
(define *new-board-button* (new button%
                                [label "New game"]
                                [parent *menu-button-panel*]
                                [callback new-game-proc]))

; This function restores a saved board in the file
; with the name currently in *dialog-text-field*.
(define (open-saved-proc button event)
  ; Run the restore board method on the menu object.
  (cond
    [(send menu restore-board
           (string-append
            "../saved-boards/"
            (send *dialog-text-field* get-value)))
     ; We found a file with the name in *dialog-text-field*!
     ; Deselect, redraw and update the score canvas.
     (set! piece-selected #f)
     (board-canvas-drawing-proc)
     (display-score)
     ; Clear the message box and write a new message.
     (send (send *message-canvas* get-dc) clear)

     ; Run the 'display-game-state' function to update the message canvas.
     (display-game-state)

     ; Hide the dialog box.
     (send *dialog* show #f)]))

; The dialog box shows up when opening a saved game.
(define *dialog* (new frame%
                      [label "Name of game file"]
                      [width 500]
                      [height 80]))
(define *dialog-text-field* (new text-field%
                                 [parent *dialog*]
                                 [label "Name of game file:"]))
(define *submit-button* (new button%
                             [label "Done"]
                             [parent *dialog*]
                             [callback open-saved-proc]))

(define *restore-board-button* (new button%
                                    [label "Open saved game"]
                                    [parent *menu-button-panel*]
                                    [callback
                                     (lambda (button event)
                                       (send *dialog* show #t))]))

; This function makes sure the current game is saved.
(define (save-game-proc button event)
  (send (send menu get-board) save-board-to-file
        (string-append "../saved-boards/"
                       (date->string (seconds->date (current-seconds)) #t))))

(define *save-board-button* (new button%
                                 [label "Save game"]
                                 [parent *menu-button-panel*]
                                 [callback save-game-proc]))