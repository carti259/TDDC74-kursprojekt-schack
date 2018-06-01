#|
Names: Carl Tidén, Tom Tersén
Last edited: 2018-05-31
Purpose of file:
 To define the class 'game-menu%' that has as responibility to instantiate
 new boards and load saved ones for the GUI.
|#

#lang racket
(require "gameboard.rkt")

(provide game-menu%)

(define game-menu%
  (class object%
    ; Give the menu a new board upon game start.
    (field [current-board (new board%)])

    (define/public (new-board)
      (set! current-board (new board%))
      (send current-board prepare-board))

    (define/public (get-board)
      current-board)

    (define/public (restore-board file-path)
      (set! current-board (new board%))
      (send current-board restore-board file-path))

    (super-new)))