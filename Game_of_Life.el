;Game of Life
;Written by: Josip Susnjara

(defconst *board_size* 40
  "width and height")
(defconst *life* "X")
(defconst *edge* "@")
(defconst *death* " ")

(defvar *game_board* nil
  "Game board obviously")
(defvar *board2* nil
  "Auxiliary board")
(defvar *playing* nil
  "Is game in progress or paused")
(defvar *timer* nil
  "Timer for play()")
(defvar *game_speed* nil
  "For game stats")
(defvar *game_speed_seconds* nil
  "After how many seconds is next state displayed, 0.5 default")
(defvar *strings* nil
  "Strings for game data")
(defvar *game_status* nil
  "Playing / Paused")
(defvar *stats_string* nil
  "Last element in *strings* list")
(defvar *game_steps* nil
  "Number of game iterations since the last board modification")

(define-derived-mode game-mode special-mode "game_of_life")

(define-key game-mode-map (kbd "RET") 'markTile)
(define-key game-mode-map (kbd "SPC") 'play)
(define-key game-mode-map (kbd "w") 'speedUp)
(define-key game-mode-map (kbd "s") 'slowDown)

(defun gameOfLife()
  "Start function"
  (interactive)
  (switch-to-buffer "Game_of_Life")
  (game-mode)
  (gameInit)
  )


(defun gameInit()
  "At the beginning"
  (setq *game_board* (make-vector (* *board_size* *board_size*) *death*))
  (setq *board2* (make-vector (* *board_size* *board_size*) *death*))
  (setq *game_speed_seconds* 0.5)
  (setq *game_speed* 1.0)
  (setq *game_status* "Paused")
  (setq *game_steps* 0)
  (setq *stats_string* (format "  |    GAME: %s    SPEED: %sx    STEPS: %s" *game_status* *game_speed* *game_steps*))
  (setq *strings* '(
		   "  |  INSTRUCTIONS:"
		   "  |    • RETURN - put / remove life on a cursor position"
		   "  |    • SPACE - start / pause game"
		   "  |    • w - speed up game pace"
		   "  |    • s - slow down game pace"
		   "  |"
		   "  |  GAME STATS:")
	)
  (add-to-list '*strings* *stats_string* t)
  (boardInit)
  (printBoard)
  (printData)
  )

(defun boardInit()
  "Defines initial board state"
  (dotimes (row *board_size*)
    (dotimes (col *board_size*)
      (if (or (= row 0) (= row (1- *board_size*)) (= col 0) (= col (1- *board_size*)) )
	  (aset *game_board* (+ col (* row *board_size*)) *edge*)
	)
      )
    )
  )

(defun printBoard()
  (let ((inhibit-read-only t))
    (erase-buffer)
    (dotimes (row *board_size*)
      (dotimes (col *board_size*)
	(insert (gameGetTile row col))
	)
      (insert "\n")
      )
    )
  )

(defun printData()
  "Prints instructions, game state, steps, speed"
  (let ((inhibit-read-only t)
	(letters_sum 0))
    (setStatsString)
    (dotimes (i (length *strings*))
      (goto-char (+ (* (+ *board_size* 1) (+ i 1)) letters_sum))
      (insert (elt *strings* i))
      (setq letters_sum (+ letters_sum (length (elt *strings* i))))
      )
    )
  )

(defun printAll()
  "Prints board and data and sets cursor at its place"
  (let (
	(rowP (1- (line-number-at-pos)))
	(colP (current-column))
	(letters_sum 0)
	)
     (printBoard)
     (printData)
     (dotimes (i rowP) ;shift for data chars
       (if (<= i (length *strings*))
	   (setq letters_sum (+ letters_sum (length (elt *strings* i)))))
       )
    (goto-char ( + letters_sum (+ 1 (+ rowP (+ colP (* rowP *board_size*))))))
    )
  )

(defun setStatsString()
  (delete *stats_string* *strings*)
  (setq *stats_string* (format "  |    GAME: %s    SPEED: %sx    STEPS: %s" *game_status* *game_speed* *game_steps*))
  (add-to-list '*strings* *stats_string* t)
  )

(defun gameGetTile(row col)
  "Get the value in the (row, column) of the matrix"
  (elt *game_board* (+ col (* row *board_size*)))
  )

(defun board2GetTile(row col)
  "Get the value in the (row, column) of the matrix"
  (elt *board2* (+ col (* row *board_size*)))
  )

(defun gameSetTile(row col)
  "Set the value in the (row, column) of the matrix"
  (if (equal (gameGetTile row col) *death*)
      (progn (aset *game_board* (+ col (* row *board_size*)) *life*) (setq *game_steps* 0))
    (if (equal (gameGetTile row col) *life*)
	(progn (aset *game_board* (+ col (* row *board_size*)) *death*) (setq *game_steps* 0))
	)
    )
  )

(defun markTile()
  "Mark the current tile"
  (interactive)
  (let (
	(row (1- (line-number-at-pos)))
	     (col (current-column)))
	     (if (and (< row *board_size*) (< col *board_size*))  (gameSetTile row col))
	     (printAll)
	     )
  )

(defun markTileBoard2(row col value)
  (aset *board2* (+ col (* row *board_size*)) value)
  )

(defun calculateTileSum(row col)
  "Calculates sum of neighbouring elements"
  (+
   (if (equal (gameGetTile (- row 1) (- col 1)) *life*) 1 0)
   (if (equal (gameGetTile (- row 1) col) *life*) 1 0)
   (if (equal (gameGetTile (- row 1) (+ col 1)) *life*) 1 0)
   (if (equal (gameGetTile row (- col 1)) *life*) 1 0)
   (if (equal (gameGetTile row (+ col 1)) *life*) 1 0)
   (if (equal (gameGetTile (+ row 1) (- col 1)) *life*) 1 0)
   (if (equal (gameGetTile (+ row 1) col) *life*) 1 0)
   (if (equal (gameGetTile (+ row 1) (+ col 1)) *life*) 1 0)   
   )
  )

(defun calculateNextState()
  "Calculates next state of the board (*board2*)"
  (dotimes (row *board_size*)
    (dotimes (col *board_size*)
      (cond
       ((equal (gameGetTile row col) *edge*) (markTileBoard2 row col *edge*)) ;edge
       ((and (equal (gameGetTile row col) *life*) (or (= (calculateTileSum row col) 2) (= (calculateTileSum row col) 3))) (markTileBoard2 row col *life*)) ;survives
       ((and (equal (gameGetTile row col) *life*) (not (or (= (calculateTileSum row col) 2) (= (calculateTileSum row col) 3)))) (markTileBoard2 row col *death*)) ;dies
       ((and (equal (gameGetTile row col) *death*) (= (calculateTileSum row col) 3)) (markTileBoard2 row col *life*)) ;born
       ((and (equal (gameGetTile row col) *death*) (not (= (calculateTileSum row col) 3))) (markTileBoard2 row col *death*)) ;still death
       )
      )
    )
  )

(defun speedUp()
  "Increases game speed x2"
  (interactive)
  (if (> *game_speed_seconds* 0.04) (progn (setq *game_speed_seconds* (/ *game_speed_seconds* 2)) (setq *game_speed* (* *game_speed* 2)))
    )
  (if *playing* (progn
		    (cancel-timer *timer*)
		    (setq *timer* (run-with-timer 0 *game_speed_seconds* 'goToNextState))
		    )
    (printAll) ;else
    )
  )

(defun slowDown()
  "Decreases game speed x2"
  (interactive)
  (if (< *game_speed_seconds* 8) (progn (setq *game_speed_seconds* (* *game_speed_seconds* 2)) (setq *game_speed* (/ *game_speed* 2)))
    )
  (if *playing* (progn
		    (cancel-timer *timer*)
		    (setq *timer* (run-with-timer 0 *game_speed_seconds* 'goToNextState))
		    )
    (printAll) ;else
    )
  )

(defun goToNextState()
  "One time interval"
  (calculateNextState)
  (dotimes (row *board_size*)
   (dotimes (col *board_size*)
     (aset *game_board* (+ col (* row *board_size*)) (board2GetTile row col))
     )
   )
  (setq *game_steps* (+ *game_steps* 1))
  (printAll)
)

(defun pause()
  "Called from play() if *playing*"
  (cancel-timer *timer*)
  (setq *playing* nil)
  (setq *game_status* "Paused")
  (printAll)
  )

(defun play()
  "Transition between two game instances"
  (interactive)
  (if (not *playing*)
      (progn (setq *timer* (run-with-timer 0 *game_speed_seconds* 'goToNextState)) (setq *playing* t) (setq *game_status* "Playing"))
    (pause)
    )
  )
