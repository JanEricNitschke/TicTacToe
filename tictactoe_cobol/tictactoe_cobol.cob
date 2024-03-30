       IDENTIFICATION DIVISION.
       PROGRAM-ID. TicTacToe-Cobol.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
        01 GAME-BOARD VALUE IS "123456789".
           03 GAME-SPOT OCCURS 9 TIMES PIC X(1).
        01 SPOT PIC 9.
        01 GAME-STATUS PIC X(1).
           88 GAME-OVER VALUE "Y".
        01 ACTIVE-PLAYER PIC X(1) VALUE "X".
           88 X-TURN VALUE "X".
           88 O-TURN VALUE "O".
        01 LOOP-IDX-1 PIC 9(2).
        01 LOOP-IDX-2 PIC 9(2).
        01 WIN-CONDITIONS.
           03 WIN-CONDITION OCCURS 8.
               05 WIN-VALUE OCCURS 3 TIMES PIC 9(1).

        01 YES-NO-FLAG PIC X(1).
           88 JA VALUE "Y".
           88 NEIN VALUE "N".

        01 X-AI-SETTING PIC X(1).
           88 X-AI VALUE "Y".
        01 O-AI-SETTING PIC X(1).
           88 O-AI VALUE "Y".
        01 O-AI-STRENGTH PIC 9(1) VALUE 0.
        01 X-AI-STRENGTH PIC 9(1) VALUE 0.
        01 AI-STRENGTH PIC 9(1) VALUE 0.

        01 EMPTY-CELLS-AREA.
               03 EMPTY-CELLS OCCURS 9 TIMES PIC 9(1).

       01  RANDOM-IDX  PIC 9 COMP.
       PROCEDURE DIVISION.
      *    Initialize win conditions
           MOVE 123456789147258369159357 TO WIN-CONDITIONS.

           PERFORM GET-SETTINGS.

           PERFORM UNTIL GAME-OVER
               EVALUATE TRUE
                   WHEN X-TURN AND X-AI
                       PERFORM AI-MOVE
                   WHEN O-TURN AND O-AI
                       PERFORM AI-MOVE
                   WHEN OTHER
                       PERFORM PLAYER-MOVE
               END-EVALUATE

               PERFORM  CHECK-GAME-WON
               IF NOT GAME-OVER
                   PERFORM CHECK-GAME-DRAW
                END-IF

               IF NOT GAME-OVER
                   PERFORM SWAP-ACTIVE-PLAYER
                END-IF
           END-PERFORM
           PERFORM DISPLAY-BOARD.
           STOP RUN.

       DISPLAY-BOARD.
           DISPLAY GAME-SPOT (1) " | " GAME-SPOT (2) " | " GAME-SPOT (3)
           DISPLAY GAME-SPOT (4) " | " GAME-SPOT (5) " | " GAME-SPOT (6)
           DISPLAY GAME-SPOT (7) " | " GAME-SPOT (8) " | " GAME-SPOT (9)
           .

       CHECK-GAME-WON.
           PERFORM VARYING LOOP-IDX-1 FROM 1 BY 1 UNTIL LOOP-IDX-1 > 8
               IF GAME-SPOT (WIN-VALUE(LOOP-IDX-1, 1)) = ACTIVE-PLAYER
               AND GAME-SPOT (WIN-VALUE(LOOP-IDX-1, 2)) = ACTIVE-PLAYER
               AND GAME-SPOT (WIN-VALUE(LOOP-IDX-1, 3)) = ACTIVE-PLAYER
                   SET GAME-OVER TO TRUE
               END-IF
           END-PERFORM
           IF GAME-OVER
               DISPLAY "Congratulations! Player "ACTIVE-PLAYER" has won the
      -    "game."
           END-IF.

       CHECK-GAME-DRAW.
           SET GAME-OVER TO TRUE
           PERFORM VARYING LOOP-IDX-1 FROM 1 BY 1 UNTIL LOOP-IDX-1 > 9
               IF GAME-SPOT(LOOP-IDX-1) NOT = "X"
               AND GAME-SPOT(LOOP-IDX-1) NOT = "O"
                   MOVE "N" TO GAME-STATUS
               END-IF
           END-PERFORM.
           IF GAME-OVER
               DISPLAY "The game is a draw!"
           END-IF.

       PLAYER-MOVE.
           MOVE 0 TO SPOT
           PERFORM UNTIL SPOT > 0
               DISPLAY "Player " ACTIVE-PLAYER " turn:"
               PERFORM DISPLAY-BOARD
               DISPLAY "Where to make your next move? [1-9]"
               ACCEPT SPOT
               IF SPOT = 0
                   DISPLAY "Invalid input. Please input a number between
      -    " 1 and 9."
               ELSE
                   IF GAME-SPOT (SPOT) = "X" OR GAME-SPOT (SPOT) = "O"
                      DISPLAY "This spot is already taken. Please choose
      -    " anther one."
                   ELSE
                       MOVE ACTIVE-PLAYER TO GAME-SPOT (SPOT)
                   END-IF
               END-IF
           END-PERFORM.

       RANDOM-MOVE.
           PERFORM SET-EMPTY-CELLS
           MOVE 0 TO RANDOM-IDX
           PERFORM UNTIL RANDOM-IDX > 0 AND EMPTY-CELLS (RANDOM-IDX) = 1
               COMPUTE RANDOM-IDX =
               FUNCTION RANDOM(FUNCTION CURRENT-DATE (9:7)) * 9 + 1
           END-PERFORM
           MOVE ACTIVE-PLAYER TO GAME-SPOT (RANDOM-IDX).

       SWAP-ACTIVE-PLAYER.
           IF ACTIVE-PLAYER = "X"
               MOVE "O" TO ACTIVE-PLAYER
           ELSE
               MOVE "X" TO ACTIVE-PLAYER
           END-IF.


       GET-YES-NO.
           MOVE " " TO YES-NO-FLAG
           PERFORM UNTIL JA OR NEIN
               DISPLAY "Enter Y for Yes or N for No:"
               ACCEPT YES-NO-FLAG
           END-PERFORM.

       GET-SETTINGS.
           DISPLAY "Should 'X' be played by the computer?"
           PERFORM GET-YES-NO.
           MOVE YES-NO-FLAG TO X-AI-SETTING.
           IF X-AI
               SET X-AI TO TRUE
               PERFORM SHOW-STRENGTH-OPTIONS
               ACCEPT X-AI-STRENGTH
           END-IF.

           DISPLAY "Should 'O' be played by the computer?"
           PERFORM GET-YES-NO.
           MOVE YES-NO-FLAG TO O-AI-SETTING.
           IF O-AI
               SET O-AI TO TRUE
               PERFORM SHOW-STRENGTH-OPTIONS
               ACCEPT O-AI-STRENGTH
           END-IF.

       SHOW-STRENGTH-OPTIONS.
           DISPLAY "AI strength settings:"
           DISPLAY "0: Easy"
           DISPLAY "1: Medium"
           DISPLAY "2: Hard"
           DISPLAY "3: Impossible"
           DISPLAY "How strong should the AI be?".

       SET-EMPTY-CELLS.
           PERFORM VARYING LOOP-IDX-1 FROM 1 BY 1 UNTIL LOOP-IDX-1 > 9
               IF GAME-SPOT(LOOP-IDX-1) NOT = "X"
               AND GAME-SPOT(LOOP-IDX-1) NOT = "O"
                   MOVE 1 TO EMPTY-CELLS(LOOP-IDX-1)
               ELSE
                   MOVE 0 TO EMPTY-CELLS(LOOP-IDX-1)
               END-IF
           END-PERFORM.

       AI-MOVE.
           IF X-TURN
               MOVE X-AI-STRENGTH TO AI-STRENGTH
           ELSE
               MOVE O-AI-STRENGTH TO AI-STRENGTH
           END-IF
           DISPLAY "AI turn as player " ACTIVE-PLAYER " with strength "
               AI-STRENGTH "."
           PERFORM DISPLAY-BOARD
           EVALUATE AI-STRENGTH
               WHEN 0
                   PERFORM RANDOM-MOVE
               WHEN OTHER
                   PERFORM RANDOM-MOVE
           END-EVALUATE
           CONTINUE AFTER 1 SECONDS.
