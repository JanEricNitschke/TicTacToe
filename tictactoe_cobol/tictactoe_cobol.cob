      * Comment
       IDENTIFICATION DIVISION.
       PROGRAM-ID. hello.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
        01 GAME-BOARD VALUE IS "123456789".
           03 GAME-SPOT OCCURS 9 TIMES PIC X(1).
        01 SPOT PIC 9.
        01 GAME-STATUS PIC X(1).
           88 GAME-OVER VALUE "Y".
        01 ACTIVE-PLAYER PIC X(1) VALUE "X".
        01 LOOP-IDX-1 PIC 9(2).
        01 LOOP-IDX-2 PIC 9(2).
      *                            Rows     | Cols   | Diags
        01 WIN-CONDITIONS.
           03 WIN-CONDITION OCCURS 8.
               05 WIN-VALUE OCCURS 3 TIMES PIC 9(1).
       PROCEDURE DIVISION.
      *    Initialize win conditions
           MOVE 123456789147258369159357 TO WIN-CONDITIONS.

           PERFORM UNTIL GAME-OVER
               PERFORM DO-MOVE

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

       DO-MOVE.
           MOVE 0 TO SPOT
           PERFORM UNTIL SPOT > 0
               DISPLAY "Player " ACTIVE-PLAYER"turn:"
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

       SWAP-ACTIVE-PLAYER.
           IF ACTIVE-PLAYER = "X"
               MOVE "O" TO ACTIVE-PLAYER
           ELSE
               MOVE "X" TO ACTIVE-PLAYER
           END-IF.
