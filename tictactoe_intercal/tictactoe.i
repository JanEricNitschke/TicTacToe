        DO NOTE === DECLARATIONS
        DO NOTE === GAME BOARD
                #1 = X
                #2 = O
                #3 = BLANK
        PLEASE ,10 <- #9
        DO ,10SUB#1 <- #3
        DO ,10SUB#2 <- #3
        PLEASE ,10SUB#3 <- #3
        DO ,10SUB#4 <- #3
        DO ,10SUB#5 <- #3
        PLEASE ,10SUB#6 <- #3
        DO ,10SUB#7 <- #3
        DO ,10SUB#8 <- #3
        PLEASE ,10SUB#9 <- #3
        DO NOTE === #2 IF IT IS O'S TURN
        DO NOTE === #1 IF IT IS X'S TURN
        DO .10 <- #1
        DO NOTE === Variables used
        DO NOTE === ,10 Game Board
        DO NOTE === .10 Current Player
        DO NOTE === ,11 Output Buffer

        DO NOTE === MAIN LOOP
(1)     DO FORGET #1
        DO NOTE === DISPLAY BOARD
        PLEASE (10) NEXT
        DO NOTE === DISPLAY "X"
        DO (100) NEXT
        DO .10 <- #1
        DO NOTE === READ USER INPUT
        DO (20) NEXT
        DO NOTE === CHECK FOR WIN
        DO (30) NEXT
        DO NOTE === CHECK FOR DRAW
        PLEASE (60) NEXT
        PLEASE (10) NEXT
        DO NOTE === DISPLAY "O"
        DO (101) NEXT
        DO .10 <- #2
        DO (20) NEXT
        DO (30) NEXT
        PLEASE (60) NEXT
        PLEASE (1) NEXT

        DO NOTE === DISPLAY BOARD ROUTINE
                |: 01111100, 00111110, 62 , 194
(10)    DO ,11 <- #26
        DO ,11SUB#1 <- #194
        DO .1 <- ,10SUB#1
        DO NOTE === Store player char and '|' in .2 and .3
        PLEASE (40) NEXT
        DO ,11SUB#2 <- .2
        DO ,11SUB#3 <- .3
        DO .1 <- ,10SUB#2
        PLEASE (40) NEXT
        DO ,11SUB#4 <- .2
        DO ,11SUB#5 <- .3
        DO .1 <- ,10SUB#3
        PLEASE (40) NEXT
        DO ,11SUB#6 <- .2
        DO ,11SUB#7 <- .3
        PLEASE ,11SUB#8 <- #238
        DO ,11SUB#9 <- #274
        DO .1 <- ,10SUB#4
        PLEASE (40) NEXT
        DO ,11SUB#10 <- .2
        DO ,11SUB#11 <- .3
        DO .1 <- ,10SUB#5
        PLEASE (40) NEXT
        DO ,11SUB#12 <- .2
        DO ,11SUB#13 <- .3
        DO .1 <- ,10SUB#6
        PLEASE (40) NEXT
        DO ,11SUB#14 <- .2
        DO ,11SUB#15 <- .3
        PLEASE ,11SUB#16 <- #238
        DO ,11SUB#17 <- #274
        DO .1 <- ,10SUB#7
        PLEASE (40) NEXT
        DO ,11SUB#18 <- .2
        DO ,11SUB#19 <- .3
        DO .1 <- ,10SUB#8
        PLEASE (40) NEXT
        DO ,11SUB#20 <- .2
        DO ,11SUB#21 <- .3
        DO .1 <- ,10SUB#9
        PLEASE (40) NEXT
        DO ,11SUB#22 <- .2
        DO ,11SUB#23 <- .3
        DO ,11SUB#24 <- #238
        DO ,11SUB#25 <- #0
        PLEASE ,11SUB#26 <- #336
        DO READ OUT ,11
        DO RESUME #1

        DO NOTE === TURN
        DO NOTE === No check is performed whether the spot is already taken
(20)    DO ,11 <- #1
        DO ,11SUB#1 <- #0
        PLEASE READ OUT ,11
        DO WRITE IN .1
        DO ,10SUB.1 <- .10
        DO RESUME #1


        DO NOTE === CHECK FOR WIN
        DO NOTE === .1 Stores the result of the whole winner check (any condition true)
        DO NOTE === A value of 1 indicates that the player has won the game
        DO NOTE === .2 Stores the result of the current condition (all spots true)
        DO NOTE === A value of 1 indicates that the player has met the condition
(30)    DO .1 <- #0
        DO .2 <- #1
        DO NOTE === Mingle board value (,10SUB#1) with current player (.10): "V$P"
        DO NOTE === XOR the result: '?"V$P"'
        DO NOTE === Select every second bit: "'?"V$P"'~'#0$#65535'"
        DO NOTE === Then select all the lower bits: ~#65535
        DO NOTE === Results in 0 if V == P and something else otherwise
        DO .9 <- "'?"',10SUB#1'$.10"'~'#0$#65535'"~#65535
        DO NOTE === ZERO CHECK
        DO NOTE === 1 if V == P and 0 otherwise
        PLEASE (50) NEXT
        DO NOTE === AND the overall current condition check with the checked spot
        DO .2 <- "&'.2$.9'"~#1
        DO .9 <- "'?"',10SUB#2'$.10"'~'#0$#65535'"~#65535
        PLEASE (50) NEXT
        DO .2 <- "&'.2$.9'"~#1
        DO .9 <- "'?"',10SUB#3'$.10"'~'#0$#65535'"~#65535
        PLEASE (50) NEXT
        DO .2 <- "&'.2$.9'"~#1
        PLEASE DO NOTE === OR the overall winner check with the current condition
        DO .1 <- "V'.1$.2'"~#1

        DO .2 <- #1
        DO .9 <- "'?"',10SUB#4'$.10"'~'#0$#65535'"~#65535
        PLEASE (50) NEXT
        DO .2 <- "&'.2$.9'"~#1
        DO .9 <- "'?"',10SUB#5'$.10"'~'#0$#65535'"~#65535
        PLEASE (50) NEXT
        DO .2 <- "&'.2$.9'"~#1
        DO .9 <- "'?"',10SUB#6'$.10"'~'#0$#65535'"~#65535
        PLEASE (50) NEXT
        DO .2 <- "&'.2$.9'"~#1
        DO .1 <- "V'.1$.2'"~#1

        DO .2 <- #1
        DO .9 <- "'?"',10SUB#7'$.10"'~'#0$#65535'"~#65535
        PLEASE (50) NEXT
        DO .2 <- "&'.2$.9'"~#1
        DO .9 <- "'?"',10SUB#8'$.10"'~'#0$#65535'"~#65535
        PLEASE (50) NEXT
        DO .2 <- "&'.2$.9'"~#1
        DO .9 <- "'?"',10SUB#9'$.10"'~'#0$#65535'"~#65535
        PLEASE (50) NEXT
        DO .2 <- "&'.2$.9'"~#1
        DO .1 <- "V'.1$.2'"~#1

        DO .2 <- #1
        DO .9 <- "'?"',10SUB#1'$.10"'~'#0$#65535'"~#65535
        PLEASE (50) NEXT
        DO .2 <- "&'.2$.9'"~#1
        DO .9 <- "'?"',10SUB#4'$.10"'~'#0$#65535'"~#65535
        PLEASE (50) NEXT
        DO .2 <- "&'.2$.9'"~#1
        DO .9 <- "'?"',10SUB#7'$.10"'~'#0$#65535'"~#65535
        PLEASE (50) NEXT
        DO .2 <- "&'.2$.9'"~#1
        DO .1 <- "V'.1$.2'"~#1

        DO .2 <- #1
        DO .9 <- "'?"',10SUB#2'$.10"'~'#0$#65535'"~#65535
        PLEASE (50) NEXT
        DO .2 <- "&'.2$.9'"~#1
        DO .9 <- "'?"',10SUB#5'$.10"'~'#0$#65535'"~#65535
        PLEASE (50) NEXT
        DO .2 <- "&'.2$.9'"~#1
        DO .9 <- "'?"',10SUB#8'$.10"'~'#0$#65535'"~#65535
        PLEASE (50) NEXT
        DO .2 <- "&'.2$.9'"~#1
        DO .1 <- "V'.1$.2'"~#1

        DO .2 <- #1
        DO .9 <- "'?"',10SUB#3'$.10"'~'#0$#65535'"~#65535
        PLEASE (50) NEXT
        DO .2 <- "&'.2$.9'"~#1
        DO .9 <- "'?"',10SUB#6'$.10"'~'#0$#65535'"~#65535
        PLEASE (50) NEXT
        DO .2 <- "&'.2$.9'"~#1
        DO .9 <- "'?"',10SUB#9'$.10"'~'#0$#65535'"~#65535
        PLEASE (50) NEXT
        DO .2 <- "&'.2$.9'"~#1
        DO .1 <- "V'.1$.2'"~#1

        DO .2 <- #1
        DO .9 <- "'?"',10SUB#1'$.10"'~'#0$#65535'"~#65535
        PLEASE (50) NEXT
        DO .2 <- "&'.2$.9'"~#1
        DO .9 <- "'?"',10SUB#5'$.10"'~'#0$#65535'"~#65535
        PLEASE (50) NEXT
        DO .2 <- "&'.2$.9'"~#1
        DO .9 <- "'?"',10SUB#9'$.10"'~'#0$#65535'"~#65535
        PLEASE (50) NEXT
        DO .2 <- "&'.2$.9'"~#1
        DO .1 <- "V'.1$.2'"~#1

        DO .2 <- #1
        DO .9 <- "'?"',10SUB#3'$.10"'~'#0$#65535'"~#65535
        PLEASE (50) NEXT
        DO .2 <- "&'.2$.9'"~#1
        DO .9 <- "'?"',10SUB#5'$.10"'~'#0$#65535'"~#65535
        PLEASE (50) NEXT
        DO .2 <- "&'.2$.9'"~#1
        DO .9 <- "'?"',10SUB#7'$.10"'~'#0$#65535'"~#65535
        PLEASE (50) NEXT
        DO .2 <- "&'.2$.9'"~#1
        DO .1 <- "V'.1$.2'"~#1

        DO NOTE === 0 -> 2 AND 1 -> 3
        DO .1 <- '#1$.1'~#3

        PLEASE (31) NEXT
        DO (10) NEXT
        DO (102) NEXT
        DO GIVE UP
(31)    DO (32) NEXT
        PLEASE DO RESUME #2
(32)    DO (33) NEXT
        DO THROW AWAY INVALID BOOLEANS. THIS ERROR SHOULD NEVER APPEAR.
(33)    PLEASE DO RESUME .1


        DO NOTE === ID TO X/O/SPACE (AFTER VERTICAL BAR) AND FOLLOWING VERTICAL BAR
                LETTER:  X        O        [SPACE]  |        [LF]
                NORMAL:  01011000 01001111 00100000 01111100 00001010
                REVERSE: 00011010 11110010 00000100 00111110 01010000
                DECIMAL: 26       242      4        62       80
                -|:      36       -180     58       0        -18
                +256:    292      76       314      256      238

                | AFTER OTHER:
                         X:     26  - 62 + 256 = 220
                         O:     242 - 62 + 256 = 436
                         SPACE: 4   - 62 + 256 = 198
                         LF:    80  - 62 + 256 = 274
(40)    PLEASE (41) NEXT
        DO .2 <- #314
        DO .3 <- #198
        PLEASE RESUME #1
(41)    DO (42) NEXT
        DO .2 <- #76
        PLEASE .3 <- #436
        DO RESUME #2
(42)    DO (43) NEXT
        DO .2 <- #292
        PLEASE .3 <- #220
        DO RESUME #3
(43)    PLEASE RESUME .1

        DO NOTE === ZERO CHECK
        DO NOTE === 0 -> 1
        DO NOTE === ANYTHING ELSE -> 0
        DO NOTE === Performs NON-ZERO CHECK:
        DO NOTE === 0 -> 0 and anything else -> 1
        DO NOTE === Then swaps the last bit ?'.9$#1' (also does other stuff but we dont care)
        DO NOTE === Then checks the last bit again.
(50)    DO (51) NEXT
        PLEASE .9 <- "?'.9$#1'"~#1
        DO RESUME #1

        DO NOTE === NON-ZERO CHECK
        DO NOTE === 0 -> 0
        DO NOTE === ANYTHING ELSE -> 1
        DO NOTE === Moves all the 1 bits to the right (.9~.9),
        DO NOTE === Then checks if the last is 1.
(51)    DO .9 <- '.9~.9'~#1
        PLEASE RESUME #1

        DO NOTE === "X"
(100)   DO ,11 <- #3
        DO ,11SUB#1 <- #230
        PLEASE ,11SUB#2 <- #202
        DO ,11SUB#3 <- #336
        DO READ OUT ,11
        DO RESUME #1

        DO NOTE === "O"
(101)   DO ,11 <- #3
        DO ,11SUB#1 <- #14
        PLEASE ,11SUB#2 <- #418
        DO ,11SUB#3 <- #336
        DO READ OUT ,11
        DO RESUME #1

        DO NOTE === "WIN"
                W: 01010111, 11101010, 234, -234, 22
                I: 01001001, 10010010, 146, 88  , 344
                N: 01001110, 01110010, 114, 32  , 288
             NULL: 00000000, 00000000, 0  , 114 , 370
               \n: 00001010, 01010000, 80 , -80 , 176
             NULL: 11111111, 11111111, 0  ,  80 , 80
(102)   PLEASE ,11 <- #4
        DO ,11SUB#1 <- #22
        DO ,11SUB#2 <- #344
        DO ,11SUB#3 <- #288
        PLEASE ,11SUB#4 <- #370
        DO READ OUT ,11
        DO ,11 <- #2
        DO ,11SUB#1 <- #176
        PLEASE ,11SUB#2 <- #80
        DO READ OUT ,11
        DO RESUME #1


        DO NOTE === "DRAW"
                D: 01000100, 00100010, 34 , -34 , 222
                R: 01010010, 01001010, 74 , -40 , 216
                A: 01000001, 10000010, 130, -56 , 200
                W: 01010111, 11101010, 234, -104, 152
             NULL: 00000000, 00000000, 0  , 234 , 234
(103)   PLEASE ,11 <- #5
        DO ,11SUB#1 <- #222
        DO ,11SUB#2 <- #216
        DO ,11SUB#3 <- #200
        PLEASE ,11SUB#4 <- #152
        DO ,11SUB#5 <- #234
        DO READ OUT ,11
        DO ,11 <- #2
        DO ,11SUB#1 <- #176
        PLEASE ,11SUB#2 <- #80
        DO READ OUT ,11
        DO RESUME #1


        DO NOTE === CHECK FOR DRAW
        PLEASE NOTE === .1 Stores the result of the whole draw check (all spots true)
        DO NOTE === A value of 1 indicates a draw
        DO NOTE === #3 indicates a blank space
(60)    DO .1 <- #1
        DO NOTE === Results in 0 if V == #3 and something else otherwise
        DO .9 <- "'?"',10SUB#1'$'#3'"'~'#0$#65535'"~#65535
        DO NOTE === NON-ZERO CHECK
        DO NOTE === .9 Stores 1 if V != #3 (aka the spot is taken and the game can possible be drawn) and 0 otherwise
        PLEASE (51) NEXT
        DO .1 <- "&'.1$.9'"~#1

        DO .9 <- "'?"',10SUB#2'$'#3'"'~'#0$#65535'"~#65535
        PLEASE (51) NEXT
        DO .1 <- "&'.1$.9'"~#1

        DO .9 <- "'?"',10SUB#3'$'#3'"'~'#0$#65535'"~#65535
        PLEASE (51) NEXT
        DO .1 <- "&'.1$.9'"~#1

        DO .9 <- "'?"',10SUB#4'$'#3'"'~'#0$#65535'"~#65535
        PLEASE (51) NEXT
        DO .1 <- "&'.1$.9'"~#1

        DO .9 <- "'?"',10SUB#5'$'#3'"'~'#0$#65535'"~#65535
        PLEASE (51) NEXT
        DO .1 <- "&'.1$.9'"~#1

        DO .9 <- "'?"',10SUB#6'$'#3'"'~'#0$#65535'"~#65535
        PLEASE (51) NEXT
        DO .1 <- "&'.1$.9'"~#1

        DO .9 <- "'?"',10SUB#7'$'#3'"'~'#0$#65535'"~#65535
        PLEASE (51) NEXT
        DO .1 <- "&'.1$.9'"~#1

        DO .9 <- "'?"',10SUB#8'$'#3'"'~'#0$#65535'"~#65535
        PLEASE (51) NEXT
        DO .1 <- "&'.1$.9'"~#1

        DO .9 <- "'?"',10SUB#9'$'#3'"'~'#0$#65535'"~#65535
        PLEASE (51) NEXT
        DO .1 <- "&'.1$.9'"~#1

        DO NOTE === 0 -> 2 AND 1 -> 3
        DO .1 <- '#1$.1'~#3

        PLEASE (61) NEXT
        DO (10) NEXT
        DO (103) NEXT
        DO GIVE UP
(61)    DO (62) NEXT
        PLEASE DO RESUME #2
(62)    DO (63) NEXT
        DO THROW AWAY INVALID BOOLEANS. THIS ERROR SHOULD NEVER APPEAR.
(63)    PLEASE DO RESUME .1
