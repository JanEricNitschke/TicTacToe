package tictactoe_java.game;

import static tictactoe_java.TicTacToeJava.swapMarker;

public class Board {
    char[] gameBoard = { '0', '1', '2', '3', '4', '5', '6', '7', '8' };
    int[][] winConditions = { { 0, 1, 2 }, { 3, 4, 5 }, { 6, 7, 8 }, // Rows
            { 0, 3, 6 }, { 1, 4, 7 }, { 2, 5, 8 }, /// Cols
            { 0, 4, 8 }, { 2, 4, 6 } }; // Diagonals

    public void showBoard() {
        String lineSeparator = "---------------";
        System.out.println(lineSeparator);
        for (int row = 0; row < 3; row++) {
            for (int col = 0; col < 3; col++) {
                System.out.print("| " + gameBoard[(row * 3) + col] + " |");
            }
            System.out.println("\n" + lineSeparator);
        }
    }

    public boolean fixSpot(int spot, char marker) {
        if (spot < 0 || spot > 8) {
            System.out.println("ERROR: Spot has to be in range [0-8]!");
            return false;
        }
        if (gameBoard[spot] == 'O' || gameBoard[spot] == 'X' ) {
            System.out.println("ERROR: Spot " + spot + " is already occupied!");
            return false;
        }
        gameBoard[spot] = marker;
        return true;
    }

    public ConditionResult checkCondition(int[] condition, char marker) {
        ConditionResult conditionResult = new ConditionResult();
        for (int spot: condition) {
            if (gameBoard[spot] == marker) {
                conditionResult.spotsDone += 1;
            }
            // Taken spots have marker or swapMarker(marker)
            // If it is neither it is an empty spot.
            // This check is shorter to write
            else if (gameBoard[spot] != swapMarker(marker)) {
                conditionResult.spotsOpen.add(spot);
            }
        }
        return conditionResult;
    }


    public boolean playerWin(char marker) {
        for (int[] condition : winConditions ) {
            ConditionResult conditionResult = checkCondition(condition, marker);
            if (conditionResult.spotsDone == 3) {
                return true;
            }
        }
        return false;
    }

    public boolean boardFull() {
        for (char spotMarker: gameBoard) {
            if (spotMarker != 'O' && spotMarker != 'X') {
                return false;
            }
        }
        return true;
    }
}
