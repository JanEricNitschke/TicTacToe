package tictactoe_java.game;

import java.util.ArrayList;

/**
 * Class representing the game board.
 * Holds the game board as a 1D array of chars.
 * Also stores the wincoditions as arrays of integer index into the
 * game board.
 */
public class Board {
    char[] gameBoard;
    int[][] winConditions;

    char[] emptyBoard = { '0', '1', '2', '3', '4', '5', '6', '7', '8' };
    int[][] defaultConditions = { { 0, 1, 2 }, { 3, 4, 5 }, { 6, 7, 8 }, // Rows
            { 0, 3, 6 }, { 1, 4, 7 }, { 2, 5, 8 }, /// Cols
            { 0, 4, 8 }, { 2, 4, 6 } }; // Diagonals

    public Board() {
        gameBoard = emptyBoard;
        winConditions = defaultConditions;
    }

    public Board(char[] board) {
        gameBoard = board;
        winConditions = defaultConditions;
    }

    public Board(int[][] conditions) {
        gameBoard = emptyBoard;
        winConditions = conditions;
    }

    public Board(char[] board, int[][] conditions) {
        gameBoard = board;
        winConditions = conditions;
    }

    /**
     * Pretty print the current state of the game board.
     */
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

    /**
     * Try to fix the given spot for the given marker.
     * If the input is valid fix the spot and return true.
     * If not give an error message and return false.
     *
     * @param spot   Index into the board of the spot to fix.
     * @param marker Marker to fix the spot for.
     * @return Whether the input was valid and the spot was fixed.
     */
    public boolean fixSpot(int spot, Marker marker) {
        if (spot < 0 || spot > 8) {
            System.out.println("ERROR: Spot has to be in range [0-8]!");
            return false;
        }
        if (gameBoard[spot] == 'O' || gameBoard[spot] == 'X') {
            System.out.println("ERROR: Spot " + spot + " is already occupied!");
            return false;
        }
        gameBoard[spot] = marker.marker;
        return true;
    }

    /**
     * Check how much of a win condition (row, col, diagonal) has been fulfilled.
     *
     * @param condition Array of indices representing the win condition to check.
     * @param marker    Marker that the condition should be checked for.
     * @return Result of the check that contains the number of filled and empty
     *         spots.
     */
    public ConditionResult checkCondition(int[] condition, Marker marker) {
        ConditionResult conditionResult = new ConditionResult();
        for (int spot : condition) {
            if (gameBoard[spot] == marker.marker) {
                conditionResult.spotsDone += 1;
            }
            // Taken spots have marker or swapMarker(marker)
            // If it is neither it is an empty spot.
            // This check is shorter to write
            else if (gameBoard[spot] != marker.swappedMarker()) {
                conditionResult.spotsOpen.add(spot);
            }
        }
        return conditionResult;
    }

    /**
     * Check if the player has won the game.
     *
     * @param marker Marker to check the win for.
     * @return Whether or not the given player has won the game.
     */
    public boolean playerWin(Marker marker) {
        for (int[] condition : winConditions) {
            ConditionResult conditionResult = checkCondition(condition, marker);
            if (conditionResult.spotsDone == 3) {
                return true;
            }
        }
        return false;
    }

    /**
     * Checks if the board is full.
     * Indicates a draw if checked after player win.
     *
     * @return Whether the board is full.
     */
    public boolean boardFull() {
        for (char spotMarker : gameBoard) {
            if (spotMarker != 'O' && spotMarker != 'X') {
                return false;
            }
        }
        return true;
    }

    /**
     * Get a ArrayList of still open spots.
     *
     * @return ArrayList of open spots.
     */
    public ArrayList<Integer> getOpenSpots() {
        ArrayList<Integer> openSpots = new ArrayList<Integer>();
        for (int i = 0; i < gameBoard.length; i++) {
            if (gameBoard[i] != 'O' && gameBoard[i] != 'X') {
                openSpots.add(i);
            }
        }
        return openSpots;
    }
}
