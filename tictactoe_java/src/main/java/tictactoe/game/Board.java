package tictactoe.game;

import java.util.ArrayList;

/**
 * Class representing the game board.
 * Holds the game board as a 1D array of chars.
 * Also stores the wincoditions as arrays of integer index into the
 * game board.
 */
public class Board {
    /**
     * Array storing the actual board itself.
     */
    private char[] gameBoard;
    /**
     * Dimensions of the game board.
     */
    private final int boardDimension = 3;
    /**
     * Array of all winconditions. A player has to fulfil one to win.
     */
    private int[][] winConditions;

    /**
     * empty board as reference.
     */
    private char[] emptyBoard = { '0', '1', '2', '3', '4', '5', '6', '7', '8' };

    /**
     * @return the winConditions
     */
    public int[][] getWinConditions() {
        return winConditions;
    }

    /**
     * Default win conditions for a normal game.
     */
    // CHECKSTYLE.OFF: MagicNumber
    private int[][] defaultConditions = {
            { 0, 1, 2 }, { 3, 4, 5 }, { 6, 7, 8 }, // Rows
            { 0, 3, 6 }, { 1, 4, 7 }, { 2, 5, 8 }, // Cols
            { 0, 4, 8 }, { 2, 4, 6 }, // Diagonals
    };
    // CHECKSTYLE.ON: MagicNumber

    /**
     * Default constructor.
     */
    public Board() {
        this.gameBoard = emptyBoard;
        this.winConditions = defaultConditions;
    }

    /**
     * Constructor with a custom board.
     * Used for tests.
     *
     * @param board Board state to start the game with.
     */
    public Board(final char[] board) {
        this.gameBoard = board;
        this.winConditions = defaultConditions;
    }

    /**
     * Constructor with custom win conditions.
     *
     * @param conditions Custom win conditions.
     */
    public Board(final int[][] conditions) {
        this.gameBoard = emptyBoard;
        this.winConditions = conditions;
    }

    /**
     * Constructor with custom board state and win conditions.
     *
     * @param board      Custom boars state.
     * @param conditions Custom win conditions.
     */
    public Board(final char[] board, final int[][] conditions) {
        this.gameBoard = board;
        this.winConditions = conditions;
    }

    /**
     * Return the total size of the game board.
     *
     * @return Total size of the game board (number of tiles).
     */
    public int getGameBoardSize() {
        return gameBoard.length;
    }

    /**
     * Pretty print the current state of the game board.
     */
    public void showBoard() {
        String lineSeparator = "---------------";
        System.out.println(lineSeparator);
        for (int row = 0; row < boardDimension; row++) {
            for (int col = 0; col < boardDimension; col++) {
                System.out.print("| "
                        + gameBoard[(row * boardDimension) + col]
                        + " |");
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
    public boolean fixSpot(final int spot, final Marker marker) {
        if (spot < 0 || spot >= (gameBoard.length)) {
            System.out.println("ERROR: Spot has to be in range [0-8]!");
            return false;
        }
        if (gameBoard[spot] == 'O' || gameBoard[spot] == 'X') {
            System.out.println("ERROR: Spot " + spot + " is already occupied!");
            return false;
        }
        gameBoard[spot] = marker.getMarker();
        return true;
    }

    /**
     * Check how much of a win condition (row, col, diagonal)
     * has been fulfilled.
     *
     * @param condition Array of indices from the win condition to check.
     * @param marker    Marker that the condition should be checked for.
     * @return Result of the check that contains the number of filled and empty
     *         spots.
     */
    public ConditionResult checkCondition(final int[] condition,
            final Marker marker) {
        ConditionResult conditionResult = new ConditionResult();
        for (int spot : condition) {
            if (gameBoard[spot] == marker.getMarker()) {
                conditionResult.addToSpotsDone(1);
                // Taken spots have marker or swapMarker(marker)
                // If it is neither it is an empty spot.
                // This check is shorter to write
            } else if (gameBoard[spot] != marker.swappedMarker()) {
                conditionResult.getSpotsOpen().add(spot);
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
    public boolean playerWin(final Marker marker) {
        for (int[] condition : winConditions) {
            ConditionResult conditionResult = checkCondition(condition, marker);
            if (conditionResult.getSpotsDone() == boardDimension) {
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

    /**
     * Reset the spot to its default value and return that.
     *
     * @param spot Spot of the board to reset.
     * @return Default value the board spot was reset to.
     */
    public char clearSpot(final int spot) {
        gameBoard[spot] = Character.forDigit(spot, 10);
        return gameBoard[spot];
    }
}
