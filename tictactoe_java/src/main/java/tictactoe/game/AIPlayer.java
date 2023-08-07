package tictactoe.game;

import java.util.ArrayList;
import java.util.Random;

/**
 * Class representing an AI player.
 */
public class AIPlayer implements Player {
    /**
     * Marker that the AI plays as.
     */
    private final char aiMarker;

    /**
     * @return the aiMarker
     */
    public char getAiMarker() {
        return aiMarker;
    }

    /**
     * Enum for storing AIDifficulty.
     */
    enum AIDiffulty {
        /**
         * Makes random move.
         */
        EASY,
        /**
         * Takes a win if there is one.
         */
        MEDIUM,
        /**
         * Wins and blocks.
         */
        HARD,
        /**
         * Plays perfectly.
         */
        IMPOSSIBLE
    }

    /**
     * Keeps track of the strength of the AI.
     */
    private final AIDiffulty aiDifficulty;

    /**
     * Constructor. Set the marker and the difficulty of the AI opponent.
     *
     * @param marker     The marker that the AI plays as.
     *                   Used to check if it is its turn.
     * @param difficulty The strength of the AI.
     *                   From 1 (weak) to 4 (impossible)
     */
    public AIPlayer(final char marker, final int difficulty) {
        this.aiMarker = marker;
        this.aiDifficulty = AIDiffulty.values()[difficulty - 1];
    }

    /**
     * Make a random valid move on the board.
     *
     * @param board Board to make a move on
     * @return A random valid (in bounds and unoccupied) move on the board.
     */
    Move randomMove(final Board board) {
        ArrayList<Integer> openSpots = board.getOpenSpots();
        return new Move(
                openSpots.get(
                        new Random().nextInt(openSpots.size())),
                0);
    }

    /**
     * Get a winning move on the board for the given player if possible.
     * If there is no winning move return null.
     *
     * @param board  Board to make the move on.
     * @param marker Marker to make the move as.
     * @return A winning move on the board or null if there is none.
     */
    Move getWinningMove(final Board board, final Marker marker) {
        for (int[] condition : board.getWinConditions()) {
            ConditionResult conditionResult = board.checkCondition(
                    condition, marker);
            if (conditionResult.getSpotsDone() == 2
                    && conditionResult.getSpotsOpen().size() == 1) {
                return new Move(
                        conditionResult.getSpotsOpen().getFirst(), 0);
            }
        }
        return null;
    }

    /**
     * Return a winning or random move on the board for the given player.
     *
     * @param board  Board to make the move on.
     * @param marker Marker to make the move as.
     * @return A winning or random move.
     */
    Move winMove(final Board board, final Marker marker) {
        Move winningMove = getWinningMove(board, marker);
        if (winningMove == null) {
            return randomMove(board);
        }
        return winningMove;
    }

    /**
     * Get a winning, blocking or random move on the board for the given player.
     * If possible a winning move is made.
     * If not it is checked if a blocking move is available.
     * If that is also not the case then a random valid move is returned.
     *
     * @param board  Board to make the move on.
     * @param marker Marker to mave the move as.
     * @return A winning, blocking or random move.
     */
    Move blockWinMove(final Board board, final Marker marker) {
        Move winningMove = getWinningMove(board, marker);
        if (winningMove != null) {
            return winningMove;
        }
        Move blockingMove = getWinningMove(board, marker.swapMarker());
        if (blockingMove != null) {
            return blockingMove;
        }
        return randomMove(board);
    }

    /**
     * Get the optimal move for the player on the board via the
     * minmax algorithm.
     *
     * @param board  Board to make the move on.
     * @param marker Marker to mave the move as.
     * @return Optimal move.
     */
    Move minmax(final Board board, final Marker marker) {
        // Base cases if win, loss, tie
        if (board.playerWin(marker)) {
            return new Move(-1, 1);
        }
        if (board.playerWin(marker.swapMarker())) {
            return new Move(-1, -1);
        }
        ArrayList<Integer> openSpots = board.getOpenSpots();
        if (openSpots.isEmpty()) {
            return new Move(-1, 0);
        }
        // Skip full algorithm on empty board
        if (openSpots.size() == board.getGameBoardSize()) {
            return randomMove(board);
        }
        // Actual minmaxalgorithm
        Move bestMove = new Move(-1, -1);
        for (int openSpot : openSpots) {
            board.fixSpot(openSpot, marker);
            Move currentMove = minmax(board, marker.swapMarker());
            if (-currentMove.getEndState() >= bestMove.getEndState()) {
                bestMove = new Move(openSpot, -currentMove.getEndState());
            }
            board.clearSpot(openSpot);
        }
        return bestMove;
    }

    /**
     * Get a move from the AI. Algorithm is determined
     * by aiDifficulty.
     *
     * @param board  Board to make the move on.
     * @param marker Marker to mave the move as.
     * @return AI move.
     */
    public Move getMove(final Board board, final Marker marker) {
        Move bestMove;
        switch (aiDifficulty) {
            case EASY:
                bestMove = randomMove(board);
                break;
            case MEDIUM:
                bestMove = winMove(board, marker);
                break;
            case HARD:
                bestMove = blockWinMove(board, marker);
                break;
            default:
                bestMove = minmax(board, marker);
        }
        return bestMove;
    }

    /**
     * Have the AI make a move depending on its difficulty.
     *
     * @param marker Marker that the AI makes a move as.
     * @param board  Game board currently being played on.
     */
    public void makeMove(final Board board, final Marker marker) {
        System.out.println("AI turn as " + marker.getMarker());
        board.showBoard();
        Move bestMove = getMove(board, marker);
        board.fixSpot(bestMove.getSpot(), marker);
        try {
            final int millisPerSecond = 1000;
            Thread.sleep(millisPerSecond);
        } catch (InterruptedException exception) {
            System.out.println("got interrupted!");
        }
    }
}
