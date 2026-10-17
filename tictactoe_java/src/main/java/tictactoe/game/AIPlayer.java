package tictactoe.game;

import java.util.ArrayList;
import java.util.Random;

/** Class representing an AI player. */
public final class AIPlayer implements Player {
    /** Marker that the AI plays as. */
    private final char aiMarker;

    /**
     * @return the aiMarker
     */
    public char getAiMarker() {
        return aiMarker;
    }

    /** Enum for storing AIDifficulty. */
    enum AIDiffulty {
        /** Makes random move. */
        EASY,
        /** Takes a win if there is one. */
        MEDIUM,
        /** Wins and blocks. */
        HARD,
        /** Plays perfectly. */
        IMPOSSIBLE
    }

    /** Keeps track of the strength of the AI. */
    private final AIDiffulty aiDifficulty;

    /** Constructor. Sets the marker and the difficulty of the AI opponent.
     * @param marker Marker the AI plays as.
     * @param difficulty Strength of the AI from 1 to 4.
     */
    public AIPlayer(final char marker, final int difficulty) {
        this.aiMarker = marker;
        this.aiDifficulty = AIDiffulty.values()[difficulty - 1];
    }

    Move randomMove(final Board board) {
        ArrayList<Integer> openSpots = board.getOpenSpots();
        return new Move(
                openSpots.get(
                        new Random().nextInt(openSpots.size())),
                0);
    }

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

    Move winMove(final Board board, final Marker marker) {
        Move winningMove = getWinningMove(board, marker);
        if (winningMove == null) {
            return randomMove(board);
        }
        return winningMove;
    }

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

    /** Get a move from the AI. Algorithm is determined by aiDifficulty.
     * @param board Board to make the move on.
     * @param marker Marker to make the move as.
     * @return Move chosen based on the difficulty.
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

    /** Have the AI make a move depending on its difficulty.
     * @param board Game board currently being played on.
     * @param marker Marker that the AI makes a move as.
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
