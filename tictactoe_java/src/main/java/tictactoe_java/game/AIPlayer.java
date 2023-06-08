package tictactoe_java.game;

import java.util.ArrayList;
import java.util.Random;

/**
 * Class representing an AI player.
 */
public class AIPlayer implements Player {
    public char aiMarker;
    int aiDifficulty;

    /**
     * Constructor. Set the marker and the difficulty of the AI opponent.
     *
     * @param marker     The marker that the AI plays as. Used to check if it is its
     *                   turn.
     * @param difficulty The strength of the AI. From 1 (weak) to 4 (impossible)
     */
    public AIPlayer(char aiMarker, int aiDifficulty) {
        this.aiMarker = aiMarker;
        this.aiDifficulty = aiDifficulty;
    }

    /**
     * Make a random valid move on the board.
     *
     * @param board Board to make a move on
     * @return A random valid (in bounds and unoccupied) move on the board.
     */
    Move randomMove(Board board) {
        ArrayList<Integer> openSpots = board.getOpenSpots();
        return new Move(openSpots.get(new Random().nextInt(openSpots.size())), 0);
    }

    /**
     * Get a winning move on the board for the given player if possible.
     * If there is no winning move return null.
     *
     * @param board  Board to make the move on.
     * @param marker Marker to make the move as.
     * @return A winning move on the board or null if there is none.
     */
    Move getWinningMove(Board board, Marker marker) {
        for (int[] condition : board.winConditions) {
            ConditionResult conditionResult = board.checkCondition(condition, marker);
            if (conditionResult.spotsDone == 2 && conditionResult.spotsOpen.size() == 1) {
                return new Move(conditionResult.spotsOpen.getFirst(), 0);
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
    Move winMove(Board board, Marker marker) {
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
    Move blockWinMove(Board board, Marker marker) {
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

    Move minmax(Board board, Marker marker) {
        // Base cases if win, loss, tie
        if (board.playerWin(marker)) {
            return new Move(-1, 1);
        }
        if (board.playerWin(marker.swapMarker())) {
            return new Move(-1, -1);
        }
        ArrayList<Integer> openSpots = board.getOpenSpots();
        if (openSpots.size() == 0) {
            return new Move(-1, 0);
        }
        // Skip full algorithm on empty board
        if (openSpots.size() == 9) {
            return randomMove(board);
        }
        // Actual minmaxalgorithm
        Move bestMove = new Move(-1, -1);
        for (int openSpot : openSpots) {
            board.fixSpot(openSpot, marker);
            Move currentMove = minmax(board, marker.swapMarker());
            if (-currentMove.endState >= bestMove.endState) {
                bestMove = new Move(openSpot, -currentMove.endState);
            }
            board.clearSpot(openSpot);
        }
        return bestMove;
    }

    public Move getMove(Board board, Marker marker) {
        Move bestMove;
        switch (aiDifficulty) {
            case 1:
                bestMove = randomMove(board);
                break;
            case 2:
                bestMove = winMove(board, marker);
                break;
            case 3:
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
    public void makeMove(Board board, Marker marker) {
        System.out.println("AI turn as " + marker.marker);
        board.showBoard();
        Move bestMove = getMove(board, marker);
        board.fixSpot(bestMove.spot, marker);
        try {
            Thread.sleep(1000);
        } catch (InterruptedException exception) {
            System.out.println("got interrupted!");
        }
    }
}
