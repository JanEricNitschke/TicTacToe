package tictactoe_java.game;

import java.util.ArrayList;
import java.util.Random;

/**
 * Class representing a human player.
 */
public class AIPlayer implements Player {
    public char aiMarker;
    int aiDifficulty;

    /**
     * Constructor. Set the scanner for taking user input.
     *
     * @param scanner Scanner for getting user input.
     */
    public AIPlayer(char marker, int difficulty) {
        aiMarker = marker;
        aiDifficulty = difficulty;
    }

    public int randomMove(Board board) {
        ArrayList<Integer> openSpots = board.getOpenSpots();
        return openSpots.get(new Random().nextInt(openSpots.size()));
    }

    /**
     * Have the AI make a move depending on its difficulty.
     *
     * @param marker Marker that the AI makes a move as.
     * @param board  Game board currently being played on.
     */
    public void makeMove(Marker marker, Board board) {
        System.out.println("AI turn as " + marker.marker);
        board.showBoard();
        int bestMove;
        switch (aiDifficulty) {
            case 1:
                bestMove = randomMove(board);
            case 2:
                bestMove = randomMove(board);
            case 3:
                bestMove = randomMove(board);
            default:
                bestMove = randomMove(board);
        }
        board.fixSpot(bestMove, marker);
        try {
            Thread.sleep(1000);
        } catch (InterruptedException exception) {
            System.out.println("got interrupted!");
        }
    }
}
