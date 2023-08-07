package tictactoe.game;

/**
 * Interface for human and ai moves.
 */
public interface Player {
    /**
     * Function to make a move on a board with a marker.
     * @param board Game board to make the move on.
     * @param marker Marker to make the moave as.
     */
    void makeMove(Board board, Marker marker);
}
