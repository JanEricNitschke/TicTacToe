package tictactoe.game;

/** Interface for human and ai moves. */
public interface Player {
    /** Make a move on the board with a marker.
     * @param board Board to make the move on.
     * @param marker Marker to make the move as.
     */
    void makeMove(Board board, Marker marker);
}
