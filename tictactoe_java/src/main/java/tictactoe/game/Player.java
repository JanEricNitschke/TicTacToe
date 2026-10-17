package tictactoe.game;

/** Interface for human and ai moves. */
public interface Player {
    /** Function to make a move on a board with a marker. */
    void makeMove(Board board, Marker marker);
}
