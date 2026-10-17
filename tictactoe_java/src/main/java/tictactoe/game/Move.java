package tictactoe.game;

/** A move in a game of tictactoe: spot on the board and expected end state. */
public class Move {
    /** Spot on the board where the move is made. */
    private int spot;

    /** Expected end state of the game after making the move. */
    private int endState;

    /** Constructor for a move on thisSpot with thisEndState. */
    public Move(final int thisSpot, final int thisEndState) {
        this.spot = thisSpot;
        this.endState = thisEndState;
    }

    /** Returns the spot. */
    public int getSpot() {
        return spot;
    }

    /** Returns the endState. */
    public int getEndState() {
        return endState;
    }
}
