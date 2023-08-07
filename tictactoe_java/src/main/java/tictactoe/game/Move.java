package tictactoe.game;

/**
 * Class representing a move in a game of tictactoe.
 * Has a spot on the board where it is made and an expected endstate.
 */
public class Move {
    /**
     * Spot this move it so be made on.
     */
    private int spot;
    /**
     * @return the spot
     */
    public int getSpot() {
        return spot;
    }

    /**
     * @param thisSpot the spot to set
     */
    public void setSpot(final int thisSpot) {
        this.spot = thisSpot;
    }

    /**
     * @return the endState
     */
    public int getEndState() {
        return endState;
    }

    /**
     * @param thisEndState the endState to set
     */
    public void setEndState(final int thisEndState) {
        this.endState = thisEndState;
    }

    /**
     * Expected end state of the game after making the move.
     */
    private int endState;

    /**
     * Constructor.
     * @param thisSpot Spot to make the move on.
     * @param thisEndState Expected end state.
     */
    public Move(final int thisSpot, final int thisEndState) {
        this.spot = thisSpot;
        this.endState = thisEndState;
    }
}
