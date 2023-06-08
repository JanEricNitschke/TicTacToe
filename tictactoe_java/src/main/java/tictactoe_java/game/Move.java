package tictactoe_java.game;

/**
 * Class representing a move in a game of tictactoe.
 * Has a spot on the board where it is made and an expected endstate.
 */
public class Move {
    public int spot;
    public int endState;

    public Move(int spot, int endState) {
        this.spot = spot;
        this.endState = endState;
    }
}
