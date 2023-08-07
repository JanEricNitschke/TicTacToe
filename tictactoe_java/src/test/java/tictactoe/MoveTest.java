package tictactoe;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import tictactoe.game.Move;

public class MoveTest {

    @Test
    @DisplayName("Constructor for Move should work.")
    public void testMove() {
        int moveSpot = 2;
        int moveEndstate = 3;
        Move move = new Move(moveSpot, moveEndstate);
        assertEquals(moveSpot, move.getSpot());
        assertEquals(moveEndstate, move.getEndState());
    }
}
