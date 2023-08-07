package tictactoe;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.util.Scanner;

import tictactoe.game.HumanPlayer;
import tictactoe.game.Marker;
import tictactoe.game.Board;

public class HumanPlayerTest {

    @Test
    @DisplayName("Human MakeMove should loop until valid input.")
    public void testMakeMove() {
        Board board = new Board();
        Marker marker = new Marker();

        Scanner scanner = new Scanner("a\n-1\n16\n3");
        HumanPlayer humanPlayer = new HumanPlayer(scanner);
        humanPlayer.makeMove(board, marker);
        assertFalse(scanner.hasNext());

        scanner = new Scanner("a\n3\n1\n2");
        humanPlayer = new HumanPlayer(scanner);
        humanPlayer.makeMove(board, marker);
        assertTrue(scanner.hasNext());
        assertEquals(2, scanner.nextInt());
    }
}
