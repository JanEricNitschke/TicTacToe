package tictactoe;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.BeforeEach;

import java.util.Scanner;

public class TicTacToeTest {

    TicTacToe tictactoe;

    @BeforeEach
    void setup() {
        tictactoe = new TicTacToe();
    }

    @Test
    public void testGetUserYesNo() {
        Scanner scanner = new Scanner("a\nN");
        assertFalse(tictactoe.getUserYesNo("Test question", scanner));

        scanner = new Scanner("1 \n z \n y");
        assertTrue(tictactoe.getUserYesNo("Test question", scanner));
    }
}
