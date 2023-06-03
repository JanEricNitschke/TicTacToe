package tictactoe_java;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;

import java.util.Scanner;

public class TicTacToeJavaTest {

    TicTacToeJava tictactoe;

    @BeforeEach
    void setup() {
        tictactoe = new TicTacToeJava();
    }

    @Test
    @DisplayName("Reading user input should work.")
    public void testGetUserYesNo() {
        Scanner scanner = new Scanner("a\nN");
        assertFalse(tictactoe.getUserYesNo("Test question", scanner));

        scanner = new Scanner("1 \n z \n y");
        assertTrue(tictactoe.getUserYesNo("Test question", scanner));
    }
}
